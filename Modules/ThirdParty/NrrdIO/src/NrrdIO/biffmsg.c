/*
  NrrdIO: C library for NRRD file IO (with optional compressions)
  Copyright (C) 2009--2026  University of Chicago
  Copyright (C) 2005--2008  Gordon Kindlmann
  Copyright (C) 1998--2004  University of Utah

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any
  damages arising from the use of this software.

  Permission is granted to anyone to use this software for any
  purpose, including commercial applications, and to alter it and
  redistribute it freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must
     not claim that you wrote the original software. If you use this
     software in a product, an acknowledgment in the product
     documentation would be appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must
     not be misrepresented as being the original software.

  3. This notice may not be removed or altered from any source distribution.
*/

#include "NrrdIO.h"
#include "privateBiff.h"

/*
With the Nov'09 re-write of biff, this sourcefile becomes the only place where a static
buffer is used for message handling; this should eventually be avoided by using things
like asprintf and vasprintf which allocated the string as needed.  However (writing here
in 2025), vasprintf is not apparently part of any C standard (as per
https://en.cppreference.com/w/c/experimental/dynamic/asprintf), so we make do with
snprintf and vsnprintf, used with MY_HACK_STRLEN.  Note that we are NOT merely hoping
that incoming error messages are shorter than MY_HACK_STRLEN. If they are longer, they
will be truncated safely to avoid any buffer overflow.
*/
#define MY_HACK_STRLEN AIR_STRLEN_HUGE
#define MY_MSG_INCR    2

/*
 * biff__MsgNoop
 *
 * pass this instead of a real biffMsg (allocated by biffMsgNew) as a
 * flag to say, "don't bother, really".  This turns all the biffMsg
 * functions into no-ops (except that var-args are still consumed
 * where they are used)
 */
static const biffMsg msgNoopStruct = {NULL, NULL, 0, NULL};
const biffMsg *const biff__MsgNoop = &msgNoopStruct;

biffMsg *
biff__MsgNew(const char *key) {
  static const char me[] = "biff__MsgNew";
  biffMsg *msg;

  if (!key) {
    fprintf(stderr, "%s: PANIC got NULL key\n", me);
    return NULL; /* exit(1); */
  }
  msg = AIR_CALLOC(1, biffMsg);
  if (msg) {
    airPtrPtrUnion appu;

    msg->key = airStrdup(key);
    msg->err = NULL;
    msg->errNum = 0;
    appu.cp = &(msg->err);
    msg->errArr = airArrayNew(appu.v, &(msg->errNum), sizeof(char *), MY_MSG_INCR);
    if (msg->errArr) {
      airArrayPointerCB(msg->errArr, NULL, airFree);
    }
  }
  if (!(msg && msg->key && msg->errArr)) {
    fprintf(stderr, "%s: PANIC couldn't calloc new msg\n", me);
    return NULL; /* exit(1); */
  }
  return msg;
}

biffMsg *
biff__MsgNix(biffMsg *msg) {

  if (msg && msg != biff__MsgNoop) {
    airFree(msg->key);
    airArrayLenSet(msg->errArr, 0); /* frees all msg->err[i] */
    airArrayNuke(msg->errArr);
    airFree(msg);
  }
  return NULL;
}

/*
 * adds a given message to the given entry.  The message is processed to
 * convert all whitespace into ' ', and to eliminate whitespace at the
 * end of the message.
 */
void
biff__MsgAdd(biffMsg *msg, const char *err) {
  static const char me[] = "biff__MsgAdd";
  uint idx;

  if (biff__MsgNoop == msg) {
    return;
  }
  if (!(msg && err)) {
    fprintf(stderr, "%s: PANIC got NULL msg (%p) or err (%p)\n", me, AIR_VOIDP(msg),
            AIR_CVOIDP(err));
    return; /* exit(1); */
  }
  idx = airArrayLenIncr(msg->errArr, 1);
  if (!msg->err) {
    fprintf(stderr, "%s: PANIC: couldn't add message to %s\n", me, msg->key);
    return; /* exit(1); */
  }
  /* new for TeemV2: run string through airMultiLinify instead of airOneLinify */
  if (!(msg->err[idx] = airMultiLinify(airStrdup(err)))) {
    /* if (!(msg->err[idx] = airStrdup(err))) { */
    fprintf(stderr, "%s: PANIC: couldn't alloc message to %s\n", me, msg->key);
    return; /* exit(1); */
  }
  return;
}

void
biff__MsgAddVL(biffMsg *msg, const char *errfmt, va_list args) {
  char errstr[MY_HACK_STRLEN + 1];

  /* h/t Jorik Blaas for highlighting need for vsnprintf over vsprintf */
  vsnprintf(errstr, MY_HACK_STRLEN + 1, errfmt, args);
  biff__MsgAdd(msg, errstr);
  return;
}

static void
msgClear(biffMsg *msg) {

  if (biff__MsgNoop == msg) {
    return;
  }
  airArrayLenSet(msg->errArr, 0); /* frees all msg->err[i] */
  /* but msg->key stays allocated */
  return;
}

/*
** max length of line formatted "[<key>] <err>\n"
*/
static size_t
msgLineLenMax(const biffMsg *msg) {
  uint ii;
  size_t len, maxlen;

  if (biff__MsgNoop == msg) {
    return 0;
  }
  maxlen = 0;
  for (ii = 0; ii < msg->errNum; ii++) {
    len = strlen(msg->err[ii]) + strlen(msg->key) + strlen("[] \n");
    maxlen = AIR_MAX(maxlen, len);
  }
  return maxlen;
}

/*
 * biff__MsgMove
 *
 * "src" is not const because we clear it after moving things out
 */
void
biff__MsgMove(biffMsg *dest, biffMsg *src, const char *err) {
  static const char me[] = "biff__MsgMove";
  uint ii;
  size_t buffSize;
  char *buff;

  if (biff__MsgNoop == dest || biff__MsgNoop == src) {
    return;
  }
  if (!(dest && src)) {
    fprintf(stderr, "%s: PANIC got NULL msg (%p %p)\n", me, AIR_VOIDP(dest),
            AIR_VOIDP(src));
    return; /* exit(1); */
  }
  /* if src and dest are same, this degenerates to biff__MsgAdd */
  if (dest == src && airStrlen(err)) {
    biff__MsgAdd(dest, err);
    return;
  }

  buffSize = msgLineLenMax(src) + 1;
  buff = AIR_CALLOC(buffSize, char);
  if (!buff) {
    fprintf(stderr, "%s: PANIC: can't allocate buffer\n", me);
    return; /* exit(1); */
  }
  for (ii = 0; ii < src->errNum; ii++) {
    snprintf(buff, buffSize, "[%s] %s", src->key, src->err[ii]);
    biff__MsgAdd(dest, buff);
  }
  free(buff);
  msgClear(src);
  if (airStrlen(err)) {
    biff__MsgAdd(dest, err);
  }
  return;
}

void
biff__MsgMoveVL(biffMsg *dest, biffMsg *src, const char *errfmt, va_list args) {
  char errstr[MY_HACK_STRLEN + 1];

  vsnprintf(errstr, MY_HACK_STRLEN + 1, errfmt, args);
  biff__MsgMove(dest, src, errstr);
  return;
}

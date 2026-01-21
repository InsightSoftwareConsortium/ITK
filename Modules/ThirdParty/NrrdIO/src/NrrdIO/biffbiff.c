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
** Until Teem has its own printf implementation, this will have to do;
** it is imperfect because these are not functionally identical.
*/
#if defined(WIN32) || defined(_WIN32)
#  define snprintf _snprintf
#endif

static biffMsg **biff__msg = NULL;    /* master array of biffMsg pointers */
static uint biff__msgNum = 0;         /* length of biff__Err == # keys maintained */
static airArray *biff__msgArr = NULL; /* air array of biff__Err and biff__Num */

#define MY_INCR 2

typedef union {
  biffMsg ***b;
  void **v;
} biff__pu;

/*
 * msgStart()
 *
 * allocates data structers needed by biff.  Panics if anything goes wrong.
 *
 * NOTE: Can be harmlessly called multiple times.
 */
static void
msgStart(void) {
  static const char me[] = "[biff] msgStart";
  biff__pu uu;

  if (biff__msgArr) {
    /* its non-NULL, must have been called already */
    return;
  }
  uu.b = &biff__msg;
  biff__msgArr = airArrayNew(uu.v, &biff__msgNum, sizeof(biffMsg *), MY_INCR);
  if (!biff__msgArr) {
    fprintf(stderr, "%s: PANIC: couldn't allocate internal data\n", me);
    /* exit(1); */
  }
  return;
}

static void
msgFinish(void) {

  if (biff__msgArr) {
    /* setting biff__msgArr to NULL is needed to put biff back in initial state
       so that next calls to biff re-trigger msgStart() */
    biff__msgArr = airArrayNuke(biff__msgArr);
  }
  return;
}

/*
 * msgFind()
 *
 * returns the biffMsg (in biff__msg) of the entry with the given `key`,
 * or NULL if it was not found
 */
static biffMsg *
msgFind(const char *key) {
  static const char me[] = "[biff] msgFind";
  biffMsg *msg;
  uint ii;

  if (!key) {
    fprintf(stderr, "%s: PANIC got NULL key", me);
    return NULL; /* exit(1); */
  }
  msg = NULL;
  if (biff__msgNum) {
    for (ii = 0; ii < biff__msgNum; ii++) {
      if (!strcmp(biff__msg[ii]->key, key)) {
        msg = biff__msg[ii];
        break;
      }
    }
  }
  return msg;
}

/*
 * assumes that msg really is in biff__msg[]
 */
static uint
msgFindIdx(biffMsg *msg) {
  uint ii;

  for (ii = 0; ii < biff__msgNum; ii++) {
    if (msg == biff__msg[ii]) {
      break;
    }
  }
  return ii;
}

/*
 * msgAdd()
 *
 * If given `key` already has a biffMsg in `biff__msg`, returns that.
 * otherwise, adds a new biffMsg for given `key` to `biff__msg`, and returns it.
 * Panics if there is a problem
 */
static biffMsg *
msgAdd(const char *key) {
  static const char me[] = "[biff] msgAdd";
  uint ii;
  biffMsg *msg;

  msg = NULL;
  /* find if key exists already */
  for (ii = 0; ii < biff__msgNum; ii++) {
    if (!strcmp(key, biff__msg[ii]->key)) {
      msg = biff__msg[ii];
      break;
    }
  }
  if (!msg) {
    /* have to add new biffMsg */
    ii = airArrayLenIncr(biff__msgArr, 1);
    if (!biff__msg) {
      fprintf(stderr, "%s: PANIC: couldn't accommodate one more key\n", me);
      return NULL; /* exit(1); */
    }
    msg = biff__msg[ii] = biff__MsgNew(key);
  }
  return msg;
}

/*
 ******** biffAdd()
 *
 * Adds string `err` at key `key`, whether or not there are any existing messages there.
 * Since biffSet() was killed Wed Apr 20 11:11:51 EDT 2005, this has become the main biff
 * function (but later the more useful biffAddf really became the main biff function)
 */
void
biffAdd(const char *key, const char *err) {
  biffMsg *msg;

  msgStart();
  msg = msgAdd(key);
  biff__MsgAdd(msg, err);
  return;
}

static void
bAddVL(const char *key, const char *errfmt, va_list args) {
  biffMsg *msg;

  msgStart();
  msg = msgAdd(key);
  biff__MsgAddVL(msg, errfmt, args);
  return;
}

/*
 ******** biffAddf()
 *
 * Adds string `err` at key `key`, whether or not there are any existing messages there.
 * This version accepts a printf style format string as input.
 */
void
biffAddf(const char *key, const char *errfmt, ...) {
  va_list args;

  va_start(args, errfmt);
  bAddVL(key, errfmt, args);
  va_end(args);
  return;
}

/*
 ******** biffMaybeAdd()
 *
 * wrapper around biffAdd() but doesn't actually do anything if !useBiff
 */
void
biffMaybeAdd(const char *key, const char *err, int useBiff) {

  if (useBiff) {
    biffAdd(key, err);
  }
  return;
}

void
biffMaybeAddf(int useBiff, const char *key, const char *errfmt, ...) {
  va_list args;

  va_start(args, errfmt);
  if (useBiff) {
    bAddVL(key, errfmt, args);
  }
  va_end(args);
  return;
}

/*
 ******** biffGet()
 *
 * creates a string which records all the errors at given `key` and returns it.
 * Returns NULL in case of error.  This function should be considered a glorified
 * strdup(): it is the callers responsibility to free() this string later
 */
char *
biffGet(const char *key) {
  static const char me[] = "biffGet";
  char *_ret, *ret;
  size_t retSize;
  biffMsg *msg;
  uint ii;

  msgStart();
  msg = msgFind(key);
  if (!msg) {
    static const char err[] = "[%s] No information for this key!";
    fprintf(stderr, "%s: WARNING: no information for key \"%s\"\n", me, key);
    retSize = strlen(err) + strlen(key) + 1;
    ret = AIR_CALLOC(retSize, char);
    if (!ret) {
      fprintf(stderr, "%s: PANIC: unable to allocate buffer\n", me);
      return NULL; /* exit(1); */
    }
    snprintf(ret, retSize, err, key);
    return ret;
  }
  retSize = 0;
  for (ii = 0; ii < msg->errNum; ii++) {
    retSize += strlen(msg->key) + strlen(msg->err[ii]) + strlen("[] \n");
  }
  retSize += 1; /* for '\0' */
  ret = _ret = AIR_CALLOC(retSize, char);
  if (!ret) {
    fprintf(stderr, "%s: PANIC: unable to allocate buffer\n", me);
    return NULL; /* exit(1); */
  }
  ret[0] = '\0';
  for (ii = msg->errNum; ii > 0; ii--) {
    snprintf(ret, retSize, "[%s] %s\n", msg->key, msg->err[ii - 1]);
    SN_INCR(ret, retSize);
  }
  return _ret;
}

/*
******** biffCheck()
**
** sees how many messages there are for a given key;
*/
unsigned int
biffCheck(const char *key) {
  biffMsg *msg;

  msgStart();
  msg = msgFind(key);
  if (biff__MsgNoop == msg) {
    return 0;
  }
  if (!msg) {
    return 0;
  }
  return msg->errNum;
}

/*
 ******* biffDone()
 *
 * frees everything associated with given `key`, and shrinks list of keys,
 * and calls msgFinish() if there are no keys left
 */
void
biffDone(const char *key) {
  static const char me[] = "biffDone";
  uint idx;
  biffMsg *msg;

  msgStart();

  msg = msgFind(key);
  if (!msg) {
    fprintf(stderr, "%s: WARNING: no information for key \"%s\"\n", me, key);
    return;
  }
  idx = msgFindIdx(msg);
  biff__MsgNix(msg);
  if (biff__msgNum > 1) {
    /* if we have more than one key in action, move the last biffMsg
       to the position that was just cleared up */
    biff__msg[idx] = biff__msg[biff__msgNum - 1];
  }
  airArrayLenIncr(biff__msgArr, -1);
  /* if that was the last key, close shop */
  if (!biff__msgArr->len) {
    msgFinish();
  }

  return;
}

void
biffMove(const char *destKey, const char *err, const char *srcKey) {
  static const char me[] = "biffMove";
  biffMsg *dest, *src;

  msgStart();
  dest = msgAdd(destKey);
  src = msgFind(srcKey);
  if (!src) {
    fprintf(stderr, "%s: WARNING: key \"%s\" unknown\n", me, srcKey);
    return;
  }
  biff__MsgMove(dest, src, err);
  return;
}

static void
moveVL(const char *destKey, const char *srcKey, const char *errfmt, va_list args) {
  static const char me[] = "moveVL";
  biffMsg *dest, *src;

  msgStart();
  dest = msgAdd(destKey);
  src = msgFind(srcKey);
  if (!src) {
    fprintf(stderr, "%s: WARNING: key \"%s\" unknown\n", me, srcKey);
    return;
  }
  biff__MsgMoveVL(dest, src, errfmt, args);
  biffDone(srcKey);
  return;
}

void
biffMovef(const char *destKey, const char *srcKey, const char *errfmt, ...) {
  va_list args;

  va_start(args, errfmt);
  moveVL(destKey, srcKey, errfmt, args);
  va_end(args);
  return;
}

char *
biffGetDone(const char *key) {
  char *ret;

  msgStart();

  ret = biffGet(key);
  biffDone(key); /* will call msgFinish if this is the last key */

  return ret;
}

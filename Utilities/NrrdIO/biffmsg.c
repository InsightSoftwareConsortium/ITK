/*
  Teem: Tools to process and visualize scientific data and images              
  Copyright (C) 2008, 2007, 2006, 2005  Gordon Kindlmann
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998  University of Utah

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  (LGPL) as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  The terms of redistributing and/or modifying this software also
  include exceptions to the LGPL that facilitate static linking.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this library; if not, write to Free Software Foundation, Inc.,
  51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include "biff.h"

/* 
** with the Nov'09 re-write of biff, this sourcefile becomes the only
** place where a static buffer is used for message handling; this
** should eventually be avoided by using things like asprintf and
** vasprintf which allocated the string as needed 
*/
#define _HACK_STRLEN  AIR_STRLEN_HUGE
#define _MSG_INCR 2

biffMsg *
biffMsgNew(const char *key) {
  static const char me[]="biffMsgNew";
  biffMsg *msg;

  if (!key) {
    fprintf(stderr, "%s: PANIC got NULL key\n", me);
    exit(1);
  }
  msg = AIR_CALLOC(1, biffMsg);
  if (msg) {
    msg->key = airStrdup(key);
    msg->err = NULL;
    msg->errNum = 0;
    msg->errArr = airArrayNew(AIR_CAST(void**, &(msg->err)),
                              &(msg->errNum), sizeof(char*), _MSG_INCR);
    if (msg->errArr) {
      airArrayPointerCB(msg->errArr, NULL, airFree);
    }
  }
  if (!( msg && msg->key && msg->errArr )) {
    fprintf(stderr, "%s: PANIC couldn't calloc new msg\n", me);
    exit(1);
  }
  return msg;
}

biffMsg *
biffMsgNix(biffMsg *msg) {

  if (msg && msg != biffMsgNoop) {
    airFree(msg->key);
    airArrayLenSet(msg->errArr, 0); /* frees all msg->err[i] */
    airArrayNuke(msg->errArr);
    airFree(msg);
  }
  return NULL;
}

/*
** adds a given message to the given entry.  The message is processed to
** convert all whitespace into ' ', and to eliminate whitespace at the
** end of the message.
*/
void
biffMsgAdd(biffMsg *msg, const char *err) {
  static const char me[]="biffMsgAdd";
  unsigned int idx;

  if (biffMsgNoop == msg) {
    return;
  }
  if (!( msg && err )) {
    fprintf(stderr, "%s: PANIC got NULL msg (%p) or err (%p)\n", me, msg, err);
    exit(1);
  }
  idx = airArrayLenIncr(msg->errArr, 1);
  if (!msg->err) {
    fprintf(stderr, "%s: PANIC: couldn't add message to %s\n", me, msg->key);
    exit(1);
  }
  if (!( msg->err[idx] = airOneLinify(airStrdup(err)) )) {
    fprintf(stderr, "%s: PANIC: couldn't alloc message to %s\n", me, msg->key);
    exit(1);
  }
  return;
}

void
biffMsgAddVL(biffMsg *msg, const char *errfmt, va_list args) {
  char errstr[_HACK_STRLEN];

  vsprintf(errstr, errfmt, args);
  biffMsgAdd(msg, errstr);
  return;
}

void
biffMsgAddf(biffMsg *msg, const char *errfmt, ...) {
  va_list args;

  va_start(args, errfmt);
  biffMsgAddVL(msg, errfmt, args);
  va_end(args);
  return;
}

void
biffMsgClear(biffMsg *msg) {

  if (biffMsgNoop == msg) {
    return;
  }
  airArrayLenSet(msg->errArr, 0); /* frees all msg->err[i] */
  /* but msg->key stays allocated */
  return;
}

/*
** max length of line formatted "[<key>] <err>\n"
*/
unsigned int
biffMsgLineLenMax(const biffMsg *msg) {
  unsigned int ii, len, maxlen;
  
  if (biffMsgNoop == msg) {
    return 0;
  }
  maxlen = 0;
  for (ii=0; ii<msg->errNum; ii++) {
    len = AIR_CAST(unsigned int, strlen(msg->err[ii]) + strlen(msg->key) + strlen("[] \n"));
    maxlen = AIR_MAX(maxlen, len);
  }
  return maxlen;
}

/*
******** biffMsgMove
**
** "src" is not const because we clear it after moving things out
*/
void
biffMsgMove(biffMsg *dest, biffMsg *src, const char *err) {
  static const char me[]="biffMsgMove";
  unsigned int ii;
  char *buff;

  if (biffMsgNoop == dest || biffMsgNoop == src) {
    return;
  }
  if (!( dest && src )) {
    fprintf(stderr, "%s: PANIC got NULL msg (%p %p)\n", me, dest, src);
    exit(1);
  }
  /* if src and dest are same, this degenerates to biffMsgAdd */
  if (dest == src && airStrlen(err)) {
    biffMsgAdd(dest, err);
    return;
  }
  
  buff = AIR_CALLOC(biffMsgLineLenMax(src)+1, char);
  if (!buff) {
    fprintf(stderr, "%s: PANIC: can't allocate buffer\n", me);
    exit(1);
  }
  for (ii=0; ii<src->errNum; ii++) {
    sprintf(buff, "[%s] %s", src->key, src->err[ii]);
    biffMsgAdd(dest, buff);
  }
  free(buff);
  biffMsgClear(src);
  if (airStrlen(err)) {
    biffMsgAdd(dest, err);
  }
  return;
}

void
biffMsgMoveVL(biffMsg *dest, biffMsg *src,
              const char *errfmt, va_list args) {
  char errstr[_HACK_STRLEN];
  
  vsprintf(errstr, errfmt, args);
  biffMsgMove(dest, src, errstr);
  return;
}

void
biffMsgMovef(biffMsg *dest, biffMsg *src, const char *errfmt, ...) {
  va_list args;
  
  va_start(args, errfmt);
  biffMsgMoveVL(dest, src, errfmt, args);
  va_end(args);
  return;
}

/*
******** biffMsgStrlen
**
** returns length of string (not including null termination, as usual) 
** of the error message that will be generated by biffMsgStrSet
*/
unsigned int
biffMsgStrlen(const biffMsg *msg) {
  static const char me[]="biffMsgStrlen";
  unsigned int ii, len;

  if (biffMsgNoop == msg) {
    return 0;
  }
  if (!( msg )) {
    fprintf(stderr, "%s: PANIC got NULL msg %p\n", me, msg);
    exit(1);
  }

  len = 0;
  for (ii=0; ii<msg->errNum; ii++) {
    len += AIR_CAST(unsigned int, strlen(msg->key) + strlen(msg->err[ii]) + strlen("[] \n"));
  }
  return len+1;
}

char *
biffMsgStrAlloc(const biffMsg *msg) {
  static const char me[]="biffMsgStrAlloc";
  char *ret;
  unsigned int len;

  if (biffMsgNoop == msg) {
    return NULL;
  }
  len = biffMsgStrlen(msg);
  ret = AIR_CALLOC(len+1, char);
  if (!ret) {
    fprintf(stderr, "%s: PANIC couldn't alloc string", me);
    exit(1);
  }
  return ret;
}

/*
** ret is assumed to be allocated for biffMsgStrlen()+1, or is the
** the return from biffMsgStrAlloc
*/
void
biffMsgStrSet(char *ret, const biffMsg *msg) {
  static const char me[]="biffMsgStrSet";
  char *buff;
  unsigned int ii;

  if (biffMsgNoop == msg) {
    return;
  }
  buff = AIR_CALLOC(biffMsgLineLenMax(msg)+1, char);
  if (!buff) {
    fprintf(stderr, "%s: PANIC couldn't alloc buffer", me);
    exit(1);
  }
  strcpy(ret, "");
  for (ii=msg->errNum; ii>0; ii--) {
    sprintf(buff, "[%s] %s\n", msg->key, msg->err[ii-1]);
    strcat(ret, buff);
  }
  free(buff);
}

char *
biffMsgStrGet(const biffMsg *msg) {
  char *ret;

  if (biffMsgNoop == msg) {
    return NULL;
  }
  ret = biffMsgStrAlloc(msg);
  biffMsgStrSet(ret, msg);
  return ret;
}

biffMsg
_biffMsgNoop = {
  NULL,
  NULL,
  0, 
  NULL
};

/*
******** biffMsgNoop
**
** pass this instead of a real biffMsg (allocated by biffMsgNew) as a 
** flag to say, "don't bother, really".  This turns all the biffMsg
** functions into no-ops (except that var-args are still consumed
** where they are used)
*/
biffMsg *
biffMsgNoop = &_biffMsgNoop;

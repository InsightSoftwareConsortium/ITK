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

static biffMsg **
_bmsg=NULL;            /* master array of biffMsg pointers */
static unsigned int 
_bmsgNum=0;            /* length of _biffErr == # keys maintained */
static airArray *
_bmsgArr=NULL;         /* air array of _biffErr and _biffNum */

#define __INCR 2

typedef union {
  biffMsg ***b;
  void **v;
} _beu;

/*
** _bmsgStart()
**
** allocates data structers needed by biff.  Panics and exit(1)s if 
** anything goes wrong.
**
** NOTE: Can be harmlessly called multiple times.
*/
void
_bmsgStart(void) {
  static const char me[]="[biff] _bmsgStart";
  _beu uu;

  if (_bmsgArr) {
    /* its non-NULL, must have been called already */
    return;
  }
  uu.b = &_bmsg;
  _bmsgArr = airArrayNew(uu.v, &_bmsgNum, sizeof(biffMsg*), __INCR);
  if (!_bmsgArr) {
    fprintf(stderr, "%s: PANIC: couldn't allocate internal data\n", me);
    exit(1);
  }
  /* airArrayPointerCB(_bmsgArr, NULL, biffMsgNix); */
  /* HEY: not using any pointer callbacks here? */
  return;
}

void
_bmsgFinish(void) {

  if (_bmsgArr) {
    /* setting _bmsgArr to NULL is needed to put biff back in initial state
       so that next calls to biff re-trigger _bmsgStart() */
    _bmsgArr = airArrayNuke(_bmsgArr);
  }
  return;
}

/*
** _bmsgFind()
**
** returns the biffMsg (in _bmsg) of the entry with the given key, or
** NULL if it was not found
*/
biffMsg *
_bmsgFind(const char *key) {
  static const char me[]="[biff] _bmsgFind";
  biffMsg *msg;
  unsigned int ii;

  if (!key) {
    fprintf(stderr, "%s: PANIC got NULL key", me);
    exit(1);
  }
  msg = NULL;
  if (_bmsgNum) {
    for (ii=0; ii<_bmsgNum; ii++) {
      if (!strcmp(_bmsg[ii]->key, key)) {
        msg = _bmsg[ii];
        break;
      }
    }
  }
  return msg;
}

/*
** assumes that msg really is in _bmsg[]
*/
unsigned int
_bmsgFindIdx(biffMsg *msg) {
  unsigned int ii;
  
  for (ii=0; ii<_bmsgNum; ii++) {
    if (msg == _bmsg[ii]) {
      break;
    }
  }
  return ii;
}

/*
** _bmsgAdd()
**
** if given key already has a biffMsg in _bmsg, returns that.
** otherise, adds a new biffMsg for given key to _bmsg, and returns it
** panics and exit(1)s if there is a problem
*/
biffMsg *
_bmsgAdd(const char *key) {
  static const char me[]="[biff] _bmsgAdd";
  unsigned int ii;
  biffMsg *msg;

  msg = NULL;
  /* find if key exists already */
  for (ii=0; ii<_bmsgNum; ii++) {
    if (!strcmp(key, _bmsg[ii]->key)) {
      msg = _bmsg[ii];
      break;
    }
  }
  if (!msg) {
    /* have to add new biffMsg */
    ii = airArrayLenIncr(_bmsgArr, 1);
    if (!_bmsg) {
      fprintf(stderr, "%s: PANIC: couldn't accomodate one more key\n", me);
      exit(1);
    }
    msg = _bmsg[ii] = biffMsgNew(key);
  }
  return msg;
}

/***********************************************************************/
/***********************************************************************/

/*
******** biffAdd()
**
** Adds string "err" at key "key", whether or not there are any 
** existing messages there.  Since biffSet() was killed 
** Wed Apr 20 11:11:51 EDT 2005, this has become the main biff
** function.
*/
void
biffAdd(const char *key, const char *err) {
  biffMsg *msg;

  _bmsgStart();
  msg = _bmsgAdd(key);
  biffMsgAdd(msg, err);
  return;
}

void
biffAddVL(const char *key, const char *errfmt, va_list args) {
  biffMsg *msg;

  _bmsgStart();
  msg = _bmsgAdd(key);
  biffMsgAddVL(msg, errfmt, args);
  return;
}

/*
******** biffAddf()
**
** Adds string "err" at key "key", whether or not there are any
** existing messages there.  This version accepts a printf style
** format string as input.
*/
void
biffAddf(const char *key, const char *errfmt, ...) {
  va_list args;

  va_start(args, errfmt);
  biffAddVL(key, errfmt, args);
  va_end(args);
  return;
}

#if 0
/*
******** biffAddf_e
**
** calls (eventually) biffMsgAdd if msg is non-NULL, otherwise calls
** biffAdd if msg is NULL.
*/
void
biffAddf_e(biffMsg *msg, const char *key, const char *errfmt, ...) {
  va_list args;

  va_start(args, errfmt);
  if (msg) {
    biffMsgAddVL(msg, errfmt, args);
  } else {
    biffAddVL(key, errfmt, args);
  }
  va_end(args);
  return;
}
#endif

/*
******** biffMaybeAdd()
**
** wrapper around biffAdd() but doesn't actually do anything if !useBiff
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
    biffAddVL(key, errfmt, args);
  }
  va_end(args);
  return;
}

/*
******** biffGet()
**
** creates a string which records all the errors at given key and
** returns it.  Returns NULL in case of error.  This function should
** be considered a glorified strdup(): it is the callers responsibility
** to free() this string later
*/
char *
biffGet(const char *key) {
  static const char me[]="biffGet";
  char *ret;
  biffMsg *msg;

  _bmsgStart();
  msg = _bmsgFind(key);
  if (!msg) {
    static const char err[]="[%s] No information for this key!";
    size_t errlen;
    fprintf(stderr, "%s: WARNING: no information for key \"%s\"\n", me, key);
    errlen = strlen(err)+strlen(key)+1;
    ret = AIR_CALLOC(errlen, char);
    if (!ret) {
      fprintf(stderr, "%s: PANIC: unable to allocate buffer\n", me);
      exit(1);
    }
#if defined(WIN32) || defined(_WIN32)
    _snprintf(ret, errlen, err, key);
#else
    snprintf(ret, errlen, err, key);
#endif
    return ret;
  }

  ret = AIR_CALLOC(biffMsgStrlen(msg)+1, char);
  if (!ret) {
    fprintf(stderr, "%s: PANIC: unable to allocate buffer\n", me);
    exit(1);
  }
  biffMsgStrSet(ret, msg);
  return ret;
}

/*
******** biffGetStrlen()
**
** for when you want to allocate the buffer for the biff string, this is
** how you learn its length
*/
int
biffGetStrlen(const char *key) {
  static const char me[]="biffGetStrlen";
  biffMsg *msg;
  unsigned int len;

  _bmsgStart();
  msg = _bmsgFind(key);
  if (!msg) {
    fprintf(stderr, "%s: WARNING: no information for key \"%s\"\n", me, key);
    return 0;
  }
  len = biffMsgStrlen(msg);
  len += 1;  /* GLK forgets if the convention is that the caller allocates
                for one more to include '\0'; this is safer */
  return len;
}

/*
******** biffSetStr()
**
** for when you want to allocate the buffer for the biff string, this is
** how you get the error message itself
*/
void
biffSetStr(char *str, const char *key) {
  static const char me[]="biffSetStr";
  biffMsg *msg;

  if (!str) {
    fprintf(stderr, "%s: ERROR: got NULL buffer for \"%s\"\n", me, key);
    return;
  }

  _bmsgStart();
  msg = _bmsgFind(key);
  if (!msg) {
    fprintf(stderr, "%s: WARNING: no information for key \"%s\"\n", me, key);
    return;
  }
  biffMsgStrSet(str, msg);

  return;
}

/*
******** biffCheck()
**
** sees how many messages there are for a given key
** returns 0 if the key doesn't exist.
*/
int
biffCheck(const char *key) {
  biffMsg *msg;

  _bmsgStart();
  msg = _bmsgFind(key);
  if (!msg) {
    return 0;
  }
  
  return msg->errNum;
}

/*
******** biffDone()
**
** frees everything associated with given key, and shrinks list of keys,
** and calls _bmsgFinish() if there are no keys left
*/
void
biffDone(const char *key) {
  static const char me[]="biffDone";
  unsigned int idx;
  biffMsg *msg;

  _bmsgStart();
  
  msg = _bmsgFind(key);
  if (!msg) {
    fprintf(stderr, "%s: WARNING: no information for key \"%s\"\n", me, key);
    return;
  }
  idx = _bmsgFindIdx(msg);
  biffMsgNix(msg);
  if (_bmsgNum > 1) {
    /* if we have more than one key in action, move the last biffMsg
       to the position that was just cleared up */
    _bmsg[idx] = _bmsg[_bmsgNum-1];
  }
  airArrayLenIncr(_bmsgArr, -1);
  /* if that was the last key, close shop */
  if (!_bmsgArr->len) {
    _bmsgFinish();
  }

  return;
}

void
biffMove(const char *destKey, const char *err, const char *srcKey) {
  static const char me[]="biffMove";
  biffMsg *dest, *src;

  _bmsgStart();
  dest = _bmsgAdd(destKey);
  src = _bmsgFind(srcKey);
  if (!src) {
    fprintf(stderr, "%s: WARNING: key \"%s\" unknown\n", me, srcKey);
    return;
  }
  biffMsgMove(dest, src, err);
  return;
}

void
biffMoveVL(const char *destKey, const char *srcKey,
           const char *errfmt, va_list args) {
  static const char me[]="biffMovev";
  biffMsg *dest, *src;

  _bmsgStart();
  dest = _bmsgAdd(destKey);
  src = _bmsgFind(srcKey);
  if (!src) {
    fprintf(stderr, "%s: WARNING: key \"%s\" unknown\n", me, srcKey);
    return;
  }
  biffMsgMoveVL(dest, src, errfmt, args);
  return;
}

void
biffMovef(const char *destKey, const char *srcKey,
          const char *errfmt, ...) {
  va_list args;

  va_start(args, errfmt);
  biffMoveVL(destKey, srcKey, errfmt, args);
  va_end(args);
  return;
}

char *
biffGetDone(const char *key) {
  char *ret;

  _bmsgStart();

  ret = biffGet(key);
  biffDone(key);  /* will call _bmsgFinish if this is the last key */

  return ret;
}

void
biffSetStrDone(char *str, const char *key) {

  _bmsgStart();

  biffSetStr(str, key);
  biffDone(key);  /* will call _bmsgFinish if this is the last key */

  return;
}

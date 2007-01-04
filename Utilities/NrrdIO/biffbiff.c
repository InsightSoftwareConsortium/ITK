/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2005  Gordon Kindlmann
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998  University of Utah
 
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

/*
** This is mostly garbage.
** It needs to be re-written.
** I apologize.
*/

/*
** _biffEntry struct
**
** hold information and messages associated with one key
*/
typedef struct {
  char key[BIFF_MAXKEYLEN+1]; /* the key */
  char **err;                 /* array of error strings; the err array itself
                                 is NOT null-terminated */
  unsigned int num;           /* length of "err" == # strings stored */
  airArray *AA;               /* air array for err and num */
} _biffEntry;

_biffEntry **_biffErr=NULL;   /* master array of _biffEntry pointers */
unsigned int _biffNum=0;      /* length of _biffErr == # keys maintained */
int _biffIdx=-1;              /* hack: index of latest key found */
airArray *_biffAA=NULL;       /* air array of _biffErr and _biffNum */

#define _BIFF_INCR 2

typedef union {
  _biffEntry ***b;
  void **v;
} _beu;

/*
** _biffInit()
**
** allocates data structers needed by biff.  Panics and exit(1)s if 
** anything goes wrong.  Can be harmlessly called multiple times.
*/
void
_biffInit(void) {
  char me[]="_biffInit";
  _beu uu;

  uu.b = &_biffErr;
  if (!_biffAA) {
    _biffAA = airArrayNew(uu.v, &_biffNum, sizeof(_biffEntry*), _BIFF_INCR);
    if (!_biffAA) {
      fprintf(stderr, "%s: PANIC: couldn't allocate internal data\n", me);
      exit(1);
    }
  }
  return;
}

void
_biffNuke(void) {

  if (_biffAA) {
    /* setting to NULL is needed to put biff back in initial state
       so that next calls to biff re-trigger _biffInit() */
    _biffAA = airArrayNuke(_biffAA);
  }
  return;
}

/*
** _biffCheckKey()
**
** makes sure given key is kosher.  Panics and exit(1)s if given a NULL key
** or if key is too long
*/
void
_biffCheckKey(const char *key) {
  char me[] = "_biffCheckKey";

  if (!key) {
    fprintf(stderr, "%s: PANIC: given NULL key\n", me);
    exit(1);
  }
  if (strlen(key) > BIFF_MAXKEYLEN) {
    fprintf(stderr, "%s: PANIC: key \"%s\" exceeds %d chars\n",
            me, key, BIFF_MAXKEYLEN);
    exit(1);
  }
  return;
}

/*
** _biffFindKey()
**
** returns a pointer to the entry which contains the given key, or
** NULL if it was not found
*/
_biffEntry *
_biffFindKey(const char *key) {
  int ii=-1;
  _biffEntry *ent;

  if (_biffNum) {
    for (ii=0; ii<(int)_biffNum; ii++) {
      /* printf("HEY: comparing key[%d]=\"%s\" to \"%s\"\n", 
         ii, _biffErr[i]->key, key); */
      if (!strcmp(_biffErr[ii]->key, key)) {
        break;
      }
    }
    if (ii == (int)_biffNum) {
      ii = -1;
    }
  }
  /* printf("HEY: index(\"%s\") = %d\n", key, ii); */
  if (-1 == ii) {
    ent = NULL;
    _biffIdx = -1;
  }
  else {
    ent = _biffErr[ii];
    _biffIdx = ii;
  }
  return ent;
}

/*
** _biffNewEntry()
**
** creates and initializes one new _biffEntry, returning a pointer to it
** panics and exit(1)s if there is a problem.
*/
_biffEntry *
_biffNewEntry(const char *key) {
  char me[]="_biffInitEntry";
  _biffEntry *ent;

  ent = (_biffEntry *)calloc(1, sizeof(_biffEntry));
  if (!ent) {
    fprintf(stderr, "%s: couldn't make entry for new key \"%s\"\n", me, key);
    exit(1);
  }
  strcpy(ent->key, key);
  ent->AA = airArrayNew((void**)&(ent->err),
                        &(ent->num), sizeof(char*), _BIFF_INCR);
  if (!ent->AA) {
    fprintf(stderr, "%s: couldn't make array for new key \"%s\"\n", me, key);
    exit(1);
  }
  airArrayPointerCB(ent->AA, NULL, airFree);
  return ent;
}

/*
** _biffNukeEntry()
**
** deletes given entry, and all info contained therein
*/
void
_biffNukeEntry(_biffEntry *ent) {

  if (ent) {
    airArrayLenSet(ent->AA, 0);
    airArrayNuke(ent->AA);
    free(ent);
  }
  return;
}

/*
** _biffAddKey()
**
** adds a key to _biffErr, and returns a pointer to the new entry
** assumes that given key does NOT appear in current list.
** panics and exit(1)s if there is a problem
*/
_biffEntry *
_biffAddKey(const char *key) {
  char me[]="_biffAddKey";
  int ii, newIdx;
  _biffEntry *ent;

  /* find index of new key */
  for (ii=0; ii<(int)_biffNum; ii++) {
    if (strcmp(key, _biffErr[ii]->key) < 0) {
      /* we've hit the one which comes after the new key */
      break;
    }
  }
  /* if the for loop was never broken, _biffNum is the correct new index */
  newIdx = ii;
  /* printf("HEY: index(new key \"%s\") = %d\n", key, ii); */

  airArrayLenIncr(_biffAA, 1);
  if (!_biffAA->data) {
    fprintf(stderr, "%s: PANIC: couldn't accomodate one more key\n", me);
    exit(1);
  }

  /* _biffNum is now one bigger */
  for (ii=_biffNum-2; ii>=newIdx; ii--) {
    _biffErr[ii+1] = _biffErr[ii];
  }
  ent = _biffErr[newIdx] = _biffNewEntry(key);

  return ent;
}

/*
** _biffAddErr()
**
** adds a given message to the given entry.  The message is processed to
** convert all whitespace into ' ', and to eliminate whitespace at the
** end of the message.
** panics and exit(1)s if there is a problem
*/
void
_biffAddErr(_biffEntry *e, const char *err) {
  char *buf, me[]="_biffAddErr";
  int ii; 
  size_t len;

  /* printf("%s: HEY(before): err[%s]->num = %d\n", me, e->key, e->num); */
  airArrayLenIncr(e->AA, 1);
  if (!e->AA->data) {
    fprintf(stderr, "%s: PANIC: couldn't add message for key %s\n",
            me, e->key);
    exit(1);
  }
  /* printf("%s: HEY(after): err[%s]->num = %d\n", me, e->key, e->num); */
  buf = airStrdup(err);
  len = strlen(buf);
  for (ii=0; ii<=len-1; ii++) {
    if (isspace(buf[ii])) {
      buf[ii] = ' ';
    }
  }
  ii = (int)(len)-1;
  while (isspace(buf[ii])) {
    buf[ii--] = 0;
  }
  /* printf("%s: HEY(after): err[%s]->num = %d\n", me, e->key, e->num); */
  /* printf("%s: HEY: err[%s][%d] now \"%s\"\n", me, e->key, e->num-1, buf); */
  e->err[e->num-1] = buf;
  return;
}

void
_biffFindMaxAndSum(unsigned int *maxP, unsigned int *sumP, _biffEntry *ent) {
  unsigned int ii, len;

  if (!ent->num) {
    /* there's a key, but no error messages.  Odd. */
    *maxP = 1;
    *sumP = 1;
    return;
  }

  *maxP = *sumP = 0;
  for (ii=0; ii<ent->num; ii++) {
    len = (unsigned int)(strlen(ent->err[ii]) + strlen(ent->key) + strlen("[] \n"));
    *sumP += len;
    *maxP = AIR_MAX(*maxP, len);
  }
  *sumP += 1;
  *maxP += 1;
  return;
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
  _biffEntry *ent;

  _biffInit();
  _biffCheckKey(key);
  
  ent = _biffFindKey(key);
  if (!ent) {
    ent = _biffAddKey(key);
  }

  /* add the new message */
  _biffAddErr(ent, err);
  return;
}

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
_biffGetStr(char *ret, char *buf, _biffEntry *ent) {
  int ii;
  
  if (!ent->num) {
    /* there's a key, but no error messages.  Odd. */
    strcpy(ret, "");
  }
  for (ii=ent->num-1; ii>=0; ii--) {
    sprintf(buf, "[%s] %s\n", ent->key, ent->err[ii]);
    strcat(ret, buf);
  }
  return;
}

/*
******** biffGet()
**
** creates a string which records all the errors at given key and
** returns it.  Returns NULL in case of error.  This function should
** be considered a glorified strdup(): it is the callers responsibility
** to free this string later
*/
char *
biffGet(const char *key) {
  unsigned int max, sum;
  char me[] = "biffGet", *ret, *buf;
  _biffEntry *ent;

  _biffInit();
  _biffCheckKey(key);
  ent = _biffFindKey(key);
  if (!ent) {
    /* error: not a key we remember seeing */
    fprintf(stderr, "%s: WARNING: no information for key \"%s\"\n", me, key);
    return NULL;
  }

  _biffFindMaxAndSum(&max, &sum, ent);
  buf = (char*)calloc(max, sizeof(char));
  ret = (char*)calloc(sum, sizeof(char));
  if (!(buf && ret)) {
    fprintf(stderr, "%s: PANIC: unable to allocate buffers\n", me);
    exit(1);
  }
  _biffGetStr(ret, buf, ent);
  free(buf);

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
  unsigned int max, sum;
  char me[] = "biffGetStrlen";
  _biffEntry *ent;

  _biffInit();
  _biffCheckKey(key);
  ent = _biffFindKey(key);
  if (!ent) {
    /* error: not a key we remember seeing */
    fprintf(stderr, "%s: WARNING: no information for key \"%s\"\n", me, key);
    return 0;
  }

  _biffFindMaxAndSum(&max, &sum, ent);
  return sum;
}

/*
******** biffSetStr()
**
** for when you want to allocate the buffer for the biff string, this is
** how you get the error message itself
*/
void
biffSetStr(char *str, const char *key) {
  unsigned int max, sum;
  char me[] = "biffSetStr", *buf;
  _biffEntry *ent;

  if (!str) {
    fprintf(stderr, "%s: ERROR: got NULL buffer \"%s\"\n", me, key);
    return;
  }

  _biffInit();
  _biffCheckKey(key);
  ent = _biffFindKey(key);
  if (!ent) {
    /* error: not a key we remember seeing */
    fprintf(stderr, "%s: WARNING: no information for key \"%s\"\n", me, key);
    return;
  }

  _biffFindMaxAndSum(&max, &sum, ent);
  buf = (char*)calloc(max, sizeof(char));
  if (!buf) {
    fprintf(stderr, "%s: PANIC: unable to allocate buffer\n", me);
    exit(1);
  }
  _biffGetStr(str, buf, ent);
  free(buf);

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
  _biffEntry *ent;

  _biffInit();
  _biffCheckKey(key);
  
  ent = _biffFindKey(key);
  if (!ent) {
    return 0;
  }
  
  return ent->num;
}

/*
******** biffDone()
**
** frees everything associated with given key, and shrinks list of keys
*/
void
biffDone(const char *key) {
  char me[]="biffDone";
  int i, idx;
  _biffEntry *ent;

  _biffInit();
  _biffCheckKey(key);

  ent = _biffFindKey(key);
  if (!ent) {
    fprintf(stderr, "%s: WARNING: no information for key \"%s\"\n", me, key);
    return;
  }
  idx = _biffIdx;

  _biffNukeEntry(ent);
  for (i=idx; i<(int)_biffNum-1; i++) {
    _biffErr[i] = _biffErr[i+1];
  }
  airArrayLenIncr(_biffAA, -1);

  return;
}

void
biffMove(const char *destKey, const char *err, const char *srcKey) {
  unsigned int ii; 
  size_t len;        // to match signature of strlen() on 64 bits
  size_t max;
  char me[] = "biffMove", *buf;
  _biffEntry *dest, *src;

  _biffInit();
  _biffCheckKey(destKey);
  _biffCheckKey(srcKey);

  /* if srcKey and destKey are the same, this degenerates to biffAdd() */
  if (!strcmp(destKey, srcKey)) {
    biffAdd(srcKey, err);
    return;
  }

  dest = _biffFindKey(destKey);
  if (!dest) {
    dest = _biffAddKey(destKey);
  }
  src = _biffFindKey(srcKey);
  if (!src) {
    fprintf(stderr, "%s: WARNING: key \"%s\" unknown\n", me, srcKey);
    return;
  }

  max = 0;
  for (ii=0; ii<src->num; ii++) {
    len = strlen(src->err[ii]) + strlen(src->key) + 4;
    max = AIR_MAX(max, len);
  }
  buf = (char*)calloc(max+1, sizeof(char));
  if (!buf) {
    fprintf(stderr, "%s: PANIC: can't allocate buffer\n", me);
    exit(1);
  }

  for (ii=0; ii<src->num; ii++) {
    sprintf(buf, "[%s] %s", srcKey, src->err[ii]);
    /* printf("%s: HEY: moving \"%s\" to %s\n", me, buf, destKey); */
    _biffAddErr(dest, buf);
  }
  if (err) {
    _biffAddErr(dest, err);
  }
  biffDone(srcKey);
  free(buf);

  return;
}

char *
biffGetDone(const char *key) {
  char *ret;

  _biffInit();
  _biffCheckKey(key);

  ret = biffGet(key);
  biffDone(key);
  _biffNuke();

  return ret;
}

void
biffSetStrDone(char *str, const char *key) {

  _biffInit();
  _biffCheckKey(key);

  biffSetStr(str, key);
  biffDone(key);
  _biffNuke();

  return;
}

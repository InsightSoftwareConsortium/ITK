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

#include "NrrdIO.h"
#include "privateNrrd.h"
#include "float.h"

/* 
** making these typedefs here allows us to used one token for both
** constructing function names, and for specifying argument types
*/
typedef signed char CH;
typedef unsigned char UC;
typedef signed short SH;
typedef unsigned short US;
/* Microsoft apparently uses 'IN' as a keyword, so we changed 'IN' to 'JN'. */
typedef signed int JN;
typedef unsigned int UI;
typedef airLLong LL;
/* ui64 to double conversion is not implemented, sorry */
#if _MSC_VER < 1300
typedef airLLong UL;
#else
typedef airULLong UL;
#endif
typedef float FL;
typedef double DB;

#define MAP(F, A) \
F(A, CH) \
F(A, UC) \
F(A, SH) \
F(A, US) \
F(A, JN) \
F(A, UI) \
F(A, LL) \
F(A, UL) \
F(A, FL) \
F(A, DB)

/* 
** _nrrdLoad<TA><TB>(<TB> *v)
**
** Dereferences v as TB*, casts it to TA, returns it.
*/
#define LOAD_DEF(TA, TB)                    \
TA                                          \
_nrrdLoad##TA##TB(TB *v) {                  \
  return (TA)(*v);                          \
}
#define LOAD_LIST(TA, TB)                   \
  (TA (*)(const void *))_nrrdLoad##TA##TB,

MAP(LOAD_DEF, UI)
MAP(LOAD_DEF, JN)
MAP(LOAD_DEF, FL)
MAP(LOAD_DEF, DB)

unsigned int (*
nrrdUILoad[NRRD_TYPE_MAX+1])(const void*) = {
  NULL, MAP(LOAD_LIST, UI) NULL
};
int (*
nrrdILoad[NRRD_TYPE_MAX+1])(const void*) = {
  NULL, MAP(LOAD_LIST, JN) NULL
};
float (*
nrrdFLoad[NRRD_TYPE_MAX+1])(const void*) = {
  NULL, MAP(LOAD_LIST, FL) NULL
};
double (*
nrrdDLoad[NRRD_TYPE_MAX+1])(const void*) = {
  NULL, MAP(LOAD_LIST, DB) NULL
};


/*
** _nrrdStore<TA><TB>(<TB> *v, <TA> j)
**
** Takes a TA j, and stores it in *v, thereby implicitly casting it to TB.
** Returns the result of the assignment, which may not be the same as
** the value that was passed in.
*/
#define STORE_DEF(TA, TB)                   \
TA                                          \
_nrrdStore##TA##TB(TB *v, TA j) {           \
  return (TA)(*v = (TB)j);                  \
}
#define STORE_LIST(TA, TB)                  \
  (TA (*)(void *, TA))_nrrdStore##TA##TB,

MAP(STORE_DEF, UI)
MAP(STORE_DEF, JN)
MAP(STORE_DEF, FL)
MAP(STORE_DEF, DB)

unsigned int (*
nrrdUIStore[NRRD_TYPE_MAX+1])(void *, unsigned int) = {
  NULL, MAP(STORE_LIST, UI) NULL
};
int (*
nrrdIStore[NRRD_TYPE_MAX+1])(void *, int) = {
  NULL, MAP(STORE_LIST, JN) NULL
};
float (*
nrrdFStore[NRRD_TYPE_MAX+1])(void *, float) = {
  NULL, MAP(STORE_LIST, FL) NULL
};
double (*
nrrdDStore[NRRD_TYPE_MAX+1])(void *, double) = {
  NULL, MAP(STORE_LIST, DB) NULL
};


/*
** _nrrdLookup<TA><TB>(<TB> *v, size_t I)
**
** Looks up element I of TB array v, and returns it cast to a TA.
*/
#define LOOKUP_DEF(TA, TB)                    \
TA                                            \
_nrrdLookup##TA##TB(TB *v, size_t I) {        \
  return (TA)v[I];                            \
}
#define LOOKUP_LIST(TA, TB)                   \
  (TA (*)(const void*, size_t))_nrrdLookup##TA##TB,

MAP(LOOKUP_DEF, UI)
MAP(LOOKUP_DEF, JN)
MAP(LOOKUP_DEF, FL)
MAP(LOOKUP_DEF, DB)

unsigned int (*
nrrdUILookup[NRRD_TYPE_MAX+1])(const void *, size_t) = {
  NULL, MAP(LOOKUP_LIST, UI) NULL
};
int (*
nrrdILookup[NRRD_TYPE_MAX+1])(const void *, size_t) = {
  NULL, MAP(LOOKUP_LIST, JN) NULL
};
float (*
nrrdFLookup[NRRD_TYPE_MAX+1])(const void *, size_t) = {
  NULL, MAP(LOOKUP_LIST, FL) NULL
};
double (*
nrrdDLookup[NRRD_TYPE_MAX+1])(const void *, size_t) = {
  NULL, MAP(LOOKUP_LIST, DB) NULL
};


/*
** _nrrdInsert<TA><TB>(<TB> *v, size_t I, <TA> j)
**
** Given TA j, stores it in v[i] (implicitly casting to TB).
** Returns the result of the assignment, which may not be the same as
** the value that was passed in.
*/
#define INSERT_DEF(TA, TB)                         \
TA                                                 \
_nrrdInsert##TA##TB(TB *v, size_t I, TA j) {       \
  return (TA)(v[I] = (TB)j);                       \
}
#define INSERT_LIST(TA, TB)                        \
  (TA (*)(void*, size_t, TA))_nrrdInsert##TA##TB,

MAP(INSERT_DEF, UI)
MAP(INSERT_DEF, JN)
MAP(INSERT_DEF, FL)
MAP(INSERT_DEF, DB)

unsigned int (*
nrrdUIInsert[NRRD_TYPE_MAX+1])(void *, size_t, unsigned int) = {
  NULL, MAP(INSERT_LIST, UI) NULL
};
int (*
nrrdIInsert[NRRD_TYPE_MAX+1])(void *, size_t, int) = {
  NULL, MAP(INSERT_LIST, JN) NULL
};
float (*
nrrdFInsert[NRRD_TYPE_MAX+1])(void *, size_t, float) = {
  NULL, MAP(INSERT_LIST, FL) NULL
};
double (*
nrrdDInsert[NRRD_TYPE_MAX+1])(void *, size_t, double) = {
  NULL, MAP(INSERT_LIST, DB) NULL
};

/*
******** nrrdSprint
**
** Dereferences pointer v and sprintf()s that value into given string s,
** returns the result of sprintf()
**
** There is obviously no provision for ensuring that the sprint'ing
** doesn't overflow the buffer, which is unfortunate...
*/
static int _nrrdSprintCH(char *s, const CH *v) { return sprintf(s, "%d", *v); }
static int _nrrdSprintUC(char *s, const UC *v) { return sprintf(s, "%u", *v); }
static int _nrrdSprintSH(char *s, const SH *v) { return sprintf(s, "%d", *v); }
static int _nrrdSprintUS(char *s, const US *v) { return sprintf(s, "%u", *v); }
static int _nrrdSprintIN(char *s, const JN *v) { return sprintf(s, "%d", *v); }
static int _nrrdSprintUI(char *s, const UI *v) { return sprintf(s, "%u", *v); }
static int _nrrdSprintLL(char *s, const LL *v) { 
  return sprintf(s, AIR_LLONG_FMT, *v); 
}
static int _nrrdSprintUL(char *s, const UL *v) { 
  return sprintf(s, AIR_ULLONG_FMT, *v); 
}
/* HEY: sizeof(float) and sizeof(double) assumed here, since we're 
   basing "8" and "17" on 6 == FLT_DIG and 15 == DBL_DIG, which are 
   digits of precision for floats and doubles, respectively */
static int _nrrdSprintFL(char *s, const FL *v) {
  return airSinglePrintf(NULL, s, "%.8g", (double)(*v)); }
static int _nrrdSprintDB(char *s, const DB *v) {
  return airSinglePrintf(NULL, s, "%.17g", *v); }
int (*
nrrdSprint[NRRD_TYPE_MAX+1])(char *, const void *) = {
  NULL,
  (int (*)(char *, const void *))_nrrdSprintCH,
  (int (*)(char *, const void *))_nrrdSprintUC,
  (int (*)(char *, const void *))_nrrdSprintSH,
  (int (*)(char *, const void *))_nrrdSprintUS,
  (int (*)(char *, const void *))_nrrdSprintIN,
  (int (*)(char *, const void *))_nrrdSprintUI,
  (int (*)(char *, const void *))_nrrdSprintLL,
  (int (*)(char *, const void *))_nrrdSprintUL,
  (int (*)(char *, const void *))_nrrdSprintFL,
  (int (*)(char *, const void *))_nrrdSprintDB,
  NULL};

/* ---- BEGIN non-NrrdIO */

/*
******** nrrdFprint
**
** Dereferences pointer v and fprintf()s that value into given file f;
** returns the result of fprintf()
*/
static int _nrrdFprintCH(FILE *f, const CH *v) { return fprintf(f, "%d", *v); }
static int _nrrdFprintUC(FILE *f, const UC *v) { return fprintf(f, "%u", *v); }
static int _nrrdFprintSH(FILE *f, const SH *v) { return fprintf(f, "%d", *v); }
static int _nrrdFprintUS(FILE *f, const US *v) { return fprintf(f, "%u", *v); }
static int _nrrdFprintIN(FILE *f, const JN *v) { return fprintf(f, "%d", *v); }
static int _nrrdFprintUI(FILE *f, const UI *v) { return fprintf(f, "%u", *v); }
static int _nrrdFprintLL(FILE *f, const LL *v) { 
  return fprintf(f, AIR_LLONG_FMT, *v); 
}
static int _nrrdFprintUL(FILE *f, const UL *v) { 
  return fprintf(f, AIR_ULLONG_FMT, *v); 
}
static int _nrrdFprintFL(FILE *f, const FL *v) {
  return airSinglePrintf(f, NULL, "%.8g", (double)(*v)); }
static int _nrrdFprintDB(FILE *f, const DB *v) {
  return airSinglePrintf(f, NULL, "%.17g", *v); }
int (*
nrrdFprint[NRRD_TYPE_MAX+1])(FILE *, const void *) = {
  NULL,
  (int (*)(FILE *, const void *))_nrrdFprintCH,
  (int (*)(FILE *, const void *))_nrrdFprintUC,
  (int (*)(FILE *, const void *))_nrrdFprintSH,
  (int (*)(FILE *, const void *))_nrrdFprintUS,
  (int (*)(FILE *, const void *))_nrrdFprintIN,
  (int (*)(FILE *, const void *))_nrrdFprintUI,
  (int (*)(FILE *, const void *))_nrrdFprintLL,
  (int (*)(FILE *, const void *))_nrrdFprintUL,
  (int (*)(FILE *, const void *))_nrrdFprintFL,
  (int (*)(FILE *, const void *))_nrrdFprintDB,
  NULL};

/* about here is where Gordon admits he might have some use for C++ */

#define _MMEF_ARGS(type) type *minP, type *maxP, int *hneP, const Nrrd *nrrd

#define _MMEF_FIXED(type)                                                \
  size_t I, N;                                                           \
  type a, b, min, max, *v;                                               \
                                                                         \
  if (!(minP && maxP))                                                   \
    return;                                                              \
                                                                         \
  /* all integral values exist */                                        \
  *hneP = nrrdHasNonExistFalse;                                          \
                                                                         \
  /* set the local data pointer */                                       \
  v = (type*)(nrrd->data);                                               \
                                                                         \
  /* get initial values */                                               \
  N = nrrdElementNumber(nrrd);                                           \
  min = max = v[0];                                                      \
                                                                         \
  /* run through array in pairs; by doing a compare on successive        \
     elements, we can do three compares per pair instead of the naive    \
     four.  In one very unexhaustive test on irix6.64, this resulted     \
     in a 20% decrease in running time.  I learned this trick from       \
     Numerical Recipes in C, long time ago, but I can't find it          \
     anywhere in the book now ... */                                     \
  for (I=0; I<=N-2; I+=2) {                                              \
    a = v[0 + I];                                                        \
    b = v[1 + I];                                                        \
    if (a < b) {                                                         \
      min = AIR_MIN(a, min);                                             \
      max = AIR_MAX(b, max);                                             \
    } else {                                                             \
      max = AIR_MAX(a, max);                                             \
      min = AIR_MIN(b, min);                                             \
    }                                                                    \
  }                                                                      \
                                                                         \
  /* get the very last one (may be redundant) */                         \
  a = v[N-1];                                                            \
  min = AIR_MIN(a, min);                                                 \
  max = AIR_MAX(a, max);                                                 \
                                                                         \
  /* record results */                                                   \
  *minP = min;                                                           \
  *maxP = max;

#define _MMEF_FLOAT(type)                                                \
  size_t I, N;                                                           \
  type a, min, max, *v;                                                  \
                                                                         \
  if (!(minP && maxP))                                                   \
    return;                                                              \
                                                                         \
  /* this may be over-written below */                                   \
  *hneP = nrrdHasNonExistFalse;                                          \
                                                                         \
  /* set the local data pointer */                                       \
  N = nrrdElementNumber(nrrd);                                           \
  v = (type*)(nrrd->data);                                               \
                                                                         \
  /* we have to explicitly search for the first non-NaN value */         \
  max = min = AIR_NAN;                                                   \
  for (I=0; I<N; I++) {                                                  \
    a = v[I];                                                            \
    if (AIR_EXISTS(a)) {                                                 \
      min = max = a;                                                     \
      break;                                                             \
    } else {                                                             \
      *hneP = nrrdHasNonExistTrue;                                       \
    }                                                                    \
  }                                                                      \
  if (I == N) {                                                          \
    /* oh dear, there were NO existent values */                         \
    min = max = AIR_NAN;                                                 \
    *hneP = nrrdHasNonExistOnly;                                         \
  } else {                                                               \
    /* there was at least one existent value; we continue searching,     \
       still checking AIR_EXISTS at each value */                        \
    for (I=I+1; I<N; I++) {                                              \
      a = v[I];                                                          \
      if (AIR_EXISTS(a)) {                                               \
        if (a < min) {                                                   \
          min = a;                                                       \
        } else {                                                         \
          if (a > max) {                                                 \
            max = a;                                                     \
          }                                                              \
        }                                                                \
      } else {                                                           \
        *hneP = nrrdHasNonExistTrue;                                     \
      }                                                                  \
    }                                                                    \
  }                                                                      \
  *minP = min;                                                           \
  *maxP = max;

static void _nrrdMinMaxExactFindCH (_MMEF_ARGS(CH)) {_MMEF_FIXED(CH)}
static void _nrrdMinMaxExactFindUC (_MMEF_ARGS(UC)) {_MMEF_FIXED(UC)}
static void _nrrdMinMaxExactFindSH (_MMEF_ARGS(SH)) {_MMEF_FIXED(SH)}
static void _nrrdMinMaxExactFindUS (_MMEF_ARGS(US)) {_MMEF_FIXED(US)}
static void _nrrdMinMaxExactFindIN (_MMEF_ARGS(JN)) {_MMEF_FIXED(JN)}
static void _nrrdMinMaxExactFindUI (_MMEF_ARGS(UI)) {_MMEF_FIXED(UI)}
static void _nrrdMinMaxExactFindLL (_MMEF_ARGS(LL)) {_MMEF_FIXED(LL)}
static void _nrrdMinMaxExactFindUL (_MMEF_ARGS(UL)) {_MMEF_FIXED(UL)}
static void _nrrdMinMaxExactFindFL (_MMEF_ARGS(FL)) {_MMEF_FLOAT(FL)}
static void _nrrdMinMaxExactFindDB (_MMEF_ARGS(DB)) {_MMEF_FLOAT(DB)}

/*
******** nrrdMinMaxExactFind[]
**
** the role of these is to allow finding the EXACT min and max of a nrrd,
** so that one does not have to rely on the potentially lossy storage
** of the min and max values in range->min and range->max, which are doubles.
**
** These also sets *hneP, using a value from the nrrdHasNonExist* enum
*/
void (*
nrrdMinMaxExactFind[NRRD_TYPE_MAX+1])(void *minP, void *maxP,
                                      int *hneP, const Nrrd *) = {
  NULL,
  (void (*)(void *, void *, int *, const Nrrd *))_nrrdMinMaxExactFindCH,
  (void (*)(void *, void *, int *, const Nrrd *))_nrrdMinMaxExactFindUC,
  (void (*)(void *, void *, int *, const Nrrd *))_nrrdMinMaxExactFindSH,
  (void (*)(void *, void *, int *, const Nrrd *))_nrrdMinMaxExactFindUS,
  (void (*)(void *, void *, int *, const Nrrd *))_nrrdMinMaxExactFindIN,
  (void (*)(void *, void *, int *, const Nrrd *))_nrrdMinMaxExactFindUI,
  (void (*)(void *, void *, int *, const Nrrd *))_nrrdMinMaxExactFindLL,
  (void (*)(void *, void *, int *, const Nrrd *))_nrrdMinMaxExactFindUL,
  (void (*)(void *, void *, int *, const Nrrd *))_nrrdMinMaxExactFindFL,
  (void (*)(void *, void *, int *, const Nrrd *))_nrrdMinMaxExactFindDB,
  NULL
};

/*
******** nrrdValCompare[]
**
** the sort of compare you'd give to qsort() to sort in ascending order:
** return < 0 if A < B,
**          0 if A == B,
**        > 0 if A > B
** The non-trivial part of this is that for floating-point values, we
** dictate that all non-existent values are smaller than all existent
** values, regardless of their actual values (so +infinity < -42).  This
** is to make sure that we have comparison that won't confuse qsort(),
** which underlies _nrrdMeasureMedian(), and to make it easier to seperate
** existant from non-existant values.
*/
#define _VC_ARGS(type) const type *A, const type *B
#define _VC_FIXED (*A < *B ? -1 : (*A > *B ? 1 : 0))
#define _VC_FLOAT                                                        \
  int ex, ret;                                                           \
                                                                         \
  ex = AIR_EXISTS(*A) + AIR_EXISTS(*B);                                  \
  switch (ex) {                                                          \
  case 2: ret = _VC_FIXED; break;                                        \
  case 1: ret = AIR_EXISTS(*A) ? 1 : -1; break;                          \
  case 0: default: ret = 0;                                              \
  }

static int _nrrdValCompareCH (_VC_ARGS(CH)) {return _VC_FIXED;}
static int _nrrdValCompareUC (_VC_ARGS(UC)) {return _VC_FIXED;}
static int _nrrdValCompareSH (_VC_ARGS(SH)) {return _VC_FIXED;}
static int _nrrdValCompareUS (_VC_ARGS(US)) {return _VC_FIXED;}
static int _nrrdValCompareIN (_VC_ARGS(JN)) {return _VC_FIXED;}
static int _nrrdValCompareUI (_VC_ARGS(UI)) {return _VC_FIXED;}
static int _nrrdValCompareLL (_VC_ARGS(LL)) {return _VC_FIXED;}
static int _nrrdValCompareUL (_VC_ARGS(UL)) {return _VC_FIXED;}
static int _nrrdValCompareFL (_VC_ARGS(FL)) {_VC_FLOAT; return ret;}
static int _nrrdValCompareDB (_VC_ARGS(DB)) {_VC_FLOAT; return ret;}
int (*
nrrdValCompare[NRRD_TYPE_MAX+1])(const void *, const void *) = {
  NULL,
  (int (*)(const void *, const void *))_nrrdValCompareCH,
  (int (*)(const void *, const void *))_nrrdValCompareUC,
  (int (*)(const void *, const void *))_nrrdValCompareSH,
  (int (*)(const void *, const void *))_nrrdValCompareUS,
  (int (*)(const void *, const void *))_nrrdValCompareIN,
  (int (*)(const void *, const void *))_nrrdValCompareUI,
  (int (*)(const void *, const void *))_nrrdValCompareLL,
  (int (*)(const void *, const void *))_nrrdValCompareUL,
  (int (*)(const void *, const void *))_nrrdValCompareFL,
  (int (*)(const void *, const void *))_nrrdValCompareDB,
  NULL
};

/*
** ...Inv: for descending order
*/
static int _nrrdValCompareInvCH (_VC_ARGS(CH)) {return -_VC_FIXED;}
static int _nrrdValCompareInvUC (_VC_ARGS(UC)) {return -_VC_FIXED;}
static int _nrrdValCompareInvSH (_VC_ARGS(SH)) {return -_VC_FIXED;}
static int _nrrdValCompareInvUS (_VC_ARGS(US)) {return -_VC_FIXED;}
static int _nrrdValCompareInvIN (_VC_ARGS(JN)) {return -_VC_FIXED;}
static int _nrrdValCompareInvUI (_VC_ARGS(UI)) {return -_VC_FIXED;}
static int _nrrdValCompareInvLL (_VC_ARGS(LL)) {return -_VC_FIXED;}
static int _nrrdValCompareInvUL (_VC_ARGS(UL)) {return -_VC_FIXED;}
static int _nrrdValCompareInvFL (_VC_ARGS(FL)) {_VC_FLOAT; return -ret;}
static int _nrrdValCompareInvDB (_VC_ARGS(DB)) {_VC_FLOAT; return -ret;}
int (*
nrrdValCompareInv[NRRD_TYPE_MAX+1])(const void *, const void *) = {
  NULL,
  (int (*)(const void *, const void *))_nrrdValCompareInvCH,
  (int (*)(const void *, const void *))_nrrdValCompareInvUC,
  (int (*)(const void *, const void *))_nrrdValCompareInvSH,
  (int (*)(const void *, const void *))_nrrdValCompareInvUS,
  (int (*)(const void *, const void *))_nrrdValCompareInvIN,
  (int (*)(const void *, const void *))_nrrdValCompareInvUI,
  (int (*)(const void *, const void *))_nrrdValCompareInvLL,
  (int (*)(const void *, const void *))_nrrdValCompareInvUL,
  (int (*)(const void *, const void *))_nrrdValCompareInvFL,
  (int (*)(const void *, const void *))_nrrdValCompareInvDB,
  NULL
};

/* ---- END non-NrrdIO */

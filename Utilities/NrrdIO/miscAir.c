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
#include <teem32bit.h>
/* timer functions */
#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
#include <time.h>
#else
#include <sys/time.h>
#endif

/*
******** airTeemVersion
******** airTeemReleaseDate
**
** updated with each release to contain a string representation of 
** the Teem version number and release date.  Originated in version 1.5;
** use of TEEM_VERSION #defines started in 1.9
*/
const char *
airTeemVersion = TEEM_VERSION_STRING;
const char *
airTeemReleaseDate = "late 2009 or early 2010";

double
_airSanityHelper(double val) {
  return val*val*val;
}

/*
******** airNull()
**
** returns NULL
*/
void *
airNull(void) {

  return NULL;
}

/*
******** airSetNull
**
** dereferences and sets to NULL, returns NULL
*/
void *
airSetNull(void **ptrP) {
  
  if (ptrP) {
    *ptrP = NULL;
  }
  return NULL;
}

/*
******** airFree()
**
** to facilitate setting a newly free()'d pointer; always returns NULL.
** also makes sure that NULL is not passed to free().
*/
void *
airFree(void *ptr) {

  if (ptr) {
    free(ptr);
  }
  return NULL;
}

/*
******** airFopen()
**
** encapsulates that idea that "-" is either standard in or stardard
** out, and does McRosopht stuff required to make piping work 
**
** Does not error checking.  If fopen fails, then C' errno and strerror are
** left untouched for the caller to access.
*/
FILE *
airFopen(const char *name, FILE *std, const char *mode) {
  FILE *ret;

  if (!strcmp(name, "-")) {
    ret = std;
#ifdef _MSC_VER
    if (strchr(mode, 'b')) {
      _setmode(_fileno(ret), _O_BINARY);
    }
#endif
  } else {
    ret = fopen(name, mode);
  }
  return ret;
}


/*
******** airFclose()
**
** just to facilitate setting a newly fclose()'d file pointer to NULL
** also makes sure that NULL is not passed to fclose(), and won't close
** stdin, stdout, or stderr (its up to the user to open these correctly)
*/
FILE *
airFclose(FILE *file) {

  if (file) {
    if (!( stdin == file || stdout == file || stderr == file )) {
      fclose(file);
    }
  }
  return NULL;
}

/*
******** airSinglePrintf
**
** a complete stand-in for {f|s}printf(), as long as the given format
** string contains exactly one conversion sequence.  The utility of
** this is to standardize the printing of IEEE 754 special values:
** QNAN, SNAN -> "NaN"
** POS_INF -> "+inf"
** NEG_INF -> "-inf"
** The format string can contain other things besides just the
** conversion sequence: airSingleFprintf(f, " (%f)\n", AIR_NAN)
** will be the same as fprintf(f, " (%s)\n", "NaN");
**
** To get fprintf behavior, pass "str" as NULL
** to get sprintf bahavior, pass "file" as NULL
**
** Someday I'll find/write a complete {f|s|}printf replacement ...
*/
int
airSinglePrintf(FILE *file, char *str, const char *_fmt, ...) {
  char *fmt, buff[AIR_STRLEN_LARGE];
  double val=0, gVal, fVal;
  int ret, isF, isD, cls;
  char *conv=NULL, *p0, *p1, *p2, *p3, *p4, *p5;
  va_list ap;
  
  va_start(ap, _fmt);
  fmt = airStrdup(_fmt);

  /* this is needlessly complicated; the "l" modifier is a no-op */
  p0 = strstr(fmt, "%e");
  p1 = strstr(fmt, "%f");
  p2 = strstr(fmt, "%g");
  p3 = strstr(fmt, "%le");
  p4 = strstr(fmt, "%lf");
  p5 = strstr(fmt, "%lg");
  isF = p0 || p1 || p2;
  isD = p3 || p4 || p5;
  /* the code here says "isF" and "isD" as if it means "is float" or 
     "is double".  It really should be "is2" or "is3", as in, 
     "is 2-character conversion sequence, or "is 3-character..." */
  if (isF) {
    conv = p0 ? p0 : (p1 ? p1 : p2);
  }
  if (isD) {
    conv = p3 ? p3 : (p4 ? p4 : p5);
  }
  if (isF || isD) {
    /* use "double" instead of "float" because var args are _always_
       subject to old-style C type promotions: float promotes to double */
    val = va_arg(ap, double);
    cls = airFPClass_d(val);
    switch (cls) {
    case airFP_SNAN:
    case airFP_QNAN:
    case airFP_POS_INF:
    case airFP_NEG_INF:
      if (isF) {
        memcpy(conv, "%s", 2);
      } else {
        /* this sneakiness allows us to replace a 3-character conversion
           sequence for a double (such as %lg) with a 3-character conversion
           for a string, which we know has at most 4 characters */
        memcpy(conv, "%4s", 3);
      }
      break;
    }
#define PRINT(F, S, C, V) ((F) ? fprintf((F),(C),(V)) : sprintf((S),(C),(V)))
    switch (cls) {
    case airFP_SNAN:
    case airFP_QNAN:
      ret = PRINT(file, str, fmt, "NaN");
      break;
    case airFP_POS_INF:
      ret = PRINT(file, str, fmt, "+inf");
      break;
    case airFP_NEG_INF:
      ret = PRINT(file, str, fmt, "-inf");
      break;
    default:
      if (p2 || p5) {
        /* got "%g" or "%lg", see if it would be better to use "%f" */
        sprintf(buff, "%f", val);
        sscanf(buff, "%lf", &fVal);
        sprintf(buff, "%g", val);
        sscanf(buff, "%lf", &gVal);
        if (fVal != gVal) {
          /* using %g (or %lg) lost precision!! Use %f (or %lf) instead */
          if (p2) {
            memcpy(conv, "%f", 2);
          } else {
            memcpy(conv, "%lf", 3);
          }
        }
      }
      ret = PRINT(file, str, fmt, val);
      break;
    }
  } else {
    /* conversion sequence is neither for float nor double */
    ret = file ? vfprintf(file, fmt, ap) : vsprintf(str, fmt, ap);
  }
  
  va_end(ap);
  free(fmt);
  return ret;
}

#if TEEM_32BIT == 1
const int airMy32Bit = 1;
#else
const int airMy32Bit = 0;
#endif

/* ---- BEGIN non-NrrdIO */

/*
******** airStderr, airStdout, airStdin
**
** these exist only to give uniform access to FILE *s that might be
** annoying to access in higher-level language wrappings around Teem.
*/
FILE *
airStderr(void) {
  return stderr;
}

FILE *
airStdout(void) {
  return stdout;
}

FILE *
airStdin(void) {
  return stdin;
}

#if TEEM_32BIT == 1
const char airMyFmt_size_t[] = "%u";
#else
const char airMyFmt_size_t[] = "%lu";
#endif

/*
******** AIR_INDEX(i,x,I,L,t)
**
** READ CAREFULLY!!
**
** Utility for mapping a floating point x in given range [i,I] to the
** index of an array with L elements, AND SAVES THE INDEX INTO GIVEN
** VARIABLE t, WHICH MUST BE OF SOME INTEGER TYPE because this relies
** on the implicit cast of an assignment to truncate away the
** fractional part.  ALSO, t must be of a type large enough to hold
** ONE GREATER than L.  So you can't pass a variable of type unsigned
** char if L is 256
**
** DOES NOT DO BOUNDS CHECKING: given an x which is not inside [i,I],
** this may produce an index not inside [0,L-1] (but it won't always
** do so: the output being outside range [0,L-1] is not a reliable
** test of the input being outside range [i, I]).  The mapping is
** accomplished by dividing the range from i to I into L intervals,
** all but the last of which is half-open; the last one is closed.
** For example, the number line from 0 to 3 would be divided as
** follows for a call with i = 0, I = 4, L = 4:
**
** index:       0    1    2    3 = L-1
** intervals: [   )[   )[   )[    ]
**            |----|----|----|----|
** value:     0    1    2    3    4
**
** The main point of the diagram above is to show how I made the
** arbitrary decision to orient the half-open interval, and which
** end has the closed interval.
**
** Note that AIR_INDEX(0,3,4,4,t) and AIR_INDEX(0,4,4,4,t) both set t = 3
**
** The reason that this macro requires a argument for saving the
** result is that this is the easiest way to avoid extra conditionals.
** Otherwise, we'd have to do some check to see if x is close enough
** to I so that the generated index would be L and not L-1.  "Close
** enough" because due to precision problems you can have an x < I
** such that (x-i)/(I-i) == 1, which was a bug with the previous version
** of this macro.  It is far simpler to just do the index generation
** and then do the sneaky check to see if the index is too large by 1.
** We are relying on the fact that C _defines_ boolean true to be exactly 1.
**
** Note also that we are never explicity casting to one kind of int or
** another-- the given t can be any integral type, including long long.
*/
/*
#define AIR_INDEX(i,x,I,L,t) ( \
(t) = (L) * ((double)(x)-(i)) / ((double)(I)-(i)), \
(t) -= ((t) == (L)) )
*/
/*
******* airIndex
**
** replaces AIR_INDEX macro; see above
*/
unsigned int
airIndex(double min, double val, double max, unsigned int N) {
  unsigned int idx;

  idx = AIR_CAST(unsigned int, N*(val - min)/(max - min));
  idx -= (idx == N);
  return idx;
}

unsigned int
airIndexClamp(double min, double val, double max, unsigned int N) {
  unsigned int idx;

  /* NOTE: now that unsigned types are used more widely in Teem, the
     clamping that used to happen after index generation now must
     happen prior to index generation */
  val = AIR_MAX(min, val);
  idx = AIR_CAST(unsigned int, N*(val - min)/(max - min));
  idx = AIR_MIN(idx, N-1);
  return idx;
}

airULLong
airIndexULL(double min, double val, double max, airULLong N) {
  airULLong idx;
#if defined(_WIN32) && !defined(__CYGWIN__) && !defined(__MINGW32__)
  /* compile error on Win32-vs60: "error C2520: conversion from
     unsigned __int64 to double not implemented, use signed __int64 ... */
  airLLong sidx;
  sidx = AIR_CAST(airLLong, N*(val - min)/(max - min));
  idx = AIR_CAST(airULLong, sidx);
#else
  idx = AIR_CAST(airULLong, N*(val - min)/(max - min));
#endif
  idx -= (idx == N);
  return idx;
}

airULLong
airIndexClampULL(double min, double val, double max, airULLong N) {
  airULLong idx;
#if defined(_WIN32) && !defined(__CYGWIN__) && !defined(__MINGW32__)
  airLLong sidx;
  val = AIR_MAX(min, val); /* see note in airIndexClamp */
  sidx = AIR_CAST(airLLong, N*(val - min)/(max - min));
  idx = AIR_CAST(airULLong, sidx);
#else
  val = AIR_MAX(min, val); /* see note in airIndexClamp */
  idx = AIR_CAST(airULLong, N*(val - min)/(max - min));
#endif
  idx = AIR_MIN(idx, N-1);
  return idx;
}

/*
******* airDoneStr()
**
** dinky little utility for generating progress messages of the form
** "  1.9%" or " 35.3%" or  "100.0%"
**
** The message will ALWAYS be six characters, and will ALWAYS be
** preceeded by six backspaces.  Thus, you pass in a string to print
** into, and it had better be allocated for at least 6+6+1 = 13 chars.
*/
char *
airDoneStr(double start, double here, double end, char *str) {
  int perc=0;

  if (str) {
    if (end != start)
      perc = (int)(1000*(here-start)/(end-start) + 0.5);
    else
      perc = 1000;
    if (perc < 0) {
      sprintf(str, "\b\b\b\b\b\b ---- ");
    } else if (perc < 1000) {
      sprintf(str, "\b\b\b\b\b\b% 3d.%d%%", perc/10, perc%10);
    }
    else if (perc == 1000) {
      /* the "% 3d" formatting sequence should have taken care
         of this, but whatever */
      sprintf(str, "\b\b\b\b\b\b100.0%%");
    }
    else {
      sprintf(str, "\b\b\b\b\b\b done.");
    }
  }

  /* what the heck was all this for ? 
  static int len = -1;

  if (-1 == len) {
    len = strlen(str);
  }
  else {
    if (len != strlen(str)) {
      printf("len(\\b\\b\\b\\b\\b\\b% 3d.%d%%) != %d\n", 
              perc/10, perc%10, len);
      exit(1);
    }
  }
  */
  return(str);
}

/*
******** airTime()
**
** returns current time in seconds (with millisecond resolution) as a double
*/
double
airTime() {
#ifdef _WIN32
  return (double)clock()/CLOCKS_PER_SEC;
#else
  struct timeval tv;

  gettimeofday(&tv, NULL);
  return((double)(tv.tv_sec + tv.tv_usec/1000000.0));
#endif
}

void
airBinaryPrintUInt(FILE *file, int digits, unsigned int N) {

  digits = AIR_CLAMP(1, digits, 32);
  for (digits=digits; digits>=1; digits--) {
    fprintf(file, "%c", ((1<<(digits-1)) & N
                         ? '1' : '0'));
  }
}

const char
airTypeStr[AIR_TYPE_MAX+1][AIR_STRLEN_SMALL] = {
  "(unknown)",
  "bool",
  "int",
  "unsigned int",
  "long int",
  "size_t",
  "float",
  "double",
  "char",
  "string",
  "enum",
  "other",
};

const size_t
airTypeSize[AIR_TYPE_MAX+1] = {
  0,
  sizeof(int),
  sizeof(int),
  sizeof(unsigned int),
  sizeof(long int),
  sizeof(size_t),
  sizeof(float),
  sizeof(double),
  sizeof(char),
  sizeof(char*),
  sizeof(int),
  0   /* we don't know anything about type "other" */
};

int
airILoad(void *v, int t) {

  switch(t) {
  case airTypeBool:   return *((int*)v); break;
  case airTypeInt:    return *((int*)v); break;
  case airTypeUInt:   return (int)*((unsigned int*)v); break;
  case airTypeLongInt:return (int)*((long int*)v); break;
  case airTypeSize_t: return (int)*((size_t*)v); break;
  case airTypeFloat:  return (int)*((float*)v); break;
  case airTypeDouble: return (int)*((double*)v); break;
  case airTypeChar:   return *((char*)v); break;
  default: return 0; break;
  }
}

int
airIStore(void *v, int t, int i) {

  switch(t) {
  case airTypeBool:   return (*((int*)v) = !!i); break;
  case airTypeInt:    return (*((int*)v) = i); break;
  case airTypeUInt:   return (int)(*((unsigned int*)v) = i); break;
  case airTypeLongInt:return (int)(*((long int*)v) = i); break;
  case airTypeSize_t: return (int)(*((size_t*)v) = i); break;
  case airTypeFloat:  return (int)(*((float*)v) = (float)(i)); break;
  case airTypeDouble: return (int)(*((double*)v) = (double)(i)); break;
  case airTypeChar:   return (*((char*)v) = (char)(i)); break;
  default: return 0; break;
  }
}

float
airFLoad(void *v, int t) {

  switch(t) {
  case airTypeBool:   return AIR_CAST(float, *((int*)v)); break;
  case airTypeInt:    return AIR_CAST(float, *((int*)v)); break;
  case airTypeUInt:   return AIR_CAST(float, *((unsigned int*)v)); break;
  case airTypeLongInt:return AIR_CAST(float, *((long int*)v)); break;
  case airTypeSize_t: return AIR_CAST(float, *((size_t*)v)); break;
  case airTypeFloat:  return *((float*)v); break;
  case airTypeDouble: return AIR_CAST(float, *((double*)v)); break;
  case airTypeChar:   return *((char*)v); break;
  default: return 0; break;
  }
}

float
airFStore(void *v, int t, float f) {

  switch(t) {
  case airTypeBool:
    return AIR_CAST(float, *((int*)v) = (int)f);
    break;
  case airTypeInt:
    return AIR_CAST(float, *((int*)v) = (int)f);
    break;
  case airTypeUInt:
    return AIR_CAST(float, *((unsigned int*)v) = (unsigned int)f);
    break;
  case airTypeLongInt:
    return AIR_CAST(float, *((long int*)v) = (long int)f);
    break;
  case airTypeSize_t:
    return AIR_CAST(float, *((size_t*)v) = (size_t)f);
    break;
  case airTypeFloat: 
    return (*((float*)v) = f);
    break;
  case airTypeDouble:
    return AIR_CAST(float, (*((double*)v) = (double)f));
    break;
  case airTypeChar:
    return (*((char*)v) = (char)f);
    break;
  default:
    return 0;
    break;
  }
}

double
airDLoad(void *v, int t) {

  switch(t) {
  case airTypeBool:   return AIR_CAST(double,*((int*)v)); break;
  case airTypeInt:    return AIR_CAST(double,*((int*)v)); break;
  case airTypeUInt:   return AIR_CAST(double,*((unsigned int*)v)); break;
  case airTypeLongInt:return AIR_CAST(double,*((long int*)v)); break;
  case airTypeSize_t: return AIR_CAST(double, *((size_t*)v)); break;
  case airTypeFloat:  return AIR_CAST(double,*((float*)v)); break;
  case airTypeDouble: return *((double*)v); break;
  case airTypeChar:   return AIR_CAST(double,*((char*)v)); break;
  default: return 0; break;
  }
}

double
airDStore(void *v, int t, double d) {

  switch(t) {
  case airTypeBool:   return AIR_CAST(double,(*((int*)v) = (int)d)); break;
  case airTypeInt:    return AIR_CAST(double,(*((int*)v) = (int)d)); break;
  case airTypeUInt:   return AIR_CAST(double,(*((unsigned int*)v) = (unsigned int)d)); break;
  case airTypeLongInt:return AIR_CAST(double,(*((long int*)v) = (long int)d)); break;
  case airTypeSize_t: return AIR_CAST(double,(*((size_t*)v) = (size_t)d)); break;
  case airTypeFloat:  return AIR_CAST(double,(*((float*)v) = (float)d)); break;
  case airTypeDouble: return (*((double*)v) = d); break;
  case airTypeChar:   return AIR_CAST(double,(*((char*)v) = (char)d)); break;
  default: return 0; break;
  }
}

/*
******** airEqvSettle()
**
** takes a mapping map[i], i in [0..len-1], and shifts the range of the
** mapping downward so that the range is a contiguous set of uints
** starting at 0.
**
** returns the number of different uints; previous version returned one
** less than this (the highest mapping output value, after the settling)
**
** honestly this doesn't necessarily have anything to do with processing
** equivalence classes, but its an operation that is nice to have in
** those cases
*/
unsigned int
airEqvSettle(unsigned int *map, unsigned int len) {
  unsigned int i, j, count, max, *hit;
  
  max = 0;
  for (i=0; i<len; i++) {
    max = AIR_MAX(max, map[i]);
  }
  hit = (unsigned int *)calloc(1+max, sizeof(unsigned int));
  for (i=0; i<len; i++) {
    hit[map[i]] = 1;
  }
  count = 0;
  for (j=0; j<=max; j++) {
    if (hit[j]) {
      hit[j] = count;
      count += 1;
    }
  }
  for (i=0; i<len; i++) {
    map[i] = hit[map[i]];
  }
  free(hit);
  return count;
}

/*
******** airEqvMap
**
** takes the equivalence pairs in eqvArr, and an array of uints map of 
** length len, and puts in map[i] the uint that class i's value should
** be changed to.  
** 
** based on numerical recipes, C edition, pg. 346
** modifications: 
**  - when resolving ancestors, map to the one with the lower index.
**  - applies settling to resulting map 
**
** returns the number of different class IDs
*/
unsigned int
airEqvMap(airArray *eqvArr, unsigned int *map, unsigned int len) {
  unsigned int *eqv, j, k, t, eqi;

  for (j=0; j<len; j++) {
    map[j] = j;
  }
  eqv = (unsigned int*)(eqvArr->data);
  for (eqi=0; eqi<eqvArr->len; eqi++) {
    j = eqv[0 + 2*eqi];
    k = eqv[1 + 2*eqi];
    while (map[j] != j) {
      j = map[j];
    }
    while (map[k] != k) {
      k = map[k];
    }
    if (j != k) {
      if (j < k) {
        t = j; j = k; k = t;
      }
      map[j] = k;
    }
  }
  for (j=0; j<len; j++) {
    while (map[j] != map[map[j]]) {
      map[j] = map[map[j]];
    }
  }
  return airEqvSettle(map, len);
}

/*
******** airEqvAdd
**
** adds another equivalence class
*/
void
airEqvAdd(airArray *eqvArr, unsigned int j, unsigned int k) {
  unsigned int *eqv, eqi;

  /* HEY: would it speed things up at all to enforce j < k? */
  if (eqvArr->len) {
    /* we have some equivalences, but we're only going to check against
       the last one in an effort to reduce duplicate entries */
    eqv = AIR_CAST(unsigned int*, eqvArr->data);
    eqi = eqvArr->len-1;
    if ( (eqv[0 + 2*eqi] == j && eqv[1 + 2*eqi] == k) ||
         (eqv[0 + 2*eqi] == k && eqv[1 + 2*eqi] == j) ) {
      /* don't add a duplicate */
      return;
    }
  }
  eqi = airArrayLenIncr(eqvArr, 1);
  eqv = AIR_CAST(unsigned int*, eqvArr->data);
  eqv[0 + 2*eqi] = j;
  eqv[1 + 2*eqi] = k;
  return;
}

/* ---- END non-NrrdIO */

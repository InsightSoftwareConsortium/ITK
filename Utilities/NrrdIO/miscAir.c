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
#include "teem32bit.h"
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
** the teem version number and release date.  Originated in version 1.5;
** use of TEEM_VERSION #defines started in 1.9
*/
const char *
airTeemVersion = TEEM_VERSION_STRING;
const char *
airTeemReleaseDate = "12 August 2005";

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
  char *fmt;
  float valF=0;
  double valD=0;
  int ret, isF, isD, cls;
  char *conv=NULL, *p0, *p1, *p2, *p3, *p4, *p5;
  va_list ap;
  
  va_start(ap, _fmt);
  fmt = airStrdup(_fmt);
  
  p0 = strstr(fmt, "%e");
  p1 = strstr(fmt, "%f");
  p2 = strstr(fmt, "%g");
  p3 = strstr(fmt, "%le");
  p4 = strstr(fmt, "%lf");
  p5 = strstr(fmt, "%lg");
  isF = p0 || p1 || p2;
  isD = p3 || p4 || p5;
  if (isF) {
    conv = p0 ? p0 : (p1 ? p1 : p2);
  }
  if (isD) {
    conv = p3 ? p3 : (p4 ? p4 : p5);
  }
  if (isF || isD) {
    if (isF) {
      /* use "double" instead of "float" because var args are _always_
         subject to old-style C type promotions: float promotes to double */
      valF = (float)(va_arg(ap, double));
      cls = airFPClass_f(valF);
    }
    else {
      valD = va_arg(ap, double);
      cls = airFPClass_d(valD);
    }
    switch (cls) {
    case airFP_SNAN:
    case airFP_QNAN:
    case airFP_POS_INF:
    case airFP_NEG_INF:
      if (isF) {
        memcpy(conv, "%s", 2);
      }
      else {
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
      if (isF) {
        ret = PRINT(file, str, fmt, valF);
      }
      else {
        ret = PRINT(file, str, fmt, valD);
      }
      break;
    }
  }
  else {
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



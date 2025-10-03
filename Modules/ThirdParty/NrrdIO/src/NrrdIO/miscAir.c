/*
  NrrdIO: C library for NRRD file IO (with optional compressions)
  Copyright (C) 2009--2025  University of Chicago
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
#include "privateAir.h"
/* timer functions */
#ifdef _WIN32
#  include <io.h>
#  include <fcntl.h>
#  include <time.h>
#else
#  include <sys/time.h>
#endif

/*
******** airTeemVersion
******** airTeemReleaseDone
******** airTeemReleaseDate
**
** updated with each release to contain a string representation of
** the Teem version number and release date.  Originated in version 1.5;
** use of TEEM_VERSION #defines started in 1.9
*/
const char *const airTeemVersion = TEEM_VERSION_STRING;
const int airTeemReleaseDone = AIR_FALSE;
const char *const airTeemReleaseDate = "hopefully 2025";

/*
******** airTeemVersionSprint
**
** uniform way of printing information about the Teem version
*/
void
airTeemVersionSprint(char buff[AIR_STRLEN_LARGE + 1]) {
  snprintf(buff, AIR_STRLEN_LARGE + 1, "Teem version %s, %s%s%s", airTeemVersion,
           airTeemReleaseDone ? "released on " : "", airTeemReleaseDate,
           airTeemReleaseDone ? "" : " (not yet released)");
  return;
}

double
_airSanityHelper(double val) {
  return val * val * val;
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
** encapsulates that idea that "-" OR "-=" is either standard in or standard
** out, and does McRosopht stuff required to make piping work. Handling "-="
** is a convenience for implementing NrrdIoState->declineStdioOnTTY, with
** the semantics (not handled here) that "-=" means "read/write from stdin/
** stdout, even when it IS a terminal". But this is currently only supported
** in full Teem, not the minimal NrrdIO library.
**
** Does no error checking.  If fopen fails, then C' errno and strerror are
** left untouched for the caller to access.
*/
FILE *
airFopen(const char *name, FILE *std, const char *mode) {
  FILE *ret;

  if (!strcmp(name, "-")) {
    ret = std;
#ifdef _WIN32
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
    if (!(stdin == file || stdout == file || stderr == file)) {
      fclose(file);
    }
  }
  return NULL;
}

/*
 ******* airSinglePrintf
 *
 * a complete stand-in for {f|s}printf(), as long as the given format string contains
 * exactly one conversion sequence, and does use any precision modifiers.  The utility of
 * this is to standardize the printing of IEEE 754 special values:
 * NAN (any kind) -> "NaN"
 * POS_INF -> "+inf"
 * NEG_INF -> "-inf"
 * The format string can contain other things besides just the conversion sequence:
 * airSinglePrintf(f, NULL, " (%f)\n", AIR_NAN) will be the same as:
 * fprintf(f, " (%s)\n", "NaN");
 *
 * To get fprintf behavior, pass "str" as NULL
 * to get sprintf bahavior, pass "file" as NULL. AND NOTE THAT THIS DOES USE sprintf
 * and not snprintf because we're not in a position to know what the buffer size is.
 *
 * Finding a complete {f|s|}printf replacement would be great, but finding one compatible
 * with our LGPL+linking exception is hard.
 */
int
airSinglePrintf(FILE *file, char *str, const char *_fmt, ...) {
  char *fmt, buff[AIR_STRLEN_LARGE + 1];
  double val = 0, gVal, fVal;
  int ret, isF, isD, cls;
  char *conv = NULL, *p0, *p1, *p2, *p3, *p4, *p5;
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
     "is 2-character conv. seq., or "is 3-character conv. seq." */
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
    case airFP_NAN:
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
#define PRINT(F, S, C, V) ((F) ? fprintf((F), (C), (V)) : sprintf((S), (C), (V)))
    switch (cls) {
    case airFP_NAN:
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

/*
******** airSprintSize_t
**
** sprints a single size_t to a given string, side-stepping
** non-standardized format specifier confusion with printf
*/
char *
airSprintSize_t(char _str[AIR_STRLEN_SMALL + 1], size_t val) {
  char str[AIR_STRLEN_SMALL + 1];
  unsigned int si;

  if (!_str) {
    return NULL;
  }
  si = AIR_STRLEN_SMALL;
  str[si] = '\0';
  do {
    str[--si] = AIR_CAST(char, (val % 10) + '0');
    val /= 10;
  } while (val);
  strcpy(_str, str + si);
  return _str;
}

/*
******** airSprintPtrdiff_t
**
** sprints a single ptrdiff_t to a given string, side-stepping
** non-standardized format specifier confusion with printf
*/
char *
airSprintPtrdiff_t(char _str[AIR_STRLEN_SMALL + 1], ptrdiff_t val) {
  char str[AIR_STRLEN_SMALL + 1];
  unsigned int si;
  int sign;

  if (!_str) {
    return NULL;
  }
  si = AIR_STRLEN_SMALL;
  str[si] = '\0';
  sign = (val < 0 ? -1 : 1);
  do {
    ptrdiff_t dig;
    dig = val % 10;
    str[--si] = AIR_CAST(char, dig > 0 ? dig + '0' : -dig + '0');
    val /= 10;
  } while (val);
  if (-1 == sign) {
    str[--si] = '-';
  }
  strcpy(_str, str + si);
  return _str;
}

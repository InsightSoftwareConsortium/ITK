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
#include "privateAir.h"
/* timer functions */
#ifdef _WIN32
#  include <io.h>
#  include <fcntl.h>
#  include <time.h>
#  define WIN32_LEAN_AND_MEAN
#  include <windows.h>
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
const char *const airTeemReleaseDate = "early 2026";

/*
******** airTeemVersionSprint
**
** uniform way of printing information about the Teem version
*/
void
airTeemVersionSprint(char buff[AIR_STRLEN_SMALL + 1]) {
  snprintf(buff, AIR_STRLEN_SMALL + 1, "Teem version %s, %s%s%s", airTeemVersion,
           airTeemReleaseDone ? "released on " : "", airTeemReleaseDate,
           airTeemReleaseDone ? "" : " (not yet released)");
  return;
}

double
air__SanityHelper(double val) {
  /* not a static symbol but is "private"; used by sane.c:airSanity() */
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
AIR_MOPPER(airSetNull)

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
AIR_MOPPER(airFclose)

/* helper function for airSinglePrintf:
   shift characters between clo and chi (inclusive) up by 2 */
static void
shift2(char *clo, char *chi) {
  while (chi >= clo) {
    *(chi + 2) = *chi;
    chi--;
  }
  return;
}

/*
 ******* airSinglePrintf
 *
 * a complete stand-in for {f|s}printf(), as long as the given format string contains
 * exactly one conversion sequence, and does use any precision modifiers.  The utility of
 * this is to standardize the printing of IEEE 754 special values:
 * NAN (any kind) -> "NaN"
 * POS_INF -> "inf"   <-- TeemV2 change: was "+inf"
 * NEG_INF -> "-inf"
 * The format string can contain other things besides just the conversion sequence:
 * airSinglePrintf(f, NULL, "This %f isn't a number\n", AIR_NAN) will be the same as:
 * fprintf(f, "This %s isn't a number\n", "NaN");
 *
 * To get fprintf behavior, pass `str` as NULL
 * to get sprintf bahavior, pass `file` as NULL
 *       and pass the allocated size of `str` as `strSize`
 *       so that we can use snprintf instead of sprintf
 *
 * Finding a complete {f|s|}printf replacement would be great, but finding one compatible
 * with our LGPL+linking exception licensing is hard.
 *
 * 2025 scrutiny: The apparent intent of the old code was that with conversion sequence
 * "%g" (in the case of printing a single-precision float) or "%lg" (for a double), then
 * the code used a different conversion sequence when necessary to ensure that upon
 * sscanf parsing, would get back to the original value. HOWEVER: some of the code
 * comments acknowledged that after going through var-args the float becomes a double
 * (so `isF`,`isD` variable names risk being delusional). But the BUG was that the
 * strategy for ensuring accuracy was broken: 0.10000000000000002 (which parses to the
 * next double after the one parsed from 0.1) would also be printed as "0.1".
 * Also, NaN values were printed as " NaN" (with needless leading space)
 *
 * So, for the TeemV2 re-write: now NaN is printed as "NaN", and now "%g" means: "I'm
 * sending in a float (and I know you'll get it as a double), but can you print it with a
 * concise string that parses back to the same float". "%lg" means the same for a double.
 * Round-trip accuracy is ensured by internally changing "%g" to "%.9g" or from "%lg" to
 * "%.17g" when sscanf shows it to be necessary.
 */
int
airSinglePrintf(FILE *file, char *str, size_t strSize, const char *_fmt, ...) {
  int ret, isF, isD;
  const char *_p0, *_p1, *_p2, *_p3, *_p4, *_p5;
  va_list ap;

  va_start(ap, _fmt);

  /* look for unmodified floating point conversion sequences */
  _p0 = strstr(_fmt, "%e");
  _p1 = strstr(_fmt, "%f");
  _p2 = strstr(_fmt, "%g");
  _p3 = strstr(_fmt, "%le");
  _p4 = strstr(_fmt, "%lf");
  _p5 = strstr(_fmt, "%lg");
  isF = _p0 || _p1 || _p2;
  isD = _p3 || _p4 || _p5;
  /* the code here says `isF` and `isD` as if it means "is float" or "is double".
     It would more accurately be `is2` or `is3`, as in, "is 2-character conv. seq., or
     "is 3-character conv. seq."
     (but for TeemV2 we run with that type implication for "%g" and "%lg") */
  if (isF || isD) {
#define PRINT(F, S, C, V)                                                               \
  ((F) /* */                                                                            \
     ? fprintf((F), (C), (V))                                                           \
     : snprintf((S), strSize, (C), (V)))
    double val0;
    const char *_conv;
    size_t fmtSize;
    char *fmt, *conv;
    int cls;
    if (isF) {
      _conv = _p0 ? _p0 : (_p1 ? _p1 : _p2);
    } else {
      _conv = _p3 ? _p3 : (_p4 ? _p4 : _p5);
    }
    /* allocate our own copy of _fmt, with length increase by the most we might need to
       add with |%g|=2 --> |%.9g|=4 or |%lg|=3 --> |%.17g|=5, namely 2 */
    fmtSize = strlen(_fmt) + 2 + 1;
    fmt = AIR_CALLOC(fmtSize, char);
    assert(fmt);
    airStrcpy(fmt, fmtSize, _fmt);
    conv = fmt + (_conv - _fmt); /* conv points into fmt like _conv points into _fmt */
    /* must use "double" here to consume var-arg instead of "float" because var args are
       _always_ subject to old-style C type promotions: float promotes to double */
    val0 = va_arg(ap, double);
    /* printf("!%s: hello val0 = %.17g\n", __func__, val0); */
    cls = airFPClass_d(val0);
    if (airFP_NAN == cls || airFP_POS_INF == cls || airFP_NEG_INF == cls) {
      /* we do not need to rebuild the `fmt` string in parts, just sneakily tweak it */
      if (isF) {
        /* started with a 2-char conversion sequence, use a different one */
        memcpy(conv, "%s", 2);
      } else {
        /* isD: we started with a 3-char conversion sequence, replaces it with
           another 3-char sequence for the special value string (3 or 4 chars).
           {f,s}printf would print the string correctly with plain `%s` but we're
           just filling out three characters of a conversion sequence to avoid having to
           change any other characters in `fmt`. */
        memcpy(conv, airFP_NEG_INF == cls ? "%4s" : "%3s", 3);
      }
      /* now print with the string conversion sequence just set */
      switch (cls) {
      case airFP_NAN:
        ret = PRINT(file, str, fmt, "NaN");
        break;
      case airFP_POS_INF:
        ret = PRINT(file, str, fmt, "inf");
        break;
      case airFP_NEG_INF:
        ret = PRINT(file, str, fmt, "-inf");
        break;
      }
    } else { /* not IEEE754 special value */
      if (_p2 || _p5) {
        /* 2025 Prof GLK revisiting this post-doc GLK code (svn r2879 2005-09-21)
        and comments with better floating-point experience and understanding:
           / * got "%g" or "%lg", see if it would be better to use "%f" * /
             * sprintf(buff, "%f", val);
             * sscanf(buff, "%lf", &fVal);
             * sprintf(buff, "%g", val);
             * sscanf(buff, "%lf", &gVal);
             * if (fVal != gVal) ...
             * / * using %g/%lg lost precision, use %f/%lf instead * /
        NO, and NO.
        NO#1: The relevant test for the insufficient precision of %g/%lg for the round-
           trip val-sprintf->buff-sscanf->val is whether val == gVal, NOT fVal != gVal.
        NO#2: %f/%lf do *not* ensure round-trip accuracy. The r2879 commit message
           "fixed bug in handling of loss of precision with %g/%lg" was ignorant.
        So what to do: we rescue+extend the apparent intent of this code and implement:
         - if got %g, assume value came from a (single-precision) float, and find a
           string that ensures round-trip accuracy for a float
         - if got %lg, assume value came as a double, and find a string that ensures
           round-trip accuracy for a double
         - (the `isF` and `isD` variable names indicate the original code intent)
        Teem code using this function was updated to work with this behavior.
        See new test program teem/src/air/test/asprint */
        char buff[AIR_STRLEN_SMALL + 1];
        size_t buffSize = AIR_STRLEN_SMALL + 1;
        if (_p2) /* got "%g" intended for float */ {
          float fltval1, fltval0 = (float)val0;
          snprintf(buff, buffSize, "%g", fltval0);
          sscanf(buff, "%f", &fltval1);
          if (fltval1 != fltval0) {
            /* "%g" by itself did NOT gave round-trip single-precision accuracy;
               assemble new format string with |%g|=2 --> |%.9g|=4 */
            shift2(conv + 2 /* first char after %g */,
                   fmt + strlen(fmt) - 1 /* last char in fmt */);
            memcpy(conv, "%.9g", 4);
          }
          /* else "%g" gave round-trip single-precision accuracy; use `fmt` as is */
        } else /* _p5: got "%lg% intended for double */ {
          double val1;
          snprintf(buff, buffSize, "%g", val0);
          sscanf(buff, "%lf", &val1);
          if (val1 != val0) { /* SORRY COPY PASTA */
            shift2(conv + 3 /* first char after %lg */,
                   fmt + strlen(fmt) - 1 /* last char in `fmt` */);
            memcpy(conv, "%.17g", 5);
          }
          /* else "%lg" gave round-trip double-precision accuracy; use `fmt` as is */
        }
        /* printf("!%s: fmt=|%s|\n", __func__, fmt); */
        /* end of possible fiddling `fmt` to ensure roung-trip accuracy */
      } /* else `isF || isD` but not _p2="%g" and not _p5="%lg": got non-special value
           to print with _p0="%e", _p1="%f", _p3="%le", or _p4="%lf"
           ==> can use `fmt` as is */
      ret = PRINT(file, str, fmt, val0);
    } /* end of else !special */
    free(fmt);
#undef PRINT
  } else {
    /* `!isF && !isD`: conversion sequence is either for float or double but something
       more specific like "%.17g" (which NOTE does not get the above special treatment of
       IEEE754 special values!s), or, it isn't for any kind of floating point value.
       We can use the given `_fmt` as is (no local allocation of `fmt`) */
    ret = file ? vfprintf(file, _fmt, ap) : vsnprintf(str, strSize, _fmt, ap);
  }

  va_end(ap);
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
  airStrcpy(_str, AIR_STRLEN_SMALL + 1, str + si);
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
  airStrcpy(_str, AIR_STRLEN_SMALL + 1, str + si);
  return _str;
}

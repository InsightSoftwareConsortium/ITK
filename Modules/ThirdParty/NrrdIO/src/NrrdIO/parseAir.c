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

/* clang-format off */
static const char *
_airBoolStr[] = {
  "(unknown bool)",
  "false",
  "true"
};

static const char *
_airBoolDesc[] = {
  "unknown boolean",
  "false",
  "true"
};

static const int
_airBoolVal[] = {
  -1,
  AIR_FALSE,
  AIR_TRUE
};

static const char *
_airBoolStrEqv[] = {
  "0", "no", "n", "false", "f", "off", "nope",
  "1", "yes", "y", "true", "t", "on", "yea",
  ""
};

static const int
_airBoolValEqv[] = {
  AIR_FALSE, AIR_FALSE, AIR_FALSE, AIR_FALSE, AIR_FALSE, AIR_FALSE, AIR_FALSE,
  AIR_TRUE, AIR_TRUE, AIR_TRUE, AIR_TRUE, AIR_TRUE, AIR_TRUE, AIR_TRUE
};

static const airEnum
_airBool = {
  "boolean",
  2,
  _airBoolStr,
  _airBoolVal,
  _airBoolDesc,
  _airBoolStrEqv,
  _airBoolValEqv,
  AIR_FALSE
};

const airEnum *const
airBool = &_airBool;
/* clang-format on */

double
airAtod(const char *str) {
  double val = 0.0;

  airSingleSscanf(str, "%lf", &val);
  return val;
}

/* NOTE: see info n air.h about TeemV2 changes to all things airType and airParse */

int
airSingleSscanf(const char *str, const char *fmt, void *ptr) {
  char *tmp;
  double val;
  int ret;

  if (!strcmp(fmt, "%e") || !strcmp(fmt, "%f") || !strcmp(fmt, "%g")
      || !strcmp(fmt, "%le") || !strcmp(fmt, "%lf") || !strcmp(fmt, "%lg")) {
    tmp = airStrdup(str);
    if (!tmp) {
      return 0;
    }
    airToLower(tmp);
    if (strstr(tmp, "nan")) {
      val = (double)AIR_NAN;
    } else if (strstr(tmp, "-inf")) {
      val = (double)AIR_NEG_INF;
    } else if (strstr(tmp, "inf")) {
      val = (double)AIR_POS_INF;
    } else {
      /* nothing special matched; pass it off to sscanf() */
      /* (save setlocale here) */
      ret = sscanf(str, fmt, ptr);
      /* (return setlocale here) */
      free(tmp);
      return ret;
    }
    /* else we matched "nan", "-inf", or "inf", and set val accordingly */
    if (!strncmp(fmt, "%l", 2)) {
      /* we were given a double pointer */
      *((double *)(ptr)) = val;
    } else {
      /* we were given a float pointer */
      *((float *)(ptr)) = AIR_FLOAT(val);
    }
    free(tmp);
    return 1;
  } else if (!strcmp(fmt, "%z")) {
    /* its a size_t */
    size_t tsz = 0;        /* tmp size_t */
    const char *chh = str; /* char here */
    while (chh) {
      int dig;
      dig = AIR_INT(*chh - '0');
      if (AIR_IN_CL(0, dig, 9)) {
        tsz = 10 * tsz + AIR_CAST(size_t, dig);
      } else {
        break;
      }
      chh++;
    }
    *((size_t *)(ptr)) = tsz;
    return 1;
  } else {
    /* not a float, double, or size_t, let sscanf handle it */
    return sscanf(str, fmt, ptr);
  }
}

unsigned int
airParseStrB(int *out, const char *_s, const char *ct, unsigned int n) {
  unsigned int i;
  char *tmp, *s, *last;

  /* if we got NULL, there's nothing to do */
  if (!(out && _s && ct)) return 0;

  /* copy the input so that we don't change it */
  s = airStrdup(_s);

  /* keep calling airStrtok() until we have everything */
  for (i = 0; i < n; i++) {
    tmp = airStrtok(i ? NULL : s, ct, &last);
    if (!tmp) {
      free(s);
      return i;
    }
    out[i] = airEnumVal(airBool, tmp);
    if (airEnumUnknown(airBool) == out[i]) {
      free(s);
      return i;
    }
  }
  free(s);
  return n;
}

#define _PARSE_STR_ARGS(type) type *out, const char *_s, const char *ct, unsigned int n
#define _PARSE_STR_BODY(format)                                                         \
  unsigned int i;                                                                       \
  char *tmp, *s, *last;                                                                 \
                                                                                        \
  /* if we got NULL, there's nothing to do */                                           \
  if (!(out && _s && ct)) return 0;                                                     \
                                                                                        \
  /* copy the input so that we don't change it */                                       \
  s = airStrdup(_s);                                                                    \
                                                                                        \
  /* keep calling airStrtok() until we have everything */                               \
  for (i = 0; i < n; i++) {                                                             \
    tmp = airStrtok(i ? NULL : s, ct, &last);                                           \
    if (!tmp) {                                                                         \
      free(s);                                                                          \
      return i;                                                                         \
    }                                                                                   \
    if (1 != airSingleSscanf(tmp, format, out + i)) {                                   \
      free(s);                                                                          \
      return i;                                                                         \
    }                                                                                   \
  }                                                                                     \
  free(s);                                                                              \
  return n;

/*
******* airParse*()
*
* parse `n` shorts, ints, floats, doubles, or single chars, from some given string `s`;
* the `n` of them are delimited by characters in `ct`; put the results in `out`.
*
* Returns the number of things succesfully parsed- should be `n`; there's been an error
* if return is < `n`.
*
* This uses air's thread-safe strtok() replacement: airStrtok()
*/
unsigned int
airParseStrH(_PARSE_STR_ARGS(short)) {
  _PARSE_STR_BODY("%hd")
}

unsigned int
airParseStrUH(_PARSE_STR_ARGS(unsigned short)) {
  _PARSE_STR_BODY("%hu")
}

unsigned int
airParseStrI(_PARSE_STR_ARGS(int)) {
  _PARSE_STR_BODY("%d")
}

unsigned int
airParseStrUI(_PARSE_STR_ARGS(unsigned int)) {
  _PARSE_STR_BODY("%u")
}

unsigned int
airParseStrL(_PARSE_STR_ARGS(long int)) {
  _PARSE_STR_BODY("%ld")
}

unsigned int
airParseStrUL(_PARSE_STR_ARGS(unsigned long int)) {
  _PARSE_STR_BODY("%lu")
}

unsigned int
airParseStrZ(_PARSE_STR_ARGS(size_t)) {
  _PARSE_STR_BODY("%z")
}

unsigned int
airParseStrF(_PARSE_STR_ARGS(float)) {
  _PARSE_STR_BODY("%f")
}

unsigned int
airParseStrD(_PARSE_STR_ARGS(double)) {
  _PARSE_STR_BODY("%lf")
}

unsigned int
airParseStrC(char *out, const char *_s, const char *ct, unsigned int n) {
  unsigned int i;
  char *tmp, *s, *last;

  /* if we got NULL, there's nothing to do */
  if (!(out && _s && ct)) return 0;

  /* copy the input so that we don't change it */
  s = airStrdup(_s);

  /* keep calling airStrtok() until we have everything */
  for (i = 0; i < n; i++) {
    tmp = airStrtok(i ? NULL : s, ct, &last);
    if (!tmp) {
      free(s);
      return i;
    }
    out[i] = tmp[0];
  }
  free(s);
  return n;
}

/* for TeemV2: "greedy" was a dumb idea, and is gone. It used to be passed in as the last
 * var-args argument. In the case of n==1, if greedy than the whole string was returned,
 * else (!greedy) airStrtok() was called */
unsigned int
airParseStrS(char **out, const char *_s, const char *ct, unsigned int n) {
  unsigned int i;
  char *tmp, *s, *last;
  airArray *mop;

  /* if we got NULL, there's nothing to do */
  if (!(out && _s && ct)) return 0;

  mop = airMopNew();
  /* copy the input so that we don't change it */
  s = airStrdup(_s);
  airMopMem(mop, &s, airMopAlways);

  /* keep calling airStrtok() until we have everything */
  for (i = 0; i < n; i++) {
    if (n > 1) {
      tmp = airStrtok(i ? NULL : s, ct, &last);
    } else {
      tmp = s;
    }
    if (!tmp) {
      airMopError(mop);
      return i;
    }
    out[i] = airStrdup(tmp);
    if (!out[i]) {
      airMopError(mop);
      return i;
    }
    airMopMem(mop, out + i, airMopOnError);
  }
  airMopOkay(mop);
  return n;
}

unsigned int
airParseStrE(int *out,
             const char *_s,
             const char *ct,
             unsigned int n,
             const airEnum *enm) {
  unsigned int i;
  char *tmp, *s, *last;
  airArray *mop;

  /* if we got NULL, there's nothing to do */
  if (!(out && _s && ct)) {
    return 0;
  }

  mop = airMopNew();
  /* copy the input so that we don't change it */
  s = airStrdup(_s);
  airMopMem(mop, &s, airMopAlways);

  if (1 == n) {
    /* Because it should be permissible to have spaces in what is
       intended to be only a single string from an airEnum, we treat
       1==n differently, and do NOT use airStrtok to tokenize the
       input string s into spaces.  We check the whole s string */
    out[0] = airEnumVal(enm, s);
    if (airEnumUnknown(enm) == out[0]) {
      airMopError(mop);
      return 0;
    }
  } else {
    /* keep calling airStrtok() until we have everything */
    for (i = 0; i < n; i++) {
      tmp = airStrtok(i ? NULL : s, ct, &last);
      if (!tmp) {
        airMopError(mop);
        return i;
      }
      out[i] = airEnumVal(enm, tmp);
      if (airEnumUnknown(enm) == out[i]
          /* getting the unknown value is not a parse failure if the
             string was actually the string for the unknown value! */
          && strcmp(tmp, enm->str[0])) {
        airMopError(mop);
        return i;
      }
    }
  }
  airMopOkay(mop);
  return n;
}

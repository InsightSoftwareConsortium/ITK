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

/* clang-format off */
static const char *_airFPClass_Str[AIR_FP_MAX+1] = {
  "(unknown_class)",
  "nan",
  "pinf",
  "ninf",
  "pnorm",
  "nnorm",
  "pdenorm",
  "ndenorm",
  "pzero",
  "nzero",
};

static const char *_airFPClass_Desc[AIR_FP_MAX+1] = {
  "unknown_class",
  "(quiet) nan",
  "positive infinity",
  "negative infinity",
  "positive normalized",
  "negative normalized",
  "positive denormalized",
  "negative denormalized",
  "positive zero",
  "negative zero",
};

static const char *_airFPClass_StrEqv[] = {
  "nan", "qnan",
  "pinf", "posinf", "+inf", "inf",
  "ninf", "neginf", "-inf",
  "pnorm", "posnorm", "+norm", "norm",
  "nnorm", "negnorm", "-norm",
  "pdenorm", "posdenorm", "+denorm", "denorm",
  "ndenorm", "negdenorm", "-denorm",
  "pzero", "+0", "+zero", "zero", "0",
  "nzero", "-0", "-zero",
  "",
};

static const int _airFPClass_ValEqv[] = {
  airFP_NAN, airFP_NAN,
  airFP_POS_INF, airFP_POS_INF, airFP_POS_INF, airFP_POS_INF,
  airFP_NEG_INF, airFP_NEG_INF, airFP_NEG_INF,
  airFP_POS_NORM, airFP_POS_NORM, airFP_POS_NORM, airFP_POS_NORM,
  airFP_NEG_NORM, airFP_NEG_NORM, airFP_NEG_NORM,
  airFP_POS_DENORM, airFP_POS_DENORM, airFP_POS_DENORM, airFP_POS_DENORM,
  airFP_NEG_DENORM, airFP_NEG_DENORM, airFP_NEG_DENORM,
  airFP_POS_ZERO, airFP_POS_ZERO, airFP_POS_ZERO, airFP_POS_ZERO, airFP_POS_ZERO,
  airFP_NEG_ZERO, airFP_NEG_ZERO, airFP_NEG_ZERO,
};

static const airEnum _airFPClass_ae = {"FP_class",
                                        AIR_FP_MAX,
                                        _airFPClass_Str,
                                        NULL,
                                        _airFPClass_Desc,
                                        _airFPClass_StrEqv,
                                        _airFPClass_ValEqv,
                                        AIR_FALSE};
const airEnum *const airFPClass_ae = &_airFPClass_ae;
/* clang-format on */

/*
** all this is based on a reading of
** Hennessy + Patterson "Computer Architecture, A Quantitative Approach"
** pages A-13 - A-17
**
** and some assorted web pages, such as:
** http://en.wikipedia.org/wiki/NaN#Encoding
** which explains what Teem calls qnanhibit, and
** http://grouper.ieee.org/groups/754/email/msg04192.html
** which includes some discussion on signal-vs-quiet nan
*/

/*
** The hex numbers in braces are examples of C's "initial member of a union"
** aggregate initialization.
*/

/*
With TeemV2, GLK decided to drop configuration-time learning of, and compile-time
handling of, "QNaNHiBit": the most significant bit (MSB) of the fraction bitfield in a
quiet (versus signalling) NaN.

In ~2000 when this code was written, if you generated a NaN from scratch via floating
point (FP) operations (e.g. create an inf by overflowing multiplication, and then divide
inf by itself), then SGI IRIX machines would set to the MSB fraction bit to 0, rather
than the 1 that other machines used. With the assumption that these operations should
have created what should be a quiet NaN, GLK interpreted this as a platform dependence in
how a quiet NaN with the same bit pattern should be created at compile time. This
motivated handling variable QNaNHiBit at configuration time, and the associated
complication in the code below.

Now, the world now seems to agree that QNaNHiBit should be 1 (and the
http://en.wikipedia.org/wiki/NaN#Encoding URL, noted above in the first iterations of
754.c and which has happily remained valid over 25 years, now documents this). For
whatever tiny extant fraction of the world wants QNaNHiBit to be 0, they may risk
floating point signal handlers being triggered by the NaN generated here at compile
time. Hopefully they have the wherewithal to disable those signal handlers for that
circumstance.

With the simplification of how QNaN is handled, the decision was also made to make
"NaN" in Teem code refer to a quiet NaN, with no pretense of generating signalling
NaNs at compile-time.  Teem has never invoked an FP signal handler, and has no reason
to start now.  So airFloatQNaN turned into airFloatQNaN and airFloatSNaN was dropped.
*/
/* #if TEEM_QNANHIBIT == 1 ... (no more) */
/* const unsigned int airMyQNaNHiBit = 1; (no more) */
#define _QNANHIBIT 1 /* just for this file */
const airFloat airFloatNaN = {0x7fffffff};

const airFloat airFloatPosInf = {0x7f800000};
const airFloat airFloatNegInf = {0xff800000}; /* why does solaris whine? */

/*
** these shouldn't be needed, but here they are if need be:

in this file:
const airFloat airFloatMax = {0x7f7fffff};
const airFloat airFloatMin = {0x00800000};
const airDouble airDoubleMax = {AIR_ULLONG(0x7fefffffffffffff)};
const airDouble airDoubleMin = {AIR_ULLONG(0x0010000000000000)};

in air.h:
extern air_export const airFloat airFloatMax;
extern air_export const airFloat airFloatMin;
extern air_export const airDouble airDoubleMax;
extern air_export const airDouble airDoubleMin;
#define AIR_FLT_MIN (airFloatMin.f)
#define AIR_FLT_MAX (airFloatMax.f)
#define AIR_DBL_MIN (airDoubleMin.d)
#define AIR_DBL_MAX (airDoubleMax.d)
*/

#define PARTSHIFT_F(sign, expo, mant)                                                   \
  (((sign & 1u) << (8 + 23)) |        /* */                                             \
   ((expo & ((1u << 8) - 1)) << 23) | /* */                                             \
   (mant & ((1u << 23) - 1)))

#define PARTSHIFT_D(sign, expo, mant0, mant1)                                           \
  (((sign & AIR_ULLONG(1)) << (11 + 52)) |         /* */                                \
   ((expo & ((AIR_ULLONG(1) << 11) - 1)) << 52) |  /* */                                \
   ((mant0 & ((AIR_ULLONG(1) << 20) - 1)) << 32) | /* */                                \
   (mant1 & ((AIR_ULLONG(1) << 32) - 1)))

float
airFPPartsToVal_f(unsigned int sign, unsigned int expo, unsigned int mant) {
  airFloat af;

  af.i = PARTSHIFT_F(sign, expo, mant);
  return af.f;
}

double
airFPPartsToVal_d(unsigned int sign,
                  unsigned int expo,
                  unsigned int mant0,
                  unsigned int mant1) {
  airDouble ad;

  ad.i = PARTSHIFT_D(sign, expo, mant0, mant1);
  return ad.d;
}

void
airFPValToParts_f(unsigned int *signP, unsigned int *expoP, unsigned int *mantP,
                  float v) {
  airFloat af;
  unsigned int ui;

  af.f = v;
  ui = af.i;
  *mantP = ui & ((1u << 23) - 1);
  ui >>= 23;
  *expoP = ui & ((1u << 8) - 1);
  ui >>= 8;
  *signP = ui & 1u;
}

void
airFPValToParts_d(unsigned int *signP, unsigned int *expoP, unsigned int *mant0P,
                  unsigned int *mant1P, double v) {
  airDouble ad;
  airULLong ui;

  ad.d = v;
  ui = ad.i;
  *mant1P = AIR_UINT(ui & ((AIR_ULLONG(1) << 32) - 1));
  ui >>= 32;
  *mant0P = AIR_UINT(ui & ((AIR_ULLONG(1) << 20) - 1));
  ui >>= 20;
  *expoP = AIR_UINT(ui & ((AIR_ULLONG(1) << 11) - 1));
  ui >>= 11;
  *signP = AIR_UINT(ui & AIR_ULLONG(1));
}

/*
******** airFPGen_f()
**
** generates a floating point value which is a member of the given class
*/
float
airFPGen_f(int cls) {
  airFloat af;
  unsigned int sign, expo, mant;

#define SET_SEM(ss, ee, mm)                                                             \
  sign = (ss);                                                                          \
  expo = (ee);                                                                          \
  mant = (mm)

  switch (cls) {
  case airFP_NAN:
    /* (no separate handling of signalling NaN) */
    SET_SEM(0, 0xff, (_QNANHIBIT << 22) | 0x3fffff);
    break;
  case airFP_POS_INF:
    SET_SEM(0, 0xff, 0);
    break;
  case airFP_NEG_INF:
    SET_SEM(1, 0xff, 0);
    break;
  case airFP_POS_NORM:
    /* exp: anything non-zero but < 0xff, mant: anything */
    SET_SEM(0, 0x80, 0x7ff000);
    break;
  case airFP_NEG_NORM:
    /* exp: anything non-zero but < 0xff, mant: anything */
    SET_SEM(1, 0x80, 0x7ff000);
    break;
  case airFP_POS_DENORM:
    /* mant: anything non-zero */
    SET_SEM(0, 0, 0xff);
    break;
  case airFP_NEG_DENORM:
    /* mant: anything non-zero */
    SET_SEM(1, 0, 0xff);
    break;
  case airFP_NEG_ZERO:
    SET_SEM(1, 0, 0);
    break;
  case airFP_POS_ZERO:
  default:
    SET_SEM(0, 0, 0);
    break;
  }
#undef SET_SEM
  af.i = PARTSHIFT_F(sign, expo, mant);
  /* printf("%s(0x%x, 0x%x, 0x%x) = 0x%x -> %.9g\n", __func__, sign, expo, mant, af.i,
         af.f); */
  return af.f;
}

/*
******** airFPGen_d()
**
** generates a floating point value which is a member of the given class
*/
double
airFPGen_d(int cls) {
  airDouble ad;
  unsigned int sign, expo, mant0, mant1;

#define SET_SEM(ss, ee, m0, m1)                                                         \
  sign = (ss);                                                                          \
  expo = (ee);                                                                          \
  mant0 = (m0);                                                                         \
  mant1 = (m1)

  switch (cls) {
  case airFP_NAN:
    /* (no separate handling of signalling NaN) */
    /* sgn: anything, mant anything non-zero with high bit _QNANHIBIT */
    SET_SEM(0, 0x7ff, (_QNANHIBIT << 19) | 0x7ffff, 0xffffffff);
    break;
  case airFP_POS_INF:
    SET_SEM(0, 0x7ff, 0, 0);
    break;
  case airFP_NEG_INF:
    SET_SEM(1, 0x7ff, 0, 0);
    break;
  case airFP_POS_NORM:
    /* exp: anything non-zero but < 0xff, mant: anything */
    SET_SEM(0, 0x400, 0x0ff00, 0);
    break;
  case airFP_NEG_NORM:
    /* exp: anything non-zero but < 0xff, mant: anything */
    SET_SEM(1, 0x400, 0x0ff00, 0);
    break;
  case airFP_POS_DENORM:
    /* mant: anything non-zero */
    SET_SEM(0, 0, 0xff, 0);
    break;
  case airFP_NEG_DENORM:
    /* mant: anything non-zero */
    SET_SEM(1, 0, 0xff, 0);
    break;
  case airFP_NEG_ZERO:
    SET_SEM(1, 0, 0, 0);
    break;
  case airFP_POS_ZERO:
  default:
    SET_SEM(0, 0, 0, 0);
    break;
  }
#undef SET_SEM
  ad.i = PARTSHIFT_D(sign, expo, mant0, mant1);
  return ad.d;
}

static int
wutClass(unsigned int index, int expoMax) {
  int ret = airFP_Unknown;
  switch (index) {
  case 0:
    /* all fields are zero */
    ret = airFP_POS_ZERO;
    break;
  case 1:
    /* only mantissa is non-zero */
    ret = airFP_POS_DENORM;
    break;
  case 2:
    /* only exponent field is non-zero */
    if (expoMax) {
      ret = airFP_POS_INF;
    } else {
      ret = airFP_POS_NORM;
    }
    break;
  case 3:
    /* exponent and mantissa fields are non-zero */
    if (expoMax) {
      /* we don't distinguish (any longer) between qnan and snan */
      ret = airFP_NAN;
    } else {
      ret = airFP_POS_NORM;
    }
    break;
  case 4:
    /* only sign field is non-zero */
    ret = airFP_NEG_ZERO;
    break;
  case 5:
    /* sign and mantissa fields are non-zero */
    ret = airFP_NEG_DENORM;
    break;
  case 6:
    /* sign and exponent fields are non-zero */
    if (expoMax) {
      ret = airFP_NEG_INF;
    } else {
      ret = airFP_NEG_NORM;
    }
    break;
  case 7:
    /* all fields are non-zero */
    if (expoMax) {
      /* we don't distinguish (any longer) between qnan and snan */
      ret = airFP_NAN;
    } else {
      ret = airFP_NEG_NORM;
    }
    break;
  }
  return ret;
}
/*
******** airFPClass_f()
**
** given a floating point number, tells which class its in
*/
int
airFPClass_f(float val) {
  unsigned int sign, expo, mant, indexv;

  airFPValToParts_f(&sign, &expo, &mant, val);
  /* "!" produces an int: https://en.cppreference.com/w/c/language/operator_logical */
  indexv = (AIR_UINT(!!sign) << 2) | (AIR_UINT(!!expo) << 1) | AIR_UINT(!!mant);
  return wutClass(indexv, 0xff == expo);
}

/*
******** airFPClass_d()
**
** given a double, tells which class its in
*/
int
airFPClass_d(double val) {
  unsigned int sign, expo, mant0, mant1, indexv;

  airFPValToParts_d(&sign, &expo, &mant0, &mant1, val);
  indexv = (AIR_UINT(!!sign) << 2) | (AIR_UINT(!!expo) << 1)
         | (AIR_UINT(!!mant0) || AIR_UINT(!!mant1));
  return wutClass(indexv, 0x7ff == expo);
}

/*
******** airIsNaN()
**
** returns 1 if input is either kind of NaN, 0 otherwise.  It is okay
** to only have a double version of this function, as opposed to
** having one for float and one for double, because Section 6.2 of the
** 754 spec tells us that that NaN is to be preserved across precision
** changes (and airSanity() explicitly checks for this).
*/
int
airIsNaN(double g) {
  unsigned int sign, expo, mant;

  airFPValToParts_f(&sign, &expo, &mant, AIR_FLOAT(g));
  AIR_UNUSED(sign);
  return (0xff == expo && mant);
}

/*
******** airIsInf_f(), airIsInf_d()
**
** returns 1 if input is positive infinity,
** -1 if negative infinity,
** or 0 otherwise (including NaN)
**
** thus the non-zero-ness of the return is an easy way to do a
** boolean check of whether the value is infinite
*/
int
airIsInf_f(float f) {
  int c, ret;

  c = airFPClass_f(f);
  if (airFP_POS_INF == c) {
    ret = 1;
  } else if (airFP_NEG_INF == c) {
    ret = -1;
  } else {
    ret = 0;
  }
  return ret;
}
int
airIsInf_d(double d) {
  int c, ret;

  c = airFPClass_d(d);
  if (airFP_POS_INF == c) {
    ret = 1;
  } else if (airFP_NEG_INF == c) {
    ret = -1;
  } else {
    ret = 0;
  }
  return ret;
}

/* airExists_f() airExists_d() were nixed because they weren't used-
  you can just use AIR_EXISTS_F and AIR_EXISTS_D directly */

/*
******** airExists()
**
** an optimization-proof alternative to AIR_EXISTS
*/
int
airExists(double val) {
  airDouble ad;
  airULLong ui;

  ad.d = val;
  ui = ad.i;
  ui >>= 52;
  return 0x7ff != AIR_UINT(ui & 0x7ff);
}

/*
******** airNaN()
**
** returns a float quiet NaN
*/
float
airNaN(void) {

  return airFPGen_f(airFP_NAN);
}

/*
******** airFPFprintf_f()
**
** prints out the bits of a "float", indicating the three different fields
*/
void
airFPFprintf_f(FILE *file, float val) {
  int i, cls;
  unsigned int sign, expo, mant;
  airFloat af;

  if (file) {
    af.f = val;
    airFPValToParts_f(&sign, &expo, &mant, val);
    cls = airFPClass_f(val);
    fprintf(file, "%.9g (class %d=%s) 0x%08x = ", (double)val, cls,
            airEnumStr(airFPClass_ae, cls), af.i);
    fprintf(file, "sign:0x%x, expo:0x%02x, mant:0x%06x = \n", sign, expo, mant);
    fprintf(file, " S [ . . Exp . . ] "
                  "[ . . . . . . . . . Mant. . . . . . . . . . ]\n");
    fprintf(file, " %d ", sign);
    for (i = 7; i >= 0; i--) {
      fprintf(file, "%d ", (expo >> i) & 1);
    }
    for (i = 22; i >= 0; i--) {
      fprintf(file, "%d ", (mant >> i) & 1);
    }
    fprintf(file, "\n");
  }
}

/*
******** airFPFprintf_d()
**
** prints out the bits of a "double", indicating the three different fields
*/
void
airFPFprintf_d(FILE *file, double val) {
  int i, cls;
  unsigned int sign, expo, mant0, mant1, half0, half1;
  airDouble ad;
  airULLong ui;

  if (file) {
    ad.d = val;
    ui = ad.i;
    half1 = AIR_UINT(ui & 0xffffffff);
    ui >>= 32;
    half0 = AIR_UINT(ui & 0xffffffff);
    cls = airFPClass_d(val);
    fprintf(file, "%.17g (class %d=%s) 0x%08x %08x = \n", val, cls,
            airEnumStr(airFPClass_ae, cls), half0, half1);
    airFPValToParts_d(&sign, &expo, &mant0, &mant1, val);
    fprintf(file, "sign:0x%x, expo:0x%03x, mant:0x%05x %08x = \n", sign, expo, mant0,
            mant1);
    fprintf(file, "S[...Exp...][.......................Mant.......................]\n");
    fprintf(file, "%d", sign);
    for (i = 10; i >= 0; i--) {
      fprintf(file, "%d", (expo >> i) & 1);
    }
    for (i = 19; i >= 0; i--) {
      fprintf(file, "%d", (mant0 >> i) & 1);
    }
    for (i = 31; i >= 0; i--) {
      fprintf(file, "%d", (mant1 >> i) & 1);
    }
    fprintf(file, "\n");
  }
}

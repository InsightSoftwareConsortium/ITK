/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2012, 2011, 2010, 2009  University of Chicago
  Copyright (C) 2008, 2007, 2006, 2005  Gordon Kindlmann
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
#include "privateAir.h"
#include "teemQnanhibit.h"

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
** _airFloatEndian{Little,Big}, _airDoubleEndian{Little,Big}
**
** these unions facilitate converting amongst
** i: unsigned integral type
** c: (sign,exp,frac) triples of unsigned integral components
** v: the floating point numbers these bit-patterns represent
*/
typedef union {
  unsigned int i;
  struct {
    unsigned int mant : 23;
    unsigned int expo : 8;
    unsigned int sign : 1;
  } c;
  float v;
} _airFloatEndianLittle;
/* HEY COPY AND PASTE */
typedef union {
  unsigned int i;
  struct {
    unsigned int sign : 1;
    unsigned int expo : 8;
    unsigned int mant : 23;
  } c;
  float v;
} _airFloatEndianBig;

typedef union {
  airULLong i;
  /* these next two members are used for printing in airFPFprintf_d */
  struct { /* access to whole double as two unsigned ints */
    unsigned int half0 : 32;
    unsigned int half1 : 32;
  } h;
  struct { /* access to fraction with two unsigned ints */
    unsigned int mant1 : 32;
    unsigned int mant0 : 20;
    unsigned int expo : 11;
    unsigned int sign : 1;
  } c;
  double v;
} _airDoubleEndianLittle;
/* HEY COPY AND PASTE */
typedef union {
  airULLong i;
  /* these next two members are used for printing in airFPFprintf_d */
  struct { /* access to whole double as two unsigned ints */
    unsigned int half1 : 32;
    unsigned int half0 : 32;
  } h;
  struct { /* access to fraction with two unsigned ints */
    unsigned int sign : 1;
    unsigned int expo : 11;
    unsigned int mant0 : 20;
    unsigned int mant1 : 32;
  } c;
  double v;
} _airDoubleEndianBig;

/*
** The hex numbers in braces are examples of C's "initial member of a union"
** aggregate initialization.
*/

#if TEEM_QNANHIBIT == 1
const int airMyQNaNHiBit = 1;
const airFloat airFloatQNaN = {0x7fffffff};
const airFloat airFloatSNaN = {0x7fbfffff};
#else
const int airMyQNaNHiBit = 0;
const airFloat airFloatQNaN = {0x7fbfffff};
const airFloat airFloatSNaN = {0x7fffffff};
#endif

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

/* the bit-masking done here quiets gcc -Wconversion warnings */
#define FP_SET_F(flit, fbig, s, e, m)          \
  flit.c.sign = 1u & (s);                      \
  flit.c.expo = ((1u<8)-1) & (e);              \
  flit.c.mant = ((1u<23)-1) & (m);             \
  fbig.c.sign = 1u & (s);                      \
  fbig.c.expo = ((1u<8)-1) & (e);              \
  fbig.c.mant = ((1u<23)-1) & (m)

#define FP_GET_F(s, e, m, flit, fbig)            \
  if (airEndianLittle == airMyEndian()) {        \
    (s) = flit.c.sign;                           \
    (e) = flit.c.expo;                           \
    (m) = flit.c.mant;                           \
  } else {                                       \
    (s) = fbig.c.sign;                           \
    (e) = fbig.c.expo;                           \
    (m) = fbig.c.mant;                           \
  }

#define FP_SET_D(dlit, dbig, s, e, m0, m1)     \
  dlit.c.sign = 1u & (s);                      \
  dlit.c.expo = ((1u<<11)-1) & (e);            \
  dlit.c.mant0 = ((1u<<20)-1) & (m0);          \
  dlit.c.mant1 = (m1);                         \
  dbig.c.sign = 1u & (s);                      \
  dbig.c.expo = ((1u<<11)-1) & (e);            \
  dbig.c.mant0 = ((1u<<20)-1) & (m0);          \
  dbig.c.mant1 = (m1)

#define FP_GET_D(s, e, m0, m1, dlit, dbig)       \
  if (airEndianLittle == airMyEndian()) {        \
    (s) = dlit.c.sign;                           \
    (e) = dlit.c.expo;                           \
    (m0) = dlit.c.mant0;                         \
    (m1) = dlit.c.mant1;                         \
  } else {                                       \
    (s) = dbig.c.sign;                           \
    (e) = dbig.c.expo;                           \
    (m0) = dbig.c.mant0;                         \
    (m1) = dbig.c.mant1;                         \
  }


float
airFPPartsToVal_f(unsigned int sign,
                  unsigned int expo,
                  unsigned int mant) {
  _airFloatEndianLittle flit;
  _airFloatEndianBig fbig;

  FP_SET_F(flit, fbig, sign, expo, mant);
  return (airEndianLittle == airMyEndian()
          ? flit.v
          : fbig.v);
}

void
airFPValToParts_f(unsigned int *signP,
                  unsigned int *expoP,
                  unsigned int *mantP, float v) {
  _airFloatEndianLittle flit;
  _airFloatEndianBig fbig;

  flit.v = fbig.v = v;
  FP_GET_F(*signP, *expoP, *mantP, flit, fbig);
}

double
airFPPartsToVal_d(unsigned int sign,
                  unsigned int expo,
                  unsigned int mant0,
                  unsigned int mant1) {
  _airDoubleEndianLittle dlit;
  _airDoubleEndianBig dbig;

  FP_SET_D(dlit, dbig, sign, expo, mant0, mant1);
  return (airEndianLittle == airMyEndian()
          ? dlit.v
          : dbig.v);
}

/*
** Disable the 'local variable used without having been initialized'
** warning produced by the MSVC compiler
*/
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4700)
#endif
void
airFPValToParts_d(unsigned int *signP,
                  unsigned int *expoP,
                  unsigned int *mant0P,
                  unsigned int *mant1P, double v) {
  _airDoubleEndianLittle dlit;
  _airDoubleEndianBig dbig;

  dlit.v = dbig.v = v;
  FP_GET_D(*signP, *expoP, *mant0P, *mant1P, dlit, dbig);
}
#ifdef _MSC_VER
#pragma warning(pop)
#endif

/*
******** airFPGen_f()
**
** generates a floating point value which is a member of the given class
*/
float
airFPGen_f(int cls) {
  _airFloatEndianLittle flit;
  _airFloatEndianBig fbig;

  switch(cls) {
  case airFP_SNAN:
    /* sgn: anything, mant: anything non-zero with high bit !TEEM_QNANHIBIT */
    FP_SET_F(flit, fbig, 0, 0xff, (!TEEM_QNANHIBIT << 22) | 0x3fffff);
    break;
  case airFP_QNAN:
    /* sgn: anything, mant: anything non-zero with high bit TEEM_QNANHIBIT */
    FP_SET_F(flit, fbig, 0, 0xff, (TEEM_QNANHIBIT << 22) | 0x3fffff);
    break;
  case airFP_POS_INF:
    FP_SET_F(flit, fbig, 0, 0xff, 0);
    break;
  case airFP_NEG_INF:
    FP_SET_F(flit, fbig, 1, 0xff, 0);
    break;
  case airFP_POS_NORM:
    /* exp: anything non-zero but < 0xff, mant: anything */
    FP_SET_F(flit, fbig, 0, 0x80, 0x7ff000);
    break;
  case airFP_NEG_NORM:
    /* exp: anything non-zero but < 0xff, mant: anything */
    FP_SET_F(flit, fbig, 1, 0x80, 0x7ff000);
    break;
  case airFP_POS_DENORM:
    /* mant: anything non-zero */
    FP_SET_F(flit, fbig, 0, 0, 0xff);
    break;
  case airFP_NEG_DENORM:
    /* mant: anything non-zero */
    FP_SET_F(flit, fbig, 1, 0, 0xff);
    break;
  case airFP_NEG_ZERO:
    FP_SET_F(flit, fbig, 1, 0, 0);
    break;
  case airFP_POS_ZERO:
  default:
    FP_SET_F(flit, fbig, 0, 0, 0);
    break;
  }
  return (airEndianLittle == airMyEndian()
          ? flit.v
          : fbig.v);
}

/*
******** airFPGen_d()
**
** generates a floating point value which is a member of the given class
*/
double
airFPGen_d(int cls) {
  _airDoubleEndianLittle dlit;
  _airDoubleEndianBig dbig;

  switch(cls) {
  case airFP_SNAN:
    /* sgn: anything, mant: anything non-zero with high bit !TEEM_QNANHIBIT */
    FP_SET_D(dlit, dbig,  0, 0x7ff, (!TEEM_QNANHIBIT << 19) | 0x7ffff, 0xffffffff);
    break;
  case airFP_QNAN:
    /* sgn: anything, mant anything non-zero with high bit TEEM_QNANHIBIT */
    FP_SET_D(dlit, dbig,  0, 0x7ff, (TEEM_QNANHIBIT << 19) | 0x7ffff, 0xffffffff);
    break;
  case airFP_POS_INF:
    FP_SET_D(dlit, dbig,  0, 0x7ff, 0, 0);
    break;
  case airFP_NEG_INF:
    FP_SET_D(dlit, dbig,  1, 0x7ff, 0, 0);
    break;
  case airFP_POS_NORM:
    /* exp: anything non-zero but < 0xff, mant: anything */
    FP_SET_D(dlit, dbig,  0, 0x400, 0x0ff00, 0);
    break;
  case airFP_NEG_NORM:
    /* exp: anything non-zero but < 0xff, mant: anything */
    FP_SET_D(dlit, dbig,  1, 0x400, 0x0ff00, 0);
    break;
  case airFP_POS_DENORM:
    /* mant: anything non-zero */
    FP_SET_D(dlit, dbig,  0, 0, 0xff, 0);
    break;
  case airFP_NEG_DENORM:
    /* mant: anything non-zero */
    FP_SET_D(dlit, dbig,  1, 0, 0xff, 0);
    break;
  case airFP_NEG_ZERO:
    FP_SET_D(dlit, dbig,  1, 0, 0, 0);
    break;
  case airFP_POS_ZERO:
  default:
    FP_SET_D(dlit, dbig,  0, 0, 0, 0);
    break;
  }
  return (airEndianLittle == airMyEndian()
          ? dlit.v
          : dbig.v);
}

/*
******** airFPClass_f()
**
** given a floating point number, tells which class its in
*/
int
airFPClass_f(float val) {
  _airFloatEndianLittle flit;
  _airFloatEndianBig fbig;
  unsigned int sign, expv, mant;
  int indexv, ret = 0;

  flit.v = fbig.v = val;
  FP_GET_F(sign, expv, mant, flit, fbig);
  indexv = ((!!sign) << 2) | ((!!expv) << 1) | (!!mant);
  switch(indexv) {
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
    if (0xff == expv) {
      ret = airFP_POS_INF;
    } else {
      ret = airFP_POS_NORM;
    }
    break;
  case 3:
    /* exponent and mantissa fields are non-zero */
    if (0xff == expv) {
      if (TEEM_QNANHIBIT == mant >> 22) {
        ret = airFP_QNAN;
      } else {
        ret = airFP_SNAN;
      }
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
    if (0xff > expv) {
      ret = airFP_NEG_NORM;
    } else {
      ret = airFP_NEG_INF;
    }
    break;
  case 7:
    /* all fields are non-zero */
    if (0xff > expv) {
      ret = airFP_NEG_NORM;
    } else {
      if (TEEM_QNANHIBIT == mant >> 22) {
        ret = airFP_QNAN;
      } else {
        ret = airFP_SNAN;
      }
    }
    break;
  }
  return ret;
}

/*
** Disable the 'local variable used without having been initialized'
** warning produced by the MSVC compiler
*/
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable : 4700)
#endif
/*
******** airFPClass_d()
**
** given a double, tells which class its in
*/
int
airFPClass_d(double val) {
  _airDoubleEndianLittle dlit;
  _airDoubleEndianBig dbig;
  unsigned int sign, expo, mant0, mant1;
  int indexv, ret=0;
  unsigned char hibit;

  dlit.v = dbig.v = val;
  /* "expo = d.c.expo" had been annotated with: "this seems to be a
     WIN32 bug: on a quiet-NaN, d.c.exp should be non-zero, but it was
     completely zero, so that this function returned airFP_NEG_DENORM
     instead of airFP_QNAN" */
  FP_GET_D(sign, expo, mant0, mant1, dlit, dbig);
  hibit = AIR_CAST(unsigned char, mant0 >> 19); /* mant0 20 bits wide: ok */

  indexv = ((!!sign) << 2) | ((!!expo) << 1) | (!!mant0 || !!mant1);
  switch(indexv) {
  case 0:
    /* all fields are zero */
    ret = airFP_POS_ZERO;
    break;
  case 1:
    /* only fractional field is non-zero */
    ret = airFP_POS_DENORM;
    break;
  case 2:
    /* only exponent field is non-zero */
    if (0x7ff > expo) {
      ret = airFP_POS_NORM;
    } else {
      ret = airFP_POS_INF;
    }
    break;
  case 3:
    /* exponent and fractional fields are non-zero */
    if (0x7ff > expo) {
      ret = airFP_POS_NORM;
    } else {
      if (TEEM_QNANHIBIT == hibit) {
        ret = airFP_QNAN;
      } else {
        ret = airFP_SNAN;
      }
    }
    break;
  case 4:
    /* only sign field is non-zero */
    ret = airFP_NEG_ZERO;
    break;
  case 5:
    /* sign and fractional fields are non-zero */
    ret = airFP_NEG_DENORM;
    break;
  case 6:
    /* sign and exponent fields are non-zero */
    if (0x7ff > expo) {
      ret = airFP_NEG_NORM;
    } else {
      ret = airFP_NEG_INF;
    }
    break;
  case 7:
    /* all fields are non-zero */
    if (0x7ff > expo)
      ret = airFP_NEG_NORM;
    else {
      if (TEEM_QNANHIBIT == hibit) {
        ret = airFP_QNAN;
      } else {
        ret = airFP_SNAN;
      }
    }
    break;
  }
  return ret;
}
#ifdef _MSC_VER
#pragma warning(pop)
#endif

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
  _airFloatEndianLittle flit;
  _airFloatEndianBig fbig;
  unsigned int sign, expo, mant;

  flit.v = fbig.v = AIR_CAST(float, g);
  FP_GET_F(sign, expo, mant, flit, fbig);
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
  _airDoubleEndianLittle dlit;
  _airDoubleEndianBig dbig;

  dbig.v = dlit.v = val;
  return (airEndianLittle == airMyEndian()
          ? 0x7ff != dlit.c.expo
          : 0x7ff != dbig.c.expo);
}

/*
******** airNaN()
**
** returns a float quiet NaN
*/
float
airNaN(void) {

  return airFPGen_f(airFP_QNAN);
}

/*
******** airFPFprintf_f()
**
** prints out the bits of a "float", indicating the three different fields
*/
void
airFPFprintf_f(FILE *file, float val) {
  int i;
  unsigned int sign, expo, mant;
  _airFloatEndianLittle flit;
  _airFloatEndianBig fbig;

  if (file) {
    flit.v = fbig.v = val;
    FP_GET_F(sign, expo, mant, flit, fbig);
    fprintf(file, "%f: class %d; 0x%08x = ",val, airFPClass_f(val),
            airEndianLittle == airMyEndian() ? flit.i : fbig.i);
    fprintf(file, "sign:0x%x, expo:0x%02x, mant:0x%06x = \n",
            sign, expo, mant);
    fprintf(file, " S [ . . Exp . . ] "
            "[ . . . . . . . . . Mant. . . . . . . . . . ]\n");
    fprintf(file, " %d ", sign);
    for (i=7; i>=0; i--) {
      fprintf(file, "%d ", (expo >> i) & 1);
    }
    for (i=22; i>=0; i--) {
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
  int i;
  _airDoubleEndianLittle dlit;
  _airDoubleEndianBig dbig;
  unsigned int sign, expo, mant0, mant1;

  if (file) {
    dlit.v = dbig.v = val;
    fprintf(file, "%f: class %d; 0x%08x %08x = \n",
            val, airFPClass_d(val),
            (airEndianLittle == airMyEndian() ? dlit.h.half1 : dbig.h.half1),
            (airEndianLittle == airMyEndian() ? dlit.h.half0 : dbig.h.half0));
    FP_GET_D(sign, expo, mant0, mant1, dlit, dbig);
    fprintf(file, "sign:0x%x, expo:0x%03x, mant:0x%05x %08x = \n",
            sign, expo, mant0, mant1);
    fprintf(file, "S[...Exp...][.......................Mant.......................]\n");
    fprintf(file, "%d", sign);
    for (i=10; i>=0; i--) {
      fprintf(file, "%d", (expo >> i) & 1);
    }
    for (i=19; i>=0; i--) {
      fprintf(file, "%d", (mant0 >> i) & 1);
    }
    for (i=31; i>=0; i--) {
      fprintf(file, "%d", (mant1 >> i) & 1);
    }
    fprintf(file, "\n");
  }
}


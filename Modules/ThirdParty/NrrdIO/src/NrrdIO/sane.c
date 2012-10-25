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

/*
******** airSanity()
**
** Does run-time checks to see if the compile-time constants are correct.
** Returns a value from the airInsane* enum; airInsane_not means all
** the checks came back without detecting any problems.
*/
int
airSanity(void) {
  double nanValue, pinf, ninf;
  float nanF, pinfF, ninfF;
  unsigned int sign, expvalue, mant;
  int tmpI;
  char endian;
  unsigned char uc0, uc1;
  static int _airSanity=0;

  if (_airSanity) {
    return airInsane_not;
  }

  /* now that there is no more compile-time endian info, this is
     merely double checking that airMyEndian() works, and returns
     the constants (either 1234, pronounced "little endian", or
     4321, "big endian") that are defined in air.h */
  tmpI = 1;
  endian = !(*((char*)(&tmpI)));
  if (endian) {
    /* big endian */
    if (4321 != airMyEndian()) {
      return airInsane_endian;
    }
  } else {
    if (1234 != airMyEndian()) {
      return airInsane_endian;
    }
  }

  /* checks on sizes of uchar, float, int, double, airLLong */
  uc0 = 255;
  uc1 = AIR_CAST(unsigned char, AIR_INT(uc0) + 1); /* want to overflow */
  if (!( 255 == uc0 && 0 == uc1 )) {
    return airInsane_UCSize;
  }
  /* these justify the AIR_EXISTS_F and AIR_EXISTS_D macros */
  if (!( (sizeof(float) == sizeof(int)) && (4 == sizeof(int)) )) {
    return airInsane_FISize;
  }
  if (!( (sizeof(double) == sizeof(airLLong)) && (8 == sizeof(airLLong)) )) {
    return airInsane_DLSize;
  }

  /* run-time NaN checks */
  pinf = DBL_MAX;
  pinf = _airSanityHelper(pinf);
  pinf = _airSanityHelper(pinf);
  pinf = _airSanityHelper(pinf);
  if (AIR_EXISTS(pinf)) {
    return airInsane_pInfExists;
  }
  ninf = -pinf;
  if (AIR_EXISTS(ninf)) {
    return airInsane_nInfExists;
  }
  nanValue = pinf / pinf;
  if (AIR_EXISTS(nanValue)) {
    return airInsane_NaNExists;
  }
  nanF = (float)nanValue;
  pinfF = (float)pinf;
  ninfF = (float)ninf;
  airFPValToParts_f(&sign, &expvalue, &mant, nanF);
  mant >>= 22;
  if (AIR_QNANHIBIT != (int)mant) {
    return airInsane_QNaNHiBit;
  }

  if (!( airFP_QNAN == airFPClass_f(AIR_NAN)
         && airFP_QNAN == airFPClass_f(AIR_QNAN)
         /*
           As of July 4 2012 GLK decides that the signalling NaN tests are
           more trouble than they're worth: the signal-ness of the NaN is not
           preserved in double-float conversion for some platforms (so
           airFP_SNAN == airFPClass_d(AIR_SNAN) has never been enforced), and
           there are more platforms for which (apparently) passing AIR_SNAN to
           airFPClass_d changes it to a quiet NaN, which defeats the purpose
           of the test.  To summarize, given that:
           ** AIR_NAN and AIR_QNAN are checked here to be quiet NaN, after
              casting to both float and double,
           ** quiet NaN "hi bit" is tested above, and that
           ** quiet and signalling NaN are mutually exclusive,
           skipping the signalling NaN tests is unlikely to undermine knowing
           the correctness of the compile-time representation of NaNs.  So the
           following line is now commented out for all platforms.
         */
         /* && airFP_SNAN == airFPClass_f(AIR_SNAN) */
         && airFP_QNAN == airFPClass_d(AIR_NAN)
         && airFP_QNAN == airFPClass_d(AIR_QNAN) )) {
    return airInsane_AIR_NAN;
  }
  if (!(airFP_QNAN == airFPClass_f(nanF)
        && airFP_POS_INF == airFPClass_f(pinfF)
        && airFP_NEG_INF == airFPClass_f(ninfF))) {
    /* really, this is verifying that assigning from a double to a
       float maintains the FPClass for non-existent values */
    return airInsane_FltDblFPClass;
  }

  /* just make sure AIR_DIO is reasonably set
     (actually, this should be done by include/teemDio.h) */
  switch (AIR_DIO) {
  case 0: break;
  case 1: break;
  default:
    return airInsane_dio;
  }

  _airSanity = 1;
  return airInsane_not;
}

static const char
_airInsaneErr[AIR_INSANE_MAX+1][AIR_STRLEN_MED] = {
  "sanity checked PASSED!",
  "airMyEndian() is wrong",
  "AIR_EXISTS(+inf) was true",
  "AIR_EXISTS(-inf) was true",
  "AIR_EXISTS(NaN) was true",
  "air_FPClass_f() wrong after double->float assignment",
  "TEEM_QNANHIBIT is wrong",
  "airFPClass(AIR_QNAN) wrong",
  "TEEM_DIO has invalid value",
  "unsigned char isn't 8 bits",
  "sizeof(float), sizeof(int) not both == 4",
  "sizeof(double), sizeof(airLLong) not both == 8",
};

static const char _airBadInsane[] = "(invalid insane value)";

const char *
airInsaneErr(int insane) {

  if (AIR_IN_CL(0, insane, AIR_INSANE_MAX)) {
    return _airInsaneErr[insane];
  }
  else {
    return _airBadInsane;
  }
}


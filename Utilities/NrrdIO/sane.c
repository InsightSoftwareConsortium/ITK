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
  double nan, pinf, ninf;
  float nanF, pinfF, ninfF;
  unsigned int sign, exp, mant;
  int tmpI, size;
  char endian;
  unsigned char uc0, uc1;
  static int _airSanity=0;
  
  if (_airSanity) {
    return airInsane_not;
  }

  /* run-time endian check */
  tmpI = 1;
  endian = !(*((char*)(&tmpI)));
  if (endian) {
    /* big endian */
    if (4321 != AIR_ENDIAN) {
      return airInsane_endian;
    }
  }
  else {
    if (1234 != AIR_ENDIAN) {
      return airInsane_endian;
    }
  }    

  /* checks on sizes of uchar, float, int, double, airLLong */  
  uc0 = 255;
  uc1 = uc0 + 1;  /* to avoid compiler warnings */
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
  nan = pinf / pinf;
  if (AIR_EXISTS(nan)) {
    return airInsane_NaNExists;
  }
  nanF = (float)nan;
  pinfF = (float)pinf;
  ninfF = (float)ninf;
  airFPValToParts_f(&sign, &exp, &mant, nanF);
  mant >>= 22;
  if (AIR_QNANHIBIT != (int)mant) {
    return airInsane_QNaNHiBit;
  }
  if (!( airFP_QNAN == airFPClass_f(AIR_NAN)
         && airFP_QNAN == airFPClass_f(AIR_QNAN)
#if !defined(_MSC_VER) || _MSC_VER < 1400 /* VS2005 converts SNAN to QNAN */
         && airFP_SNAN == airFPClass_f(AIR_SNAN) 
#endif
         && airFP_QNAN == airFPClass_d(AIR_NAN)
         && airFP_QNAN == airFPClass_d(AIR_QNAN) )) {
    /* we don't bother checking for 
       airFP_SNAN == airFPClass_d(AIR_SNAN) because
       on some platforms the signal-ness of the NaN
       is not preserved in double-float conversion */
    return airInsane_AIR_NAN;
  }
  if (!(airFP_QNAN == airFPClass_f(nanF)
        && airFP_POS_INF == airFPClass_f(pinfF)
        && airFP_NEG_INF == airFPClass_f(ninfF))) {
    /* really, this is verifying that assigning from a double to a 
       float maintains the FPClass for non-existant values */
    return airInsane_FltDblFPClass;
  }
  
  /* just make sure AIR_DIO is reasonably set 
     (actually, this should be done by include/teem/need/dio.h) */
  switch (AIR_DIO) {
  case 0: break;
  case 1: break;
  default:
    return airInsane_dio;
  }

  /* run-time 32/64-bit check */
  size = 0;
  switch (AIR_32BIT) {
  case 1: size = 4; break;
  case 0: size = 8; break;
  default: break;
  }
  if (size != sizeof(size_t)) {
    return airInsane_32Bit;
  }

  _airSanity = 1;
  return airInsane_not;
}

const char
_airInsaneErr[AIR_INSANE_MAX+1][AIR_STRLEN_MED] = {
  "sanity checked PASSED!",
  "TEEM_ENDIAN is wrong",
  "AIR_EXISTS(+inf) was true",
  "AIR_EXISTS(-inf) was true",
  "AIR_EXISTS(NaN) was true",
  "air_FPClass_f() wrong after double->float assignment",
  "TEEM_QNANHIBIT is wrong",
  "airFPClass(AIR_QNAN,AIR_SNAN) wrong",
  "TEEM_DIO has invalid value",
  "TEEM_32BIT is wrong",
  "unsigned char isn't 8 bits",
  "sizeof(float), sizeof(int) not both == 4",
  "sizeof(double), sizeof(airLLong) not both == 8",
};

char _airBadInsane[] = "(invalid insane value)";

const char *
airInsaneErr(int insane) {
  
  if (AIR_IN_CL(0, insane, AIR_INSANE_MAX)) {
    return _airInsaneErr[insane];
  }
  else {
    return _airBadInsane;
  }
}


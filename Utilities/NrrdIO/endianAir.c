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
#include "teemEndian.h"

/*
******** int airMyEndian
**
** it gets set to 1234 or 4321
*/
#if TEEM_ENDIAN == 1234
const int airMyEndian = 1234;
#else
const int airMyEndian = 4321;
#endif

char
_airEndianStr[][AIR_STRLEN_SMALL] = {
  "(unknown endian)",
  "little",
  "big"
};

char
_airEndianDesc[][AIR_STRLEN_MED] = {
  "unknown endianness",
  "Intel and compatible",
  "Everyone besides Intel and compatible"
};

int
_airEndianVal[] = {
  airEndianUnknown,
  airEndianLittle,
  airEndianBig,
};

const airEnum
_airEndian = {
  "endian",
  2,
  _airEndianStr, _airEndianVal,
  _airEndianDesc,
  NULL, NULL,
  AIR_FALSE
};

const airEnum *const
airEndian = &_airEndian;


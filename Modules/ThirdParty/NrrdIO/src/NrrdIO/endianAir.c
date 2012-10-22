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

/*
******** airMyEndian()
**
** determine at run-time if we are little (1234) or big (4321) endian
*/
int
airMyEndian(void) {
  int tmpI, ret;
  char leastbyte;

  /* set int to 1:
     least signficant byte will be 1,
     most signficant byte will be 0 */
  tmpI = 1;
  /* cast address of (4-byte) int to char*, and dereference,
     which retrieves the byte at the low-address-end of int
     (the "first" byte in memory ordering).
     On big endian, we're getting the most significant byte (0);
     on little endian, we're getting least significant byte (1) */
  leastbyte = *(AIR_CAST(char*, &tmpI));
  if (leastbyte) {
    ret = airEndianLittle;
  } else {
    ret = airEndianBig;
  }
  return ret;
}

static const char *
_airEndianStr[] = {
  "(unknown endian)",
  "little",
  "big"
};

static const char *
_airEndianDesc[] = {
  "unknown endianness",
  "Intel and compatible",
  "Everyone besides Intel and compatible"
};

static const int
_airEndianVal[] = {
  airEndianUnknown,
  airEndianLittle,
  airEndianBig,
};

static const airEnum
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


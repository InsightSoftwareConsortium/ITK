/*
  NrrdIO: stand-alone code for basic nrrd functionality
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

#include "teemEndian.h"
#include "teemQnanhibit.h"

/* miscAir.c */
extern double _airSanityHelper(double val);

/*
** _airFloat, _airDouble
**
** these unions facilitate converting amonst
** i: unsigned integral type
** c: (sign,exp,frac) triples of unsigned integral components
** v: the floating point numbers these bit-patterns represent
*/
typedef union {
  unsigned int i;
  struct {
#if TEEM_ENDIAN == 1234        /* little endian */
    unsigned int mant : 23;
    unsigned int expo : 8;
    unsigned int sign : 1;
#else                          /* big endian */
    unsigned int sign : 1;
    unsigned int expo : 8;
    unsigned int mant : 23;
#endif
  } c;
  float v;
} _airFloat;

typedef union {
  airULLong i;
  /* these next two members are used for printing in airFPFprintf_d */
  struct { /* access to whole double as two unsigned ints */
#if TEEM_ENDIAN == 1234
    unsigned int half0 : 32;
    unsigned int half1 : 32;
#else
    unsigned int half1 : 32;
    unsigned int half0 : 32;
#endif
  } h;
  struct { /* access to fraction with two unsigned ints */
#if TEEM_ENDIAN == 1234        /* little endian */
    unsigned int mant1 : 32;
    unsigned int mant0 : 20;
    unsigned int expo : 11;
    unsigned int sign : 1;
#else                          /* big endian */
    unsigned int sign : 1;
    unsigned int expo : 11;
    unsigned int mant0 : 20;
    unsigned int mant1 : 32;
#endif
  } c;
  double v;
} _airDouble;


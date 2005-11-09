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


/*
** the end result of this is that the source file which includes
** this can be sure that TEEM_ENDIAN is set, and can be sure that
** it is set to either 1234 or 4321
*/

#ifndef TEEM_ENDIAN
#  error TEEM_ENDIAN not defined, see architecture-specific .mk file or check compilation options
#elif TEEM_ENDIAN == 1234
#  /* okay, its little endian */
#elif TEEM_ENDIAN == 4321
#  /* okay, its big endian */
#else
#  error TEEM_ENDIAN not set to 1234 (little endian) or 4321 (big endian), see architecture-specific .mk file or check compilation options
#endif

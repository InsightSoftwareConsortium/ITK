/*
  NrrdIO: stand-alone code for basic nrrd functionality
  Copyright (C) 2004, 2003, 2002, 2001, 2000, 1999, 1998 University of Utah
 
  These source files have been copied and/or modified from teem,
  Gordon Kindlmann's research software; <http://teem.sourceforge.net>.
  Teem is licensed under the GNU Lesser Public License. The
  non-copyleft licensing defined here applies to only the source files
  in the NrrdIO distribution (not the rest of teem), and only to the
  files originating with NrrdIO (not analogous files in teem).
 
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
** this can be sure that TEEM_BIGBITFIELD is set, and can be sure that
** it is set to either 0 or 1
*/

#ifndef TEEM_BIGBITFIELD
#  error TEEM_BIGBITFIELD not defined, see architecture-specific .mk file or check compilation options
#elif TEEM_BIGBITFIELD == 1
#  /* okay, its 1 */
#elif TEEM_BIGBITFIELD == 0
#  /* okay, its 0 */
#else
#  error TEEM_BIGBITFIELD not set to 0 or 1, see architecture-specific .mk file or check compilation options
#endif

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


/*
** the end result of this is that the source file which includes
** this can be sure that TEEM_DIO is set, and can be sure that
** it is set to either 0 or 1
*/

#ifndef TEEM_DIO
#  error TEEM_DIO not defined, see architecture-specific .mk file or check compilation options
#elif TEEM_DIO == 1
#  /* okay, its 1 */
#elif TEEM_DIO == 0
#  /* okay, its 0 */
#else
#  error TEEM_DIO not set to 1 or 0, see architecture-specific .mk file or check compilation options
#endif

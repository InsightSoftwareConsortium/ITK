/* ----------------------------- MNI Header -----------------------------------
@NAME       : type_limits.h
@DESCRIPTION: Includes limits.h and float.h, undefining things that are
              defined in both to avoid errors from lint (on SGI).
@METHOD     : 
@CREATED    : August 7, 1992 (Peter Neelin)
@MODIFIED   : 
 * $Log: type_limits.h,v $
 * Revision 6.1  1999-10-19 14:45:12  neelin
 * Fixed Log subsitutions for CVS
 *
 * Revision 6.0  1997/09/12 13:24:54  neelin
 * Release of minc version 0.6
 *
 * Revision 5.0  1997/08/21  13:25:53  neelin
 * Release of minc version 0.5
 *
 * Revision 4.0  1997/05/07  20:07:52  neelin
 * Release of minc version 0.4
 *
 * Revision 3.0  1995/05/15  19:33:12  neelin
 * Release of minc version 0.3
 *
 * Revision 2.0  1994/09/28  10:38:19  neelin
 * Release of minc version 0.2
 *
 * Revision 1.5  94/09/28  10:37:33  neelin
 * Pre-release
 * 
 * Revision 1.4  93/08/11  12:06:47  neelin
 * Added RCS logging in source.
 * 
@COPYRIGHT  :
              Copyright 1993 Peter Neelin, McConnell Brain Imaging Centre, 
              Montreal Neurological Institute, McGill University.
              Permission to use, copy, modify, and distribute this
              software and its documentation for any purpose and without
              fee is hereby granted, provided that the above copyright
              notice appear in all copies.  The author and McGill University
              make no representations about the suitability of this
              software for any purpose.  It is provided "as is" without
              express or implied warranty.
@RCSID      : $Header: /private-cvsroot/minc/libsrc/type_limits.h,v 6.1 1999-10-19 14:45:12 neelin Exp $ MINC (MNI)
---------------------------------------------------------------------------- */

#include <limits.h>

/* Undefine those things that get redefined in float.h */
#ifdef FLT_DIG
#undef FLT_DIG
#endif
#ifdef DBL_DIG
#undef DBL_DIG
#endif
#ifdef DBL_MIN
#undef DBL_MIN
#endif
#ifdef DBL_MAX
#undef DBL_MAX
#endif

#include <float.h>

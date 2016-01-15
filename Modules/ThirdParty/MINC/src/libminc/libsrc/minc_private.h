#ifndef MINC_PRIVATE_H
#define MINC_PRIVATE_H

/* ----------------------------- MNI Header -----------------------------------
@NAME       : minc_private.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: The general include file for MINC routines.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : July 29, 1992 (Peter Neelin)
@MODIFIED   : 
 * $Log: minc_private.h,v $
 * Revision 6.4  2004-12-14 23:53:46  bert
 * Get rid of compilation warnings
 *
 * Revision 6.3  2004/10/15 13:47:31  bert
 * Minor changes for Windows compatibility
 *
 * Revision 6.2  2004/04/27 15:47:47  bert
 * #include minc_config.h and minc_error.h
 *
 * Revision 6.1  1999/10/19 14:45:09  neelin
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
 * Revision 2.1  1995/02/08  19:00:55  neelin
 * Removed include of math.h
 *
 * Revision 2.0  1994/09/28  10:38:07  neelin
 * Release of minc version 0.2
 *
 * Revision 1.7  94/09/28  10:37:27  neelin
 * Pre-release
 * 
 * Revision 1.6  93/10/06  10:00:30  neelin
 * Added include of memory.h for memcpy on SUNs.
 * 
 * Revision 1.5  93/08/11  12:06:39  neelin
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
@RCSID      : $Header: /private-cvsroot/minc/libsrc/minc_private.h,v 6.4 2004-12-14 23:53:46 bert Exp $ MINC (MNI)
---------------------------------------------------------------------------- */

#if defined(_MSC_VER)
/* If we are building on the Microsoft C compiler, we want to
 * explicitly export all public functions from the DLL
 */
#define MNCAPI __declspec(dllexport)
#else
#define MNCAPI
#endif

#include "config.h"
#define _GNU_SOURCE 1         /* Include all BSD & GNU interfaces */
#include  <stdlib.h>
#include  <stdio.h>
#include  <string.h>
#include  <memory.h>
#include  "minc.h"
#include  "minc_useful.h"
#include  "minc_basic.h"
#include  "minc_structures.h"
#include  "minc_routines.h"
#include  "minc_config.h"
#include  "minc_error.h"
#include  "minc_config.h"
#endif

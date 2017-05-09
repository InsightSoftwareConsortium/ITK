#ifndef MINC_BASIC_H
#define MINC_BASIC_H

/* ----------------------------- MNI Header -----------------------------------
@NAME       : minc_basic.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Constants and macros for private use by MINC routines.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : August 28, 1992 (Peter Neelin)
@MODIFIED   : 
 * $Log: minc_basic.h,v $
 * Revision 6.8  2010-03-27 15:09:28  rotor
 *  * back to 1000000
 *
 * Revision 6.7  2010-03-02 12:23:14  rotor
 *  * ported HDF calls to 1.8.x
 *  * Makefile.am: updated for minccmp
 *
 * Revision 6.6  2008/04/11 05:15:00  rotor
 *  * rewrote error code  (Claude) to remove global defs that were
 *     causing build problems with DYLIB on OSX
 *
 * Revision 6.5  2008/01/12 01:05:37  rotor
 *  * initial commits from Steve to remove warnings
 *
 * Revision 6.4  2007/08/09 17:05:25  rotor
 *  * added some fixes of Claudes for chunking and internal compression
 *
 * Revision 6.3  2004/04/27 15:48:15  bert
 * Minor changes
 *
 * Revision 6.2  2001/04/17 18:40:13  neelin
 * Modifications to work with NetCDF 3.x
 * In particular, changed NC_LONG to NC_INT (and corresponding longs to ints).
 * Changed NC_UNSPECIFIED to NC_NAT.
 * A few fixes to the configure script.
 *
 * Revision 6.1  1999/10/19 14:45:08  neelin
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
 * Revision 2.0  1994/09/28  10:38:01  neelin
 * Release of minc version 0.2
 *
 * Revision 1.8  94/09/28  10:37:26  neelin
 * Pre-release
 * 
 * Revision 1.7  93/10/28  10:18:23  neelin
 * Added FILLVALUE_EPSILON for doing fillvalue checking in icv's.
 * 
 * Revision 1.6  93/08/11  12:06:37  neelin
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
@RCSID      : $Header: /private-cvsroot/minc/libsrc/minc_basic.h,v 6.8 2010-03-27 15:09:28 rotor Exp $ MINC (MNI)
---------------------------------------------------------------------------- */

#include <minc.h>

/* --------- MINC specific constants ------------- */

/* Maximum buffer size for conversions. Should not be a power of 2 - this
   can cause poor performance on some systems (e.g. SGI) due to caching-
   related inefficiencies */
#define MI_MAX_VAR_BUFFER_SIZE 1000000

/* Possible values for sign of a value */
#define MI_PRIV_DEFSIGN   0
#define MI_PRIV_SIGNED    1
#define MI_PRIV_UNSIGNED  2

/* Operations for MI_varaccess */
#define MI_PRIV_GET 10
#define MI_PRIV_PUT 11

/* Suffix for dimension width variable names */
#define MI_WIDTH_SUFFIX "-width"

/* Epsilon for detecting fillvalues */
#define FILLVALUE_EPSILON (10.0 * FLT_EPSILON)



/* Macros for converting data types. These macros are compound statements, 
   so don't put a semi-colon after them. dvalue should be a double, type
   is an int NetCDF type, sign is one of MI_PRIV_UNSIGNED and 
   MI_PRIV_SIGNED and ptr is a void pointer */

#define MI_TO_DOUBLE(dvalue, type, sign, ptr) \
   switch (type) { \
   case NC_BYTE : \
   case NC_CHAR: \
      switch (sign) { \
      case MI_PRIV_UNSIGNED : \
         dvalue = (double) *((unsigned char *) ptr); break; \
      case MI_PRIV_SIGNED : \
         dvalue = (double) *((signed char *) ptr); break; \
      } \
      break; \
   case NC_SHORT : \
      switch (sign) { \
      case MI_PRIV_UNSIGNED : \
         dvalue = (double) *((unsigned short *) ptr); break; \
      case MI_PRIV_SIGNED : \
         dvalue = (double) *((signed short *) ptr); break; \
      } \
      break; \
   case NC_INT : \
      switch (sign) { \
      case MI_PRIV_UNSIGNED : \
         dvalue = (double) *((unsigned int *) ptr); break; \
      case MI_PRIV_SIGNED : \
         dvalue = (double) *((signed int  *) ptr); break; \
      } \
      break; \
   case NC_FLOAT : \
      dvalue = (double) *((float *) ptr); \
      break; \
   case NC_DOUBLE : \
      dvalue = (double) *((double *) ptr); \
      break; \
   case NC_NAT : \
      MI_LOG_PKG_ERROR2(MI_ERR_NONNUMERIC, \
         "Attempt to convert NC_NAT value to double"); \
      dvalue = 0; \
      break; \
   } 

#define MI_FROM_DOUBLE(dvalue, type, sign, ptr) \
   switch (type) { \
   case NC_BYTE : \
   case NC_CHAR : \
      switch (sign) { \
      case MI_PRIV_UNSIGNED : \
         dvalue = MAX(0, dvalue); \
         dvalue = MIN(UCHAR_MAX, dvalue); \
         *((unsigned char *) ptr) = ROUND(dvalue); \
         break; \
      case MI_PRIV_SIGNED : \
         dvalue = MAX(SCHAR_MIN, dvalue); \
         dvalue = MIN(SCHAR_MAX, dvalue); \
         *((signed char *) ptr) = ROUND(dvalue); \
         break; \
      } \
      break; \
   case NC_SHORT : \
      switch (sign) { \
      case MI_PRIV_UNSIGNED : \
         dvalue = MAX(0, dvalue); \
         dvalue = MIN(USHRT_MAX, dvalue); \
         *((unsigned short *) ptr) = ROUND(dvalue); \
         break; \
      case MI_PRIV_SIGNED : \
         dvalue = MAX(SHRT_MIN, dvalue); \
         dvalue = MIN(SHRT_MAX, dvalue); \
         *((signed short *) ptr) = ROUND(dvalue); \
         break; \
      } \
      break; \
   case NC_INT : \
      switch (sign) { \
      case MI_PRIV_UNSIGNED : \
         dvalue = MAX(0, dvalue); \
         dvalue = MIN(UINT_MAX, dvalue); \
         *((unsigned int *) ptr) = ROUND(dvalue); \
         break; \
      case MI_PRIV_SIGNED : \
         dvalue = MAX(INT_MIN, dvalue); \
         dvalue = MIN(INT_MAX, dvalue); \
         *((signed int *) ptr) = ROUND(dvalue); \
         break; \
      } \
      break; \
   case NC_FLOAT : \
      dvalue = MAX(-FLT_MAX,dvalue); \
      *((float *) ptr) = MIN(FLT_MAX,dvalue); \
      break; \
   case NC_DOUBLE : \
      *((double *) ptr) = dvalue; \
      break; \
   case NC_NAT : \
      MI_LOG_PKG_ERROR2(MI_ERR_NONNUMERIC, \
         "Attempt to convert to NC_NAT from double"); \
      dvalue = 0; \
      break; \
   }

/**/
#define _(x) x			/* For future gettext */

#endif

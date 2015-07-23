#ifndef MINC_USEFUL_H
#define MINC_USEFUL_H

/* ----------------------------- MNI Header -----------------------------------
@NAME       : minc_useful.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: A set of macros and definitions useful for MINC routines.
              (derived from mni_def.h)
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : July 15, 1991       David MacDonald
@MODIFIED   : 
 * $Log: minc_useful.h,v $
 * Revision 6.2  2004-10-15 13:47:55  bert
 * Minor changes for Windows compatibility
 *
 * Revision 6.1  1999/10/19 14:45:10  neelin
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
 * Revision 3.1  1997/04/10  19:22:18  neelin
 * Removed redefinition of NULL and added pointer casts in appropriate places.
 *
 * Revision 3.0  1995/05/15  19:33:12  neelin
 * Release of minc version 0.3
 *
 * Revision 2.0  1994/09/28  10:38:10  neelin
 * Release of minc version 0.2
 *
 * Revision 1.7  94/09/28  10:37:31  neelin
 * Pre-release
 * 
 * Revision 1.6  93/08/11  12:06:44  neelin
 * Added RCS logging in source.
 * 
              July 29, 1992       Peter Neelin
                 - changed name for MINC routines and added some macros
@COPYRIGHT  :
              Copyright 1993 David MacDonald and Peter Neelin, 
              McConnell Brain Imaging Centre, 
              Montreal Neurological Institute, McGill University.
              Permission to use, copy, modify, and distribute this
              software and its documentation for any purpose and without
              fee is hereby granted, provided that the above copyright
              notice appear in all copies.  The authors and McGill University
              make no representations about the suitability of this
              software for any purpose.  It is provided "as is" without
              express or implied warranty.
@RCSID      : $Header: /private-cvsroot/minc/libsrc/minc_useful.h,v 6.2 2004-10-15 13:47:55 bert Exp $ MINC (MNI)
---------------------------------------------------------------------------- */

/* ------------ define signed for vaxes ----------------- */
#if (defined(vax) && !defined(__STDC__))
#define signed
#endif

/* --------- define the prefixes to all functions ---------- */

#ifndef MNCAPI
#   define  MNCAPI
#endif
#define  PRIVATE     static
#define  SEMIPRIVATE

/* --------- define  TRUE and FALSE ------------------------ */

#ifndef  FALSE
#define  FALSE  0
#endif

#ifndef  TRUE
#define  TRUE   1
#endif

/* --------- macro to determine the size of a static array,
             e.g.,   int  array[] = { 1, 3, 9, 5 };           ------------ */

#define  SIZEOF_STATIC_ARRAY( array ) \
         ( sizeof(array) / sizeof((array)[0]))

/* --------- interpolate between a and b ------------------- */

#define  INTERPOLATE( alpha, a, b ) ((a) + (alpha) * ((b) - (a)))

/* --------- PI, and angles -------------------------------- */

#define  PI           M_PI                  /* from math.h */

#define  DEG_TO_RAD   (PI / 180.0)
#define  RAD_TO_DEG   (180.0 / PI)

/* --------- Absolute value, min, and max.  Bear in mind that these
             may evaluate an expression multiple times, i.e., ABS( x - y ),
             and therefore may be inefficient, or incorrect,
             i.e, ABS( ++x );                          ------------------ */

#define  ABS( x )  ( ((x) > (0)) ? (x) : (-(x)) )

/* ---------- Round to nearest integer - must be cast to integer in order
              for rounding to take effect, and the cast must truncate 
              towards zero - eg. i = (int) ROUND(x);
              Same caveats as ABS                      ------------------ */

#define ROUND( x ) ((x) + ( ((x) >= 0) ? 0.5 : (-0.5) ) )

#undef   MAX
#define  MAX( x, y )  ( ((x) >= (y)) ? (x) : (y) )

#define  MAX3( x, y, z )  MAX( x, MAX(y,z) )

#undef   MIN
#define  MIN( x, y )  ( ((x) <= (y)) ? (x) : (y) )

#define  MIN3( x, y, z )  MIN( x, MIN(y,z) )

/* --------- gets the address of a 2-d array element in a 1-d array ----- */

#define  IJ( i, j, nj )          ( (i) * (nj) + (j) )

/* --------- gets the address of a 3-d array element in a 1-d array ----- */

#define  IJK( i, j, k, nj, nk )  ( (k) + (nk) * ((j) + (nj) * (i)) )

/* --------- memory allocation macros -------------------------- */

#define  MALLOC( n_items, type ) \
         ( (type *) malloc( (size_t) (n_items) * sizeof(type) ) )

#define  CALLOC( n_items, type ) \
         ( (type *) calloc( (size_t) (n_items), sizeof(type) ) )

#define  REALLOC( ptr, n_items, type ) \
         ( (type *) realloc( (void *) ptr, (size_t) (n_items) * sizeof(type) ) )

#define  FREE( ptr ) \
         free( (void *) ptr )

/* --------- environment variables -------------------------- */

#ifdef sun
char  *getenv();         /* on suns, this declaration is not in stdlib.h */
#endif

#define  ENV_EXISTS( env ) ( getenv(env) != (char *) 0 )

/* --------- string macros -------------------------- */

#define STRINGS_EQUAL(str1,str2) (strcmp(str1,str2)==0)

#endif

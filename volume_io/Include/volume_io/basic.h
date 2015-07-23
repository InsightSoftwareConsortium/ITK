#ifndef VOL_IO_BASIC_H
#define VOL_IO_BASIC_H 

/* ----------------------------------------------------------------------------
@COPYRIGHT  :
              Copyright 1993,1994,1995 David MacDonald,
              McConnell Brain Imaging Centre,
              Montreal Neurological Institute, McGill University.
              Permission to use, copy, modify, and distribute this
              software and its documentation for any purpose and without
              fee is hereby granted, provided that the above copyright
              notice appear in all copies.  The author and McGill University
              make no representations about the suitability of this
              software for any purpose.  It is provided "as is" without
              express or implied warranty.
@VERSION    : $Header: /private-cvsroot/minc/volume_io/Include/volume_io/basic.h,v 1.35 2005-05-19 21:19:27 bert Exp $
---------------------------------------------------------------------------- */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : basic.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: A set of macros and definitions useful for all MNI programs.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : July 15, 1991       David MacDonald
@MODIFIED   :
---------------------------------------------------------------------------- */

#include  <math.h>
#include  <stdlib.h>
#include  <stdio.h>

#ifdef __sgi
#include  <string.h>     /* --- for memcpy, etc. */
#else
#include  <memory.h>     /* --- for memcpy, etc. */
#endif

#include  <volume_io/def_math.h>
#include  <volume_io/system_dependent.h>

/* --------- define  TRUE and FALSE ------------------------ */

#ifndef  FALSE
#define  FALSE  0
#endif

#ifndef  TRUE
#define  TRUE   1
#endif

/* These are the internal typedefs, which are aliased to their "classic"
 * volume_io names below, if the new VIO_PREFIX_NAMES macro is set to
 * zero.
 */
typedef char *VIO_STR;
typedef int VIO_BOOL;
typedef double VIO_Real;
typedef signed char VIO_SCHAR;
typedef unsigned char VIO_UCHAR;

typedef enum { VIO_OK=0,
               VIO_ERROR,
               VIO_INTERNAL_ERROR,
               VIO_END_OF_FILE,
               VIO_QUIT
             } VIO_Status;


             
/* --------- gets the address of a 2-d array element in a 1-d array ----- */

#define  VIO_IJ( i, j, nj )          ( (i) * (nj) + (j) )

/* --------- gets the address of a 3-d array element in a 1-d array ----- */

#define  VIO_IJK( i, j, k, nj, nk )  ( (k) + (nk) * ((j) + (nj) * (i)) )


/* --------- Absolute value, min, and max.  Bear in mind that these
             may evaluate an expression multiple times, i.e., ABS( x - y ),
             and therefore may be inefficient, or incorrect,
             i.e, ABS( ++x );                          ------------------ */

#define  VIO_ABS( x )   ( ((x) > 0) ? (x) : (-(x)) )
#define  VIO_FABS( x )  fabs( (double) x )
#define  VIO_SIGN( x )  ( ((x) > 0) ? 1 : (((x) < 0) ? -1 : 0) )
#define  VIO_FSIGN( x ) ( ((x) > 0.0) ? 1.0 : (((x) < 0.0) ? -1.0 : 0.0) )

#ifdef   MAX
#undef   MAX
#endif
#define  MAX( x, y )  ( ((x) >= (y)) ? (x) : (y) )

#define  MAX3( x, y, z )  ( ((x) >= (y)) ? MAX( x, z ) : MAX( y, z ) )

#ifdef   MIN
#undef   MIN
#endif
#define  MIN( x, y )  ( ((x) <= (y)) ? (x) : (y) )

#define  MIN3( x, y, z )  ( ((x) <= (y)) ? MIN( x, z ) : MIN( y, z ) )


#define  VIO_IS_INT( x )    ((double) (x) == (double) ((int) (x)))

#define  VIO_FLOOR( x )     ((long) floor(x))

#define  VIO_ROUND( x )     VIO_FLOOR( (double) (x) + 0.5 )

#define  VIO_CEILING( x )   ((long) ceil(x))

#define  VIO_FRACTION( x )  ((double) (x) - (double) VIO_FLOOR(x))

#define  VIO_ENV_EXISTS( env ) ( getenv(env) != (char *) 0 )

/* --------- macro to determine the size of a static array,
             e.g.,   int  array[] = { 1, 3, 9, 5 };           ------------ */

#define  VIO_SIZEOF_STATIC_ARRAY( array ) \
         (int) ( sizeof(array) / sizeof((array)[0]))

/* --------- interpolate between a and b ------------------- */

#define  VIO_INTERPOLATE( alpha, a, b ) ((a) + (alpha) * ((b) - (a)))


#define  VIO_DEG_TO_RAD   (M_PI / 180.0)
#define  VIO_RAD_TO_DEG   (180.0 / M_PI)


/* for loops */

#define  for_less( i, start, end )  for( (i) = (start);  (i) < (end);  ++(i) )

#define  for_down( i, start, end )  for( (i) = (start);  (i) >= (end); --(i))

#define  for_inclusive( i, start, end )  \
                   for( (i) = (start);  (i) <= (end);  ++(i) )

#define  for_enum( e, max, type )  \
                for( (e) = (type) 0;  (e) < (max);  (e) = (type) ((int) (e)+1) )

#define  CONVERT_INTEGER_RANGE( x1, min1, max1, min2, max2 )                  \
              ((min2) + (2 * (x1) + 1 - 2 * (min1)) * ((max2) - (min2) + 1) / \
                                                      ((max1) - (min1) + 1) / 2)

#define  HANDLE_INTERNAL_ERROR( X )                                           \
         handle_internal_error( X )

#endif /* VOL_IO_BASIC_H */

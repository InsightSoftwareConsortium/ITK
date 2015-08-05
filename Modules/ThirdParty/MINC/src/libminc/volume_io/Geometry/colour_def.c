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
---------------------------------------------------------------------------- */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif /*HAVE_CONFIG_H*/

#include  <internal_volume_io.h>

/* ----------------------------- MNI Header -----------------------------------
@NAME       : make_rgba_Colour
@INPUT      : r
              g
              b
              a
@OUTPUT     : 
@RETURNS    : VIO_Colour
@DESCRIPTION: Packs the four components into a colour.  Each component must
              be in the range 0 to 255.  Depending on what graphics library
              is being linked with, if any, for instance, GL or OpenGL,
              this library function may be overridden by another to define
              the correct method of packing bytes into a colour.  If no
              graphics are involved, then the byte order does not matter
              and the code here can be used.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 10, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  VIO_Colour  make_rgba_Colour(
    int    r,
    int    g,
    int    b,
    int    a )
{
    VIO_Colour          c;
    unsigned  char  *byte_ptr;

    c = 0;    /* to avoid used-before-set compiler messages */
    ASSIGN_PTR(byte_ptr) = (void *) &c;

    byte_ptr[0] = (unsigned char) a;
    byte_ptr[1] = (unsigned char) b;
    byte_ptr[2] = (unsigned char) g;
    byte_ptr[3] = (unsigned char) r;

    return( c );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_Colour_r
@INPUT      : colour
@OUTPUT     : 
@RETURNS    : red component
@DESCRIPTION: Returns the red component of the colour in the range 0 to 255.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 10, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  int  get_Colour_r(
    VIO_Colour   colour )
{
    unsigned  char  *b;

    ASSIGN_PTR(b) = (void *) &colour;

    return( (int) b[3] );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_Colour_g
@INPUT      : colour
@OUTPUT     : 
@RETURNS    : green component
@DESCRIPTION: Returns the green component of the colour in the range 0 to 255.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 10, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  int  get_Colour_g(
    VIO_Colour   colour )
{
    unsigned  char  *b;

    ASSIGN_PTR(b) = (void *) &colour;

    return( (int) b[2] );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_Colour_b
@INPUT      : colour
@OUTPUT     : 
@RETURNS    : blue component
@DESCRIPTION: Returns the blue component of the colour in the range 0 to 255.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 10, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  int  get_Colour_b(
    VIO_Colour   colour )
{
    unsigned  char  *b;

    ASSIGN_PTR(b) = (void *) &colour;

    return( (int) b[1] );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : get_Colour_a
@INPUT      : colour
@OUTPUT     : 
@RETURNS    : alpha component
@DESCRIPTION: Returns the alpha (opacity) component of the colour in the
              range 0 to 255.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : May 10, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  int  get_Colour_a(
    VIO_Colour   colour )
{
    unsigned  char  *b;

    ASSIGN_PTR(b) = (void *) &colour;

    return( (int) b[0] );
}

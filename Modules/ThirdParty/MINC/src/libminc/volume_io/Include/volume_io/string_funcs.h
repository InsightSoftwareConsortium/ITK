#ifndef VOL_IO_STRING_H
#define VOL_IO_STRING_H

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
@VERSION    : $Header: /private-cvsroot/minc/volume_io/Include/volume_io/string_funcs.h,v 1.9 2005-05-19 21:19:28 bert Exp $
---------------------------------------------------------------------------- */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : string_funcs.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Macros for string manipulations
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#include  <string.h>

#define  VIO_EXTREMELY_LARGE_STRING_SIZE  10000

#define  VIO_END_OF_STRING                  ((char) 0)

#define  VIO_COPY_MEMORY( dest, src, n_items )                                  \
         (void) memcpy( (void *) (dest), (void *) (src),                    \
                        (size_t) (n_items) * sizeof((src)[0]) )

#endif /* VOL_IO_STRING_H */

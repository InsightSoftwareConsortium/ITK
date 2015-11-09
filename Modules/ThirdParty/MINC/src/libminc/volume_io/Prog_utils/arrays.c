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
@NAME       : set_array_size
@INPUT      : array
              type_size
              previous_n_elems
              new_n_elems       - desired new array size
              chunk_size
              filename
              line_number
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Sets the number of items allocated in the array to a multiple of
            : chunk_size larger than new_n_elems
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  set_array_size(
    void      **array,
    size_t    type_size,
    size_t    previous_n_elems,
    size_t    new_n_elems,
    size_t    chunk_size
    _ALLOC_SOURCE_LINE_ARG_DEF )
{
    size_t  new_chunk, previous_chunk;

    if( new_n_elems != 0 )
    {
        new_chunk = ((new_n_elems+chunk_size-1) / chunk_size) * chunk_size;
        if( previous_n_elems == 0 )
        {
            *array = alloc_memory_1d( new_chunk, type_size
                                      _ALLOC_SOURCE_LINE_ARGUMENTS );
        }
        else
        {
            previous_chunk = ((previous_n_elems+chunk_size-1) / chunk_size) *
                             chunk_size;

            if( new_chunk != previous_chunk )
                realloc_memory( array, new_chunk, type_size
                                 _ALLOC_SOURCE_LINE_ARGUMENTS );
        }
    }
    else if( previous_n_elems != 0 )
        free_memory_1d( array _ALLOC_SOURCE_LINE_ARGUMENTS );
}

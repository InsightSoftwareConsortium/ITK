#ifndef VOL_IO_ARRAYS_H
#define VOL_IO_ARRAYS_H

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
@VERSION    : $Header: /private-cvsroot/minc/volume_io/Include/volume_io/arrays.h,v 1.13 2004-10-04 20:23:51 bert Exp $
---------------------------------------------------------------------------- */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : arrays.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Macros for adding to and deleting from arrays.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#include  <volume_io/alloc.h>

#define  DEFAULT_CHUNK_SIZE    100

/* ----------------------------- MNI Header -----------------------------------
@NAME       : SET_ARRAY_SIZE
@INPUT      : array
              previous_n_elems
              new_n_elems       - desired new array size
              chunk_size
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

#define  SET_ARRAY_SIZE( array, previous_n_elems, new_n_elems, chunk_size )   \
         set_array_size( (void **) (&(array)), sizeof(*(array)),              \
                         (size_t) (previous_n_elems),                         \
                         (size_t) (new_n_elems),                              \
                         (size_t) (chunk_size) _ALLOC_SOURCE_LINE )

/* ----------------------------- MNI Header -----------------------------------
@NAME       : ADD_ELEMENT_TO_ARRAY
@INPUT      : array         : the array to add to
            : n_elems       : current number of items in the array
            : elem_to_add   : the item to add
            : chunk_size    : the chunk_size for allocation
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Adds an element to the end of an array, and increments the n_elems
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  ADD_ELEMENT_TO_ARRAY( array, n_elems, elem_to_add, chunk_size)        \
         {                                                                     \
             SET_ARRAY_SIZE( array, n_elems, (n_elems)+1, chunk_size );        \
             (array) [(n_elems)] = (elem_to_add);                              \
             ++(n_elems);                                                      \
         }

/* ----------------------------- MNI Header -----------------------------------
@NAME       : DELETE_ELEMENT_FROM_ARRAY
@INPUT      : array             : the array to add to
            : n_elems           : current number of items in the array
            : index_to_remove   : the index of the element to delete
            : chunk_size        : the chunk_size for allocation
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Deletes an element from an array, sliding down subsequent
            : elements, and decrements the n_elems
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  DELETE_ELEMENT_FROM_ARRAY( array, n_elems, index_to_remove, chunk_size ) \
     {                                                                        \
         (void) memmove( (void *) ((unsigned long) (array) +                  \
      (unsigned long) (index_to_remove) * (unsigned long) sizeof(*(array))),  \
      (void *) ((unsigned long) (array) +                                     \
     ((unsigned long) (index_to_remove)+1)* (unsigned long) sizeof(*(array))),\
   ((size_t) (n_elems) - (size_t) (index_to_remove) - 1) * sizeof(*(array)) );\
                                                                              \
         --(n_elems);                                                         \
                                                                              \
         SET_ARRAY_SIZE( array, (n_elems)+1, n_elems, chunk_size);            \
     }

/* ----------------------------- MNI Header -----------------------------------
@NAME       : ADD_ELEMENT_TO_ARRAY_WITH_SIZE
@INPUT      : array
            : n_alloced
            : n_elems
            : elem_to_add
            : chunk_size
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Adds an element to an array where a separate n_allocated and 
            : n_elems is maintained.  n_allocated will always be greater than
            : or equal to n_elems.  This routine is useful so that you don't
            : have to call SET_ARRAY_SIZE everytime you remove an element,
            : as in done in DELETE_ELEMENT_FROM_ARRAY
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  ADD_ELEMENT_TO_ARRAY_WITH_SIZE( array, n_alloced, n_elems, elem_to_add, chunk_size )                                                         \
         {                                                                    \
             if( (n_elems) >= (n_alloced) )                                   \
             {                                                                \
                 SET_ARRAY_SIZE( array, n_alloced, (n_elems) + 1, chunk_size );\
                 (n_alloced) = (n_elems)+1;                                   \
             }                                                                \
             (array) [(n_elems)] = (elem_to_add);                             \
             ++(n_elems);                                                     \
         }

#endif /* VOL_IO_ARRAYS_H */

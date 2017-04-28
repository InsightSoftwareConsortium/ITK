#ifndef VOL_IO_ALLOC_H
#define VOL_IO_ALLOC_H

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
@VERSION    : $Header: /private-cvsroot/minc/volume_io/Include/volume_io/alloc.h,v 1.19 2005-05-19 21:19:27 bert Exp $
---------------------------------------------------------------------------- */

/* ----------------------------- MNI Header -----------------------------------
@NAME       : alloc.h
@INPUT      : 
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: A set of macros for allocating 1, 2, and 3 dimensional arrays.
@METHOD     : Requires the file alloc.c linked in.
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#include  <volume_io/basic.h>
#include  <stdlib.h>

#define  _ALLOC_SOURCE_LINE
#define  _ALLOC_SOURCE_LINE_ARG_DEF
#define  _ALLOC_SOURCE_LINE_ARGUMENTS
#define  PRINT_ALLOC_SOURCE_LINE

#if 0
#define  _ALLOC_SOURCE_LINE    , __FILE__, __LINE__
#define  _ALLOC_SOURCE_LINE_ARG_DEF   , char  filename[], int line_number
#define  _ALLOC_SOURCE_LINE_ARGUMENTS   , filename, line_number
#define  PRINT_ALLOC_SOURCE_LINE   print_alloc_source_line( filename, line_number );
#endif


/* ----------------------------- MNI Header -----------------------------------
@NAME       : ALLOC
@INPUT      : n_items
@OUTPUT     : ptr
@RETURNS    : 
@DESCRIPTION: Macro to allocate n_items of the type ptr points to, assigning
            : ptr.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  ALLOC( ptr, n_items )                                                \
             ASSIGN_PTR(ptr) =                                                \
                     alloc_memory_1d( (size_t) (n_items),                     \
                                      sizeof(*(ptr)) _ALLOC_SOURCE_LINE )

/* ----------------------------- MNI Header -----------------------------------
@NAME       : FREE
@INPUT      : ptr
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Macro to FREE the ptr.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  FREE( ptr )                                                          \
         free_memory_1d( (void **) &(ptr) _ALLOC_SOURCE_LINE )

/* ----------------------------- MNI Header -----------------------------------
@NAME       : REALLOC
@INPUT      : ptr
            : n_items
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Macro to change the number of items that ptr points to, assigning
            : ptr.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  REALLOC( ptr, n_items )                                              \
           realloc_memory( (void **) &(ptr), (size_t) (n_items),              \
                           sizeof(*(ptr)) _ALLOC_SOURCE_LINE )

/* ----------------------------- MNI Header -----------------------------------
@NAME       : ALLOC_VAR_SIZED_STRUCT
@INPUT      : element_type
            : n_elements
@OUTPUT     : 
            : ptr
@RETURNS    : 
@DESCRIPTION: Macro to allocate a structure with a variable size, assigning
            : ptr.
            : To use this, the variable length array must be the last element
            : of the structure.
            :
            : Use:
            :    typedef struct
            :    {
            :       int      n_items;
            :       double   variable_length_list[1];
            :    } var_struct;
            :
            :    var_struct   *s_ptr;
            :
            :    ALLOC_VAR_SIZED_STRUCT( s_ptr, double, 15 );
            :
            :    s->n_items = 15;
            :    s->variable_length_list[0] = 1.0;
            :    s->variable_length_list[1] = -2.0;
            :            ...
            :            ...
            :    s->variable_length_list[13] = 1456.0;
            :    s->variable_length_list[14] = 1234.0;
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  ALLOC_VAR_SIZED_STRUCT( ptr, element_type, n_elements )              \
  ASSIGN_PTR(ptr) = alloc_memory_in_bytes(                                    \
  (size_t) (sizeof(*(ptr))+((size_t)(n_elements)-1) * sizeof(element_type))   \
  _ALLOC_SOURCE_LINE )

/* ----------------------------- MNI Header -----------------------------------
@NAME       : VIO_ALLOC2D
@INPUT      : n1
            : n2
@OUTPUT     : 
            : ptr
@RETURNS    : 
@DESCRIPTION: Macro to allocate an n1 by n2 array, assigning : ptr.
@METHOD     : Allocates a single chunk size n1 by n2, and a list of size n1
            : pointers, which are each assigned to point into the single
            : chunk.  Therefore, only 2 malloc's are required.
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  VIO_ALLOC2D( ptr, n1, n2 )                                               \
   ASSIGN_PTR(ptr) = alloc_memory_2d( (size_t) (n1), (size_t) (n2),           \
                          sizeof(**(ptr)) _ALLOC_SOURCE_LINE )

/* ----------------------------- MNI Header -----------------------------------
@NAME       : VIO_FREE2D
@INPUT      : ptr
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Macro to free the 2 dimensional array.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  VIO_FREE2D( ptr )                                                        \
         free_memory_2d( (void ***) &(ptr) _ALLOC_SOURCE_LINE )

/* ----------------------------- MNI Header -----------------------------------
@NAME       : VIO_ALLOC3D
@INPUT      : n1
            : n2
            : n3
@OUTPUT     : 
            : ptr
@RETURNS    : 
@DESCRIPTION: Macro to allocate an n1 by n2 by n3 array, assigning : ptr.
@METHOD     : Similar to VIO_ALLOC2D, this requires only 3 mallocs.
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  VIO_ALLOC3D( ptr, n1, n2, n3 )                                           \
         ASSIGN_PTR(ptr) = alloc_memory_3d( (size_t) (n1), (size_t) (n2),     \
                          (size_t) (n3), sizeof(***(ptr)) _ALLOC_SOURCE_LINE )

/* ----------------------------- MNI Header -----------------------------------
@NAME       : VIO_FREE3D
@INPUT      : ptr
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees a 3 dimensional array.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  VIO_FREE3D( ptr )                                                        \
         free_memory_3d( (void ****) &(ptr) _ALLOC_SOURCE_LINE )

/* ----------------------------- MNI Header -----------------------------------
@NAME       : ALLOC4D
@INPUT      : n1
            : n2
            : n3
            : n4
@OUTPUT     : 
            : ptr
@RETURNS    : 
@DESCRIPTION: Macro to allocate an n1 by n2 by n3 by n4 array, assigning : ptr.
@METHOD     : Similar to VIO_ALLOC2D, this requires only 4 mallocs.
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  VIO_ALLOC4D( ptr, n1, n2, n3, n4 )                                       \
       ASSIGN_PTR(ptr) = alloc_memory_4d( (size_t) (n1), (size_t) (n2),       \
                          (size_t) (n3), (size_t) (n4),                       \
                          sizeof(****(ptr)) _ALLOC_SOURCE_LINE )

/* ----------------------------- MNI Header -----------------------------------
@NAME       : FREE4D
@INPUT      : ptr
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees a 4 dimensional array.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  VIO_FREE4D( ptr )                                                        \
         free_memory_4d( (void *****) &(ptr) _ALLOC_SOURCE_LINE )

/* ----------------------------- MNI Header -----------------------------------
@NAME       : ALLOC5D
@INPUT      : n1
            : n2
            : n3
            : n4
            : n5
@OUTPUT     : 
            : ptr
@RETURNS    : 
@DESCRIPTION: Macro to allocate an n1 by n2 by n3 by n4 by n5 array, assigning
            : ptr.
@METHOD     : Similar to VIO_ALLOC2D, this requires only 5 mallocs.
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  VIO_ALLOC5D( ptr, n1, n2, n3, n4, n5 )                                   \
     ASSIGN_PTR(ptr) = alloc_memory_5d( (size_t) (n1), (size_t) (n2),         \
                          (size_t) (n3), (size_t) (n4), (size_t) (n5),        \
                          sizeof(*****(ptr)) _ALLOC_SOURCE_LINE )

/* ----------------------------- MNI Header -----------------------------------
@NAME       : FREE5D
@INPUT      : ptr
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees a 5 dimensional array.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

#define  VIO_FREE5D( ptr )                                                        \
         free_memory_5d( (void ******) &(ptr) _ALLOC_SOURCE_LINE )

#endif /*VOL_IO_ALLOC_H*/

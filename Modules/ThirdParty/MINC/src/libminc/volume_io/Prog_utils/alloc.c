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
#include  <stdlib.h>

/* ----------------------------- MNI Header -----------------------------------
@NAME       : set_up_array_pointers_2D
@INPUT      : ptr
              n1
              n2
              type_size
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Given a pointer allocated for 2D, creates the 1st level
              pointers.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug. 2, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  set_up_array_pointers_2D(
    void      **ptr,
    size_t    n1,
    size_t    n2,
    size_t    type_size )
{
    size_t   i;

    for_less( i, 1, n1 )
        ptr[i] = (void *) ((long) ptr[i-1] + (long) n2* (long) type_size);
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : private_alloc_memory
@INPUT      : n_bytes
@OUTPUT     : ptr
@RETURNS    : 
@DESCRIPTION: Allocates the specified number of bytes.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug.  2, 1995        David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_Status  private_alloc_memory(
    void         **ptr,
    size_t       n_bytes )
{
    if( n_bytes != 0 )
    {
        *ptr = (void *) malloc( n_bytes );

        if( *ptr == NULL )
            return( VIO_ERROR );
    }
    else
        *ptr = NULL;

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : private_alloc_memory_2d
@INPUT      : n1
              n2
              type_size
@OUTPUT     : ptr
@RETURNS    : 
@DESCRIPTION: Allocates the specified number of 2d elements.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug.  2, 1995        David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  VIO_Status  private_alloc_memory_2d(
    void         ***ptr,
    size_t       n1,
    size_t       n2,
    size_t       type_size )
{
    if( private_alloc_memory( (void **) ptr, n1 * sizeof(**ptr) ) != VIO_OK ||
        private_alloc_memory( *ptr, n1 * n2 * type_size ) != VIO_OK )
    {
        return( VIO_ERROR );
    }

    set_up_array_pointers_2D( *ptr, n1, n2, type_size );

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : private_alloc_memory_3d
@INPUT      : n1
              n2
              n3
              type_size
@OUTPUT     : ptr
@RETURNS    : 
@DESCRIPTION: Allocates the specified number of 3d elements.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug.  2, 1995        David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static VIO_Status private_alloc_memory_3d(
    void         ****ptr,
    size_t       n1,
    size_t       n2,
    size_t       n3,
    size_t       type_size )
{
    if( private_alloc_memory_2d( (void ***) ptr, n1, n2, sizeof(***ptr) )!= VIO_OK||
        private_alloc_memory( **ptr, n1 * n2 * n3 * type_size ) != VIO_OK)
    {
        return( VIO_ERROR );
    }

    set_up_array_pointers_2D( **ptr, n1 * n2, n3, type_size );

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : private_alloc_memory_4d
@INPUT      : n1
              n2
              n3
              n4
              type_size
@OUTPUT     : ptr
@RETURNS    : 
@DESCRIPTION: Allocates the specified number of 4d elements.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug.  2, 1995        David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static VIO_Status private_alloc_memory_4d(
    void         *****ptr,
    size_t       n1,
    size_t       n2,
    size_t       n3,
    size_t       n4,
    size_t       type_size )
{
    if( private_alloc_memory_3d( (void ****) ptr, n1, n2, n3,
                                 sizeof(****ptr) )!= VIO_OK ||
        private_alloc_memory( ***ptr, n1 * n2 * n3 * n4 * type_size ) != VIO_OK )
    {
        return( VIO_ERROR );
    }

    set_up_array_pointers_2D( ***ptr, n1 * n2 * n3, n4, type_size );

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : private_alloc_memory_5d
@INPUT      : n1
              n2
              n3
              n4
              n5
              type_size
@OUTPUT     : ptr
@RETURNS    : 
@DESCRIPTION: Allocates the specified number of 5d elements.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug.  2, 1995        David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static VIO_Status private_alloc_memory_5d(
    void         ******ptr,
    size_t       n1,
    size_t       n2,
    size_t       n3,
    size_t       n4,
    size_t       n5,
    size_t       type_size )
{
    if( private_alloc_memory_4d( (void *****) ptr, n1, n2, n3, n4,
                                 sizeof(*****ptr) )!= VIO_OK ||
        private_alloc_memory( ****ptr, n1 * n2 * n3 * n4 * n5 * type_size )!=VIO_OK)
    {
        return( VIO_ERROR );
    }

    set_up_array_pointers_2D( ****ptr, n1 * n2 * n3 * n4, n5, type_size );

    return( VIO_OK );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : alloc_memory_in_bytes
@INPUT      : n_bytes
              filename
              line_number
@OUTPUT     : 
@RETURNS    : void *
@DESCRIPTION: Allocates the specified amount of memory, and if successful,
              calls the routine to record the memory allocated, and returns
              the pointer.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug. 2, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  *alloc_memory_in_bytes(
    size_t       n_bytes
    _ALLOC_SOURCE_LINE_ARG_DEF )
{
    void         *ptr;

    if( private_alloc_memory( &ptr, n_bytes ) != VIO_OK )
    {
        print_error( "Cannot alloc 1D array of %lu bytes.\n", n_bytes );
        PRINT_ALLOC_SOURCE_LINE
        abort_if_allowed();
    }
#if 0    
#ifndef  NO_DEBUG_ALLOC
    else
        record_ptr_alloc_check( ptr, n_bytes, filename, line_number );
#endif
#endif    

    return( ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : alloc_memory_1d
@INPUT      : n_elements
              type_size
              filename
              line_number
@OUTPUT     : 
@RETURNS    : void *
@DESCRIPTION: Allocates a 1D array and returns it.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug. 2, 1995    David MacDonald
@MODIFIED   : Apr. 16, 1996   D. MacDonald    : returns the pointer
---------------------------------------------------------------------------- */

VIOAPI  void  *alloc_memory_1d(
    size_t       n_elements,
    size_t       type_size
    _ALLOC_SOURCE_LINE_ARG_DEF )
{
    void   *ptr;

    if( private_alloc_memory( &ptr, n_elements * type_size ) != VIO_OK )
    {
        print_error( "Cannot alloc 1D array of %lu elements of %lu bytes.\n",
                     n_elements, type_size );
        PRINT_ALLOC_SOURCE_LINE
        abort_if_allowed();
    }
#if 0    
#ifndef  NO_DEBUG_ALLOC
    else
        record_ptr_alloc_check( ptr, n_elements * type_size,
                                filename, line_number );
#endif
#endif

    return( ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : alloc_memory_2d
@INPUT      : n1
              n2
              type_size
              filename
              line_number
@OUTPUT     : 
@RETURNS    : void *
@DESCRIPTION: Allocates a 2D array and returns a pointer to it.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug. 2, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  *alloc_memory_2d(
    size_t       n1,
    size_t       n2,
    size_t       type_size
    _ALLOC_SOURCE_LINE_ARG_DEF )
{
    void   **ptr;

    if( private_alloc_memory_2d( &ptr, n1, n2, type_size ) != VIO_OK )
    {
        print_error( "Cannot alloc 2D array of %lu by %lu elements of %lu bytes.\n",
                     n1, n2, type_size );
        PRINT_ALLOC_SOURCE_LINE
        abort_if_allowed();
    }
#if 0    
#ifndef  NO_DEBUG_ALLOC
    else
    {
        record_ptr_alloc_check( ptr, n1 * sizeof(*ptr),
                                filename, line_number );
        record_ptr_alloc_check( *ptr, n1 * n2 * type_size,
                                filename, line_number );
    }
#endif
#endif 
    return( (void *) ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : alloc_memory_3d
@INPUT      : n1
              n2
              n3
              type_size
              filename
              line_number
@OUTPUT     : 
@RETURNS    : void *
@DESCRIPTION: Allocates a 3D array.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug. 2, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  *alloc_memory_3d(
    size_t       n1,
    size_t       n2,
    size_t       n3,
    size_t       type_size
    _ALLOC_SOURCE_LINE_ARG_DEF )
{
    void         ***ptr;

    if( private_alloc_memory_3d( &ptr, n1, n2, n3, type_size ) != VIO_OK )
    {
        print_error( "Cannot alloc 3D array of %lu by %lu by %lu elements of %lu bytes.\n",
                     n1, n2, n3, type_size );
        PRINT_ALLOC_SOURCE_LINE
        abort_if_allowed();
    }
#if 0    
#ifndef  NO_DEBUG_ALLOC
    else
    {
        record_ptr_alloc_check( ptr, n1 * sizeof(*ptr),
                                filename, line_number );
        record_ptr_alloc_check( *ptr, n1 * n2 * sizeof(**ptr),
                                filename, line_number );
        record_ptr_alloc_check( **ptr, n1 * n2 * n3 * type_size,
                                filename, line_number );
    }
#endif
#endif

    return( (void *) ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : alloc_memory_4d
@INPUT      : n1
              n2
              n3
              n4
              type_size
              filename
              line_number
@OUTPUT     : 
@RETURNS    : void *
@DESCRIPTION: Allocates a 4D array.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug. 2, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  *alloc_memory_4d(
    size_t       n1,
    size_t       n2,
    size_t       n3,
    size_t       n4,
    size_t       type_size
    _ALLOC_SOURCE_LINE_ARG_DEF )
{
    void         ****ptr;

    if( private_alloc_memory_4d( &ptr, n1, n2, n3, n4, type_size ) != VIO_OK )
    {
        print_error( "Cannot alloc 4D array of %lu by %lu by %lu by %lu elements of %lu bytes.\n",
                     n1, n2, n3, n4, type_size );
        PRINT_ALLOC_SOURCE_LINE
        abort_if_allowed();
    }
#if 0    
#ifndef  NO_DEBUG_ALLOC
    else
    {
        record_ptr_alloc_check( ptr, n1 * sizeof(*ptr),
                                filename, line_number );
        record_ptr_alloc_check( *ptr, n1 * n2 * sizeof(**ptr),
                                filename, line_number );
        record_ptr_alloc_check( **ptr, n1 * n2 * n3 * sizeof(***ptr),
                                filename, line_number );
        record_ptr_alloc_check( ***ptr, n1 * n2 * n3 * n4 * type_size,
                                filename, line_number );
    }
#endif
#endif
    return( (void *) ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : alloc_memory_5d
@INPUT      : n1
              n2
              n3
              n4
              n5
              type_size
              filename
              line_number
@OUTPUT     : 
@RETURNS    : void *
@DESCRIPTION: Allocates a 5D array.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug. 2, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  *alloc_memory_5d(
    size_t       n1,
    size_t       n2,
    size_t       n3,
    size_t       n4,
    size_t       n5,
    size_t       type_size
    _ALLOC_SOURCE_LINE_ARG_DEF )
{
    void         *****ptr;

    if( private_alloc_memory_5d( &ptr, n1, n2, n3, n4, n5, type_size ) != VIO_OK )
    {
        print_error( "Cannot alloc 4D array of %lu by %lu by %lu by %lu by %lu elements of %lu bytes.\n",
                     n1, n2, n3, n4, n5, type_size );
        PRINT_ALLOC_SOURCE_LINE
        abort_if_allowed();
    }
#if 0    
#ifndef  NO_DEBUG_ALLOC
    else
    {
        record_ptr_alloc_check( ptr, n1 * sizeof(*ptr),
                                filename, line_number );
        record_ptr_alloc_check( *ptr, n1 * n2 * sizeof(**ptr),
                                filename, line_number );
        record_ptr_alloc_check( **ptr, n1 * n2 * n3 * sizeof(***ptr),
                                filename, line_number );
        record_ptr_alloc_check( ***ptr, n1 * n2 * n3 * n4 * sizeof(****ptr),
                                filename, line_number );
        record_ptr_alloc_check( ****ptr, n1 * n2 * n3 * n4 * n5 * type_size,
                                filename, line_number );
    }
#endif
#endif 
    return( (void *) ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : realloc_memory
@INPUT      : ptr
              n_elements
              type_size
              filename
              line_number
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Reallocates the ptr.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  realloc_memory(
    void      **ptr,
    size_t    n_elements,
    size_t    type_size
    _ALLOC_SOURCE_LINE_ARG_DEF )
{
#ifndef  NO_DEBUG_ALLOC
    void   *old_ptr = *ptr;
#endif

    if( n_elements != 0 )
    {
        *ptr = (void *) realloc( *ptr, n_elements * type_size );

        if( *ptr == NULL )
        {
            print_error( "Error reallocing %lu elements of size %lu.\n",
                         n_elements, type_size );
            PRINT_ALLOC_SOURCE_LINE
            abort_if_allowed();
        }
#if 0
#ifndef  NO_DEBUG_ALLOC
        change_ptr_alloc_check( old_ptr, *ptr, n_elements * type_size,
                                filename, line_number );
#endif
#endif
    }
    else
    {
        print_error("Error: tried to realloc invalid number of elements, %lu.\n",
                     n_elements );
        PRINT_ALLOC_SOURCE_LINE
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : private_free_memory_1d
@INPUT      : ptr
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees the array and assigns the pointer to NULL.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug.  2, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  private_free_memory_1d(
    void   **ptr)
{
    if( *ptr != NULL )
    {
        free( *ptr );

        *ptr = NULL;
    }
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : private_free_memory_2d
@INPUT      : ptr
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees the array and assigns the pointer to NULL.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug.  2, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  private_free_memory_2d(
    void   ***ptr)
{
    private_free_memory_1d( *ptr );
    private_free_memory_1d( (void **) ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : private_free_memory_3d
@INPUT      : ptr
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees the array and assigns the pointer to NULL.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug.  2, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  private_free_memory_3d(
    void   ****ptr)
{
    private_free_memory_1d( **ptr );
    private_free_memory_2d( (void ***) ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : private_free_memory_4d
@INPUT      : ptr
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees the array and assigns the pointer to NULL.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug.  2, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  private_free_memory_4d(
    void   *****ptr)
{
    private_free_memory_1d( ***ptr );
    private_free_memory_3d( (void ****) ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : private_free_memory_5d
@INPUT      : ptr
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees the array and assigns the pointer to NULL.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    : Aug.  2, 1995    David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

static  void  private_free_memory_5d(
    void   ******ptr)
{
    private_free_memory_1d( ****ptr );
    private_free_memory_4d( (void *****) ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : free_memory_1d
@INPUT      : ptr
              filename
              line_number
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees the pointer, and sets it to NIL.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  free_memory_1d(
    void   **ptr
    _ALLOC_SOURCE_LINE_ARG_DEF )
{
#if 0
#ifndef  NO_DEBUG_ALLOC
    if( unrecord_ptr_alloc_check( *ptr, filename, line_number ) )
#endif
#endif
        private_free_memory_1d( ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : free_memory_2d
@INPUT      : ptr
              filename
              line_number
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees the pointer, and sets it to NIL.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  free_memory_2d(
    void   ***ptr
    _ALLOC_SOURCE_LINE_ARG_DEF )
{
#if 0
#ifndef  NO_DEBUG_ALLOC
    if( unrecord_ptr_alloc_check( **ptr, filename, line_number ) &&
        unrecord_ptr_alloc_check( (void *) *ptr, filename, line_number ) )
#endif
#endif  
        private_free_memory_2d( ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : free_memory_3d
@INPUT      : ptr
              filename
              line_number
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees the pointer, and sets it to NIL.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  free_memory_3d(
    void   ****ptr
    _ALLOC_SOURCE_LINE_ARG_DEF )
{
#if 0  
#ifndef  NO_DEBUG_ALLOC
    if( unrecord_ptr_alloc_check( ***ptr, filename, line_number ) &&
        unrecord_ptr_alloc_check( (void *) **ptr, filename, line_number ) &&
        unrecord_ptr_alloc_check( (void *) *ptr, filename, line_number ) )
#endif
#endif  
        private_free_memory_3d( ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : free_memory_4d
@INPUT      : ptr
              filename
              line_number
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees the pointer, and sets it to NIL.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  free_memory_4d(
    void   *****ptr
    _ALLOC_SOURCE_LINE_ARG_DEF )
{
#if 0  
#ifndef  NO_DEBUG_ALLOC
    if( unrecord_ptr_alloc_check( ****ptr, filename, line_number ) &&
        unrecord_ptr_alloc_check( (void *) ***ptr, filename, line_number ) &&
        unrecord_ptr_alloc_check( (void *) **ptr, filename, line_number ) &&
        unrecord_ptr_alloc_check( (void *) *ptr, filename, line_number ) )
#endif
#endif   
        private_free_memory_4d( ptr );
}

/* ----------------------------- MNI Header -----------------------------------
@NAME       : free_memory_5d
@INPUT      : ptr
              filename
              line_number
@OUTPUT     : 
@RETURNS    : 
@DESCRIPTION: Frees the pointer, and sets it to NIL.
@METHOD     : 
@GLOBALS    : 
@CALLS      : 
@CREATED    :                      David MacDonald
@MODIFIED   : 
---------------------------------------------------------------------------- */

VIOAPI  void  free_memory_5d(
    void   ******ptr
    _ALLOC_SOURCE_LINE_ARG_DEF )
{
#if 0  
#ifndef  NO_DEBUG_ALLOC
    if( unrecord_ptr_alloc_check( *****ptr, filename, line_number ) &&
        unrecord_ptr_alloc_check( (void *) ****ptr, filename, line_number ) &&
        unrecord_ptr_alloc_check( (void *) ***ptr, filename, line_number ) &&
        unrecord_ptr_alloc_check( (void *) **ptr, filename, line_number ) &&
        unrecord_ptr_alloc_check( (void *) *ptr, filename, line_number ) )
#endif
#endif   
        private_free_memory_5d( ptr );
}

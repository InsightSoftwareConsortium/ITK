/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5MM.c
 *			Jul 10 1997
 *			Robb Matzke <matzke@llnl.gov>
 *
 * Purpose:		Memory management functions.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */


#include "H5private.h"
#include "H5Eprivate.h"
#include "H5MMprivate.h"


/*-------------------------------------------------------------------------
 * Function:    H5MM_malloc
 *
 * Purpose:     Similar to the C89 version of malloc().
 *
 *              On size of 0, we return a NULL pointer instead of the
 *              standard-allowed 'special' pointer since that's more
 *              difficult to check as a return value. This is still
 *              considered an error condition since allocations of zero
 *              bytes usually indicate problems.
 *  
 * Return:  Success:    Pointer new memory
 *
 *          Failure:	NULL
 *
 * Programmer:  Quincey Koziol
 *              Nov  8 2003
 *
 *-------------------------------------------------------------------------
 */
void *
H5MM_malloc(size_t size)
{
    void *ret_value;

    HDassert(size);

    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(size)
        ret_value = HDmalloc(size);
    else
        ret_value = NULL;

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5MM_malloc() */


/*-------------------------------------------------------------------------
 * Function:    H5MM_calloc
 *
 * Purpose:     Similar to the C89 version of calloc(), except this
 *              routine just takes a 'size' parameter.
 *
 *              On size of 0, we return a NULL pointer instead of the
 *              standard-allowed 'special' pointer since that's more
 *              difficult to check as a return value. This is still
 *              considered an error condition since allocations of zero
 *              bytes usually indicate problems.
 *
 *
 * Return:  Success:    Pointer new memory
 *
 *          Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Nov  8 2003
 *
 *-------------------------------------------------------------------------
 */
void *
H5MM_calloc(size_t size)
{
    void *ret_value;

    HDassert(size);

    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(size)
        ret_value = HDcalloc((size_t)1, size);
    else
        ret_value = NULL;

    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5MM_calloc() */


/*-------------------------------------------------------------------------
 * Function:    H5MM_realloc
 *
 * Purpose:     Similar semantics as C89's realloc(). Specifically, the
 *              following calls are equivalent:
 *
 *              H5MM_realloc(NULL, size)    <==> H5MM_malloc(size)
 *              H5MM_realloc(ptr, 0)        <==> H5MM_xfree(ptr)
 *              H5MM_realloc(NULL, 0)       <==> NULL
 *
 *              Note that the (NULL, 0) combination is undefined behavior
 *              in the C standard.
 *
 * Return:  Success:    Ptr to new memory if size > 0
 *                      NULL if size is zero
 *
 *          Failure:    NULL (input buffer is unchanged on failure)
 *
 * Programmer:  Robb Matzke
 *              Jul 10 1997
 *
 *-------------------------------------------------------------------------
 */
void *
H5MM_realloc(void *mem, size_t size)
{
    void *ret_value;

    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(mem || size);

    if(NULL == mem && 0 == size) {  
        /* Not defined in the standard, return NULL */
        ret_value = NULL;
    }
    else {
        ret_value = HDrealloc(mem, size);

        /* Some platforms do not return NULL if size is zero. */
        if(0 == size)
            ret_value = NULL;
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MM_realloc() */


/*-------------------------------------------------------------------------
 * Function:    H5MM_xstrdup
 *
 * Purpose:     Duplicates a string, including memory allocation.
 *              NULL is an acceptable value for the input string.
 *
 * Return:      Success:    Pointer to a new string (NULL if s is NULL).
 *
 *              Failure:    abort()
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 10 1997
 *-------------------------------------------------------------------------
 */
char *
H5MM_xstrdup(const char *s)
{
    char	*ret_value = NULL;

    FUNC_ENTER_NOAPI(NULL)

    if(s) {
        if(NULL == (ret_value = (char *)H5MM_malloc(HDstrlen(s) + 1)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        HDstrcpy(ret_value, s);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MM_xstrdup() */


/*-------------------------------------------------------------------------
 * Function:    H5MM_strdup
 *
 * Purpose:     Duplicates a string, including memory allocation.
 *              NULL is NOT an acceptable value for the input string.
 *
 *              If the string to be duplicated is the NULL pointer, then
 *              an error will be raised.
 *
 * Return:      Success:    Pointer to a new string
 *
 *              Failure:    abort()
 *
 * Programmer:  Robb Matzke
 *              matzke@llnl.gov
 *              Jul 10 1997
 *-------------------------------------------------------------------------
 */
char *
H5MM_strdup(const char *s)
{
    char	*ret_value;

    FUNC_ENTER_NOAPI(NULL)

    if(!s)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "null string")
    if(NULL == (ret_value = (char *)H5MM_malloc(HDstrlen(s) + 1)))
	HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    HDstrcpy(ret_value, s);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5MM_strdup() */


/*-------------------------------------------------------------------------
 * Function:	H5MM_xfree
 *
 * Purpose:	Just like free(3) except null pointers are allowed as
 *		arguments, and the return value (always NULL) can be
 *		assigned to the pointer whose memory was just freed:
 *
 *			thing = H5MM_xfree (thing);
 *
 * Return:	Success:	NULL
 *
 *		Failure:	never fails
 *
 * Programmer:	Robb Matzke
 *		matzke@llnl.gov
 *		Jul 10 1997
 *
 *-------------------------------------------------------------------------
 */
void *
H5MM_xfree(void *mem)
{
    /* Use FUNC_ENTER_NOAPI_NOINIT_NOERR here to avoid performance issues */
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(mem)
        HDfree(mem);

    FUNC_LEAVE_NOAPI(NULL);
} /* end H5MM_xfree() */

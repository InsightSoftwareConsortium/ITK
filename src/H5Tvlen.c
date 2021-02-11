/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.  *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*
 * Module Info: This module contains the functionality for variable-length
 *      datatypes in the H5T interface.
 */

/****************/
/* Module Setup */
/****************/

#include "H5Tmodule.h"          /* This source code file is part of the H5T module */

/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions    */
#include "H5CXprivate.h"        /* API Contexts         */
#include "H5Eprivate.h"         /* Error handling       */
#include "H5HGprivate.h"        /* Global Heaps         */
#include "H5Iprivate.h"         /* IDs                  */
#include "H5MMprivate.h"        /* Memory management    */
#include "H5Tpkg.h"             /* Datatypes            */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Memory-based VL sequence callbacks */
static herr_t H5T_vlen_reclaim_recurse(void *elem, const H5T_t *dt, H5MM_free_t free_func, void *free_info);
static ssize_t H5T_vlen_seq_mem_getlen(const void *_vl);
static void * H5T_vlen_seq_mem_getptr(void *_vl);
static htri_t H5T_vlen_seq_mem_isnull(const H5F_t *f, void *_vl);
static herr_t H5T_vlen_seq_mem_read(H5F_t *f, void *_vl, void *_buf, size_t len);
static herr_t H5T_vlen_seq_mem_write(H5F_t *f, const H5T_vlen_alloc_info_t *vl_alloc_info, void *_vl, void *_buf, void *_bg, size_t seq_len, size_t base_size);
static herr_t H5T_vlen_seq_mem_setnull(H5F_t *f, void *_vl, void *_bg);

/* Memory-based VL string callbacks */
static ssize_t H5T_vlen_str_mem_getlen(const void *_vl);
static void * H5T_vlen_str_mem_getptr(void *_vl);
static htri_t H5T_vlen_str_mem_isnull(const H5F_t *f, void *_vl);
static herr_t H5T_vlen_str_mem_read(H5F_t *f, void *_vl, void *_buf, size_t len);
static herr_t H5T_vlen_str_mem_write(H5F_t *f, const H5T_vlen_alloc_info_t *vl_alloc_info, void *_vl, void *_buf, void *_bg, size_t seq_len, size_t base_size);
static herr_t H5T_vlen_str_mem_setnull(H5F_t *f, void *_vl, void *_bg);

/* Disk-based VL sequence (and string) callbacks */
static ssize_t H5T_vlen_disk_getlen(const void *_vl);
static void * H5T_vlen_disk_getptr(void *_vl);
static htri_t H5T_vlen_disk_isnull(const H5F_t *f, void *_vl);
static herr_t H5T_vlen_disk_read(H5F_t *f, void *_vl, void *_buf, size_t len);
static herr_t H5T_vlen_disk_write(H5F_t *f, const H5T_vlen_alloc_info_t *vl_alloc_info, void *_vl, void *_buf, void *_bg, size_t seq_len, size_t base_size);
static herr_t H5T_vlen_disk_setnull(H5F_t *f, void *_vl, void *_bg);

/*********************/
/* Public Variables */
/*********************/


/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:	H5Tvlen_create
 *
 * Purpose:	Create a new variable-length datatype based on the specified
 *		BASE_TYPE.
 *
 * Return:	Success:	ID of new VL datatype
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, May 20, 1999
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Tvlen_create(hid_t base_id)
{
    H5T_t	*base = NULL;		/*base datatype	*/
    H5T_t	*dt = NULL;		/*new datatype	*/
    hid_t	ret_value;	        /*return value			*/

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", base_id);

    /* Check args */
    if(NULL == (base = (H5T_t *)H5I_object_verify(base_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an valid base datatype")

    /* Create up VL datatype */
    if((dt = H5T__vlen_create(base)) == NULL)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "invalid VL location")

    /* Atomize the type */
    if((ret_value = H5I_register(H5I_DATATYPE, dt, TRUE)) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register datatype")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Tvlen_create() */


/*-------------------------------------------------------------------------
 * Function:	H5T__vlen_create
 *
 * Purpose:	Create a new variable-length datatype based on the specified
 *		BASE_TYPE.
 *
 * Return:	Success:	new VL datatype
 *
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November 20, 2001
 *
 *-------------------------------------------------------------------------
 */
H5T_t *
H5T__vlen_create(const H5T_t *base)
{
    H5T_t	*dt = NULL;		/* New VL datatype */
    H5T_t	*ret_value = NULL;	/* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    HDassert(base);

    /* Build new type */
    if(NULL == (dt = H5T__alloc()))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, NULL, "memory allocation failed")
    dt->shared->type = H5T_VLEN;

    /*
     * Force conversions (i.e. memory to memory conversions should duplicate
     * data, not point to the same VL sequences)
     */
    dt->shared->force_conv = TRUE;
    if(NULL == (dt->shared->parent = H5T_copy(base, H5T_COPY_ALL)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTCOPY, NULL, "can't copy base datatype")

    /* Inherit encoding version from base type */
    dt->shared->version = base->shared->version;

    /* This is a sequence, not a string */
    dt->shared->u.vlen.type = H5T_VLEN_SEQUENCE;

    /* Set up VL information */
    if(H5T_set_loc(dt, NULL, H5T_LOC_MEMORY) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "invalid datatype location")

    /* Set return value */
    ret_value = dt;

done:
    if(!ret_value)
        if(dt && H5T_close_real(dt) < 0)
            HDONE_ERROR(H5E_DATATYPE, H5E_CANTRELEASE, NULL, "unable to release datatype info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__vlen_create() */


/*-------------------------------------------------------------------------
 * Function: H5T__vlen_set_loc
 *
 * Purpose:	Sets the location of a VL datatype to be either on disk or in memory
 *
 * Return:
 *  One of two values on success:
 *      TRUE - If the location of any vlen types changed
 *      FALSE - If the location of any vlen types is the same
 *  <0 is returned on failure
 *
 * Programmer:	Quincey Koziol
 *		Friday, June 4, 1999
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5T__vlen_set_loc(const H5T_t *dt, H5F_t *f, H5T_loc_t loc)
{
    htri_t ret_value = FALSE;   /* Indicate success, but no location change */

    FUNC_ENTER_PACKAGE

    /* check parameters */
    HDassert(dt);
    HDassert(loc >= H5T_LOC_BADLOC && loc < H5T_LOC_MAXLOC);

    /* Only change the location if it's different */
    if(loc != dt->shared->u.vlen.loc || f != dt->shared->u.vlen.f) {
        switch(loc) {
            case H5T_LOC_MEMORY:   /* Memory based VL datatype */
                HDassert(NULL == f);

                /* Mark this type as being stored in memory */
                dt->shared->u.vlen.loc = H5T_LOC_MEMORY;

                if(dt->shared->u.vlen.type == H5T_VLEN_SEQUENCE) {
                    /* Size in memory, disk size is different */
                    dt->shared->size = sizeof(hvl_t);

                    /* Set up the function pointers to access the VL sequence in memory */
                    dt->shared->u.vlen.getlen = H5T_vlen_seq_mem_getlen;
                    dt->shared->u.vlen.getptr = H5T_vlen_seq_mem_getptr;
                    dt->shared->u.vlen.isnull = H5T_vlen_seq_mem_isnull;
                    dt->shared->u.vlen.read = H5T_vlen_seq_mem_read;
                    dt->shared->u.vlen.write = H5T_vlen_seq_mem_write;
                    dt->shared->u.vlen.setnull = H5T_vlen_seq_mem_setnull;
                } /* end if */
                else if(dt->shared->u.vlen.type == H5T_VLEN_STRING) {
                    /* Size in memory, disk size is different */
                    dt->shared->size = sizeof(char *);

                    /* Set up the function pointers to access the VL string in memory */
                    dt->shared->u.vlen.getlen = H5T_vlen_str_mem_getlen;
                    dt->shared->u.vlen.getptr = H5T_vlen_str_mem_getptr;
                    dt->shared->u.vlen.isnull = H5T_vlen_str_mem_isnull;
                    dt->shared->u.vlen.read = H5T_vlen_str_mem_read;
                    dt->shared->u.vlen.write = H5T_vlen_str_mem_write;
                    dt->shared->u.vlen.setnull = H5T_vlen_str_mem_setnull;
                } /* end else-if */
                else
                    HDassert(0 && "Invalid VL type");

                /* Reset file pointer (since this VL is in memory) */
                dt->shared->u.vlen.f = NULL;
                break;

            case H5T_LOC_DISK:   /* Disk based VL datatype */
                HDassert(f);

                /* Mark this type as being stored on disk */
                dt->shared->u.vlen.loc = H5T_LOC_DISK;

                /*
                 * Size of element on disk is 4 bytes for the length, plus the size
                 * of an address in this file, plus 4 bytes for the size of a heap
                 * ID.  Memory size is different
                 */
                dt->shared->size = 4 + (size_t)H5F_SIZEOF_ADDR(f) + 4;

                /* Set up the function pointers to access the VL information on disk */
                /* VL sequences and VL strings are stored identically on disk, so use the same functions */
                dt->shared->u.vlen.getlen = H5T_vlen_disk_getlen;
                dt->shared->u.vlen.getptr = H5T_vlen_disk_getptr;
                dt->shared->u.vlen.isnull = H5T_vlen_disk_isnull;
                dt->shared->u.vlen.read = H5T_vlen_disk_read;
                dt->shared->u.vlen.write = H5T_vlen_disk_write;
                dt->shared->u.vlen.setnull = H5T_vlen_disk_setnull;

                /* Set file ID (since this VL is on disk) */
                dt->shared->u.vlen.f = f;
                break;

            case H5T_LOC_BADLOC:
                /* Allow undefined location. In H5Odtype.c, H5O_dtype_decode sets undefined
                 * location for VL type and leaves it for the caller to decide.
                 */
                break;

            case H5T_LOC_MAXLOC:
                /* MAXLOC is invalid */
            default:
                HGOTO_ERROR(H5E_DATATYPE, H5E_BADRANGE, FAIL, "invalid VL datatype location")
        } /* end switch */ /*lint !e788 All appropriate cases are covered */

        /* Indicate that the location changed */
        ret_value = TRUE;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T__vlen_set_loc() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_seq_mem_getlen
 *
 * Purpose:	Retrieves the length of a memory based VL element.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5T_vlen_seq_mem_getlen(const void *_vl)
{
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    const hvl_t *vl=(const hvl_t *)_vl;   /* Pointer to the user's hvl_t information */
#else
    hvl_t vl;             /* User's hvl_t information */
#endif

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check parameters, return result */
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    HDassert(vl);

    FUNC_LEAVE_NOAPI((ssize_t)vl->len)
#else
    HDassert(_vl);
    H5MM_memcpy(&vl, _vl, sizeof(hvl_t));

    FUNC_LEAVE_NOAPI((ssize_t)vl.len)
#endif
}   /* end H5T_vlen_seq_mem_getlen() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_seq_mem_getptr
 *
 * Purpose:	Retrieves the pointer for a memory based VL element.
 *
 * Return:	Non-NULL on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		Saturday, June 12, 2004
 *
 *-------------------------------------------------------------------------
 */
static void *
H5T_vlen_seq_mem_getptr(void *_vl)
{
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    const hvl_t *vl=(const hvl_t *)_vl;   /* Pointer to the user's hvl_t information */
#else
    hvl_t vl;             /* User's hvl_t information */
#endif

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters, return result */
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    HDassert(vl);

    FUNC_LEAVE_NOAPI(vl->p)
#else
    HDassert(_vl);
    H5MM_memcpy(&vl, _vl, sizeof(hvl_t));

    FUNC_LEAVE_NOAPI(vl.p)
#endif
}   /* end H5T_vlen_seq_mem_getptr() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_seq_mem_isnull
 *
 * Purpose:	Checks if a memory sequence is the "null" sequence
 *
 * Return:	TRUE/FALSE on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Saturday, November 8, 2003
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5T_vlen_seq_mem_isnull(const H5F_t H5_ATTR_UNUSED *f, void *_vl)
{
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    const hvl_t *vl=(const hvl_t *)_vl;   /* Pointer to the user's hvl_t information */
#else
    hvl_t vl;             /* User's hvl_t information */
#endif

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters, return result */
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    HDassert(vl);

    FUNC_LEAVE_NOAPI((vl->len==0 || vl->p==NULL) ? TRUE : FALSE)
#else
    HDassert(_vl);
    H5MM_memcpy(&vl, _vl, sizeof(hvl_t));

    FUNC_LEAVE_NOAPI((vl.len==0 || vl.p==NULL) ? TRUE : FALSE)
#endif
} /* end H5T_vlen_seq_mem_isnull() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_seq_mem_read
 *
 * Purpose:	"Reads" the memory based VL sequence into a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_vlen_seq_mem_read(H5F_t H5_ATTR_UNUSED *f, void *_vl, void *buf, size_t len)
{
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    const hvl_t *vl = (const hvl_t *)_vl;   /* Pointer to the user's hvl_t information */
#else
    hvl_t vl;             /* User's hvl_t information */
#endif

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters, copy data */
    HDassert(buf);
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    HDassert(vl && vl->p);

    H5MM_memcpy(buf, vl->p, len);
#else
    HDassert(_vl);
    H5MM_memcpy(&vl, _vl, sizeof(hvl_t));
    HDassert(vl.p);

    H5MM_memcpy(buf, vl.p, len);
#endif

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5T_vlen_seq_mem_read() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_seq_mem_write
 *
 * Purpose:	"Writes" the memory based VL sequence from a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_vlen_seq_mem_write(H5F_t H5_ATTR_UNUSED *f, const H5T_vlen_alloc_info_t *vl_alloc_info, void *_vl, void *buf, void H5_ATTR_UNUSED *_bg, size_t seq_len, size_t base_size)
{
    hvl_t vl;                       /* Temporary hvl_t to use during operation */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check parameters */
    HDassert(_vl);
    HDassert(buf);

    if(seq_len) {
        size_t len = seq_len * base_size;       /* Sequence size */

        /* Use the user's memory allocation routine is one is defined */
        if(vl_alloc_info->alloc_func != NULL) {
            if(NULL == (vl.p = (vl_alloc_info->alloc_func)(len, vl_alloc_info->alloc_info)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "application memory allocation routine failed for VL data")
          } /* end if */
        else    /* Default to system malloc */
            if(NULL == (vl.p = HDmalloc(len)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "memory allocation failed for VL data")

        /* Copy the data into the newly allocated buffer */
        H5MM_memcpy(vl.p, buf, len);
    } /* end if */
    else
        vl.p = NULL;

    /* Set the sequence length */
    vl.len = seq_len;

    /* Set pointer in user's buffer with memcpy, to avoid alignment issues */
    H5MM_memcpy(_vl, &vl, sizeof(hvl_t));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T_vlen_seq_mem_write() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_seq_mem_setnull
 *
 * Purpose:	Sets a VL info object in memory to the "nil" value
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Saturday, November 8, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_vlen_seq_mem_setnull(H5F_t H5_ATTR_UNUSED *f, void *_vl, void H5_ATTR_UNUSED *_bg)
{
    hvl_t vl;                       /* Temporary hvl_t to use during operation */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters */
    HDassert(_vl);

    /* Set the "nil" hvl_t */
    vl.len = 0;
    vl.p = NULL;

    /* Set pointer in user's buffer with memcpy, to avoid alignment issues */
    H5MM_memcpy(_vl, &vl, sizeof(hvl_t));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5T_vlen_seq_mem_setnull() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_str_mem_getlen
 *
 * Purpose:	Retrieves the length of a memory based VL string.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5T_vlen_str_mem_getlen(const void *_vl)
{
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    const char *s = *(const char * const *)_vl;   /* Pointer to the user's string information */
#else
    const char *s;      /* Pointer to the user's string information */
#endif

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters */
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    HDassert(s);
#else
    HDassert(_vl);
    H5MM_memcpy(&s, _vl, sizeof(char *));
#endif

    FUNC_LEAVE_NOAPI((ssize_t)HDstrlen(s))
} /* end H5T_vlen_str_mem_getlen() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_str_mem_getptr
 *
 * Purpose:	Retrieves the pointer for a memory based VL string.
 *
 * Return:	Non-NULL on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		Saturday, June 12, 2004
 *
 *-------------------------------------------------------------------------
 */
static void *
H5T_vlen_str_mem_getptr(void *_vl)
{
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    char *s=*(char **)_vl;   /* Pointer to the user's string information */
#else
    char *s;      /* Pointer to the user's string information */
#endif

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters */
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    HDassert(s);
#else
    HDassert(_vl);
    H5MM_memcpy(&s, _vl, sizeof(char *));
#endif

    FUNC_LEAVE_NOAPI(s)
} /* end H5T_vlen_str_mem_getptr() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_str_mem_isnull
 *
 * Purpose:	Checks if a memory string is a NULL pointer
 *
 * Return:	TRUE/FALSE on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Saturday, November 8, 2003
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5T_vlen_str_mem_isnull(const H5F_t H5_ATTR_UNUSED *f, void *_vl)
{
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    char *s=*(char **)_vl;   /* Pointer to the user's string information */
#else
    char *s;      /* Pointer to the user's string information */
#endif

    FUNC_ENTER_NOAPI_NOINIT_NOERR

#ifndef H5_NO_ALIGNMENT_RESTRICTIONS
    H5MM_memcpy(&s, _vl, sizeof(char *));
#endif

    FUNC_LEAVE_NOAPI(s==NULL ? TRUE : FALSE)
}   /* end H5T_vlen_str_mem_isnull() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_str_mem_read
 *
 * Purpose:	"Reads" the memory based VL string into a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_vlen_str_mem_read(H5F_t H5_ATTR_UNUSED *f, void *_vl, void *buf, size_t len)
{
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
    char *s=*(char **)_vl;   /* Pointer to the user's string information */
#else
    char *s;      /* Pointer to the user's string information */
#endif

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(len > 0) {
        /* check parameters */
        HDassert(buf);
#ifdef H5_NO_ALIGNMENT_RESTRICTIONS
        HDassert(s);
#else
        HDassert(_vl);
        H5MM_memcpy(&s, _vl, sizeof(char *));
#endif

        H5MM_memcpy(buf, s, len);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
}   /* end H5T_vlen_str_mem_read() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_str_mem_write
 *
 * Purpose:	"Writes" the memory based VL string from a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_vlen_str_mem_write(H5F_t H5_ATTR_UNUSED *f, const H5T_vlen_alloc_info_t *vl_alloc_info,
    void *_vl, void *buf, void H5_ATTR_UNUSED *_bg, size_t seq_len,
    size_t base_size)
{
    char *t;                        /* Pointer to temporary buffer allocated */
    size_t len;                     /* Maximum length of the string to copy */
    herr_t      ret_value=SUCCEED;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check parameters */
    HDassert(buf);

    /* Use the user's memory allocation routine if one is defined */
    if(vl_alloc_info->alloc_func != NULL) {
        if(NULL == (t = (char *)(vl_alloc_info->alloc_func)((seq_len + 1) * base_size, vl_alloc_info->alloc_info)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "application memory allocation routine failed for VL data")
    } /* end if */
    else        /* Default to system malloc */
        if(NULL == (t = (char *)HDmalloc((seq_len + 1) * base_size)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTALLOC, FAIL, "memory allocation failed for VL data")

    /* 'write' the string into the buffer, with memcpy() */
    len = (seq_len * base_size);
    H5MM_memcpy(t, buf, len);
    t[len] = '\0';

    /* Set pointer in user's buffer with memcpy, to avoid alignment issues */
    H5MM_memcpy(_vl, &t, sizeof(char *));

done:
    FUNC_LEAVE_NOAPI(ret_value) /*lint !e429 The pointer in 't' has been copied */
} /* end H5T_vlen_str_mem_write() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_str_mem_setnull
 *
 * Purpose:	Sets a VL info object in memory to the "null" value
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Saturday, November 8, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_vlen_str_mem_setnull(H5F_t H5_ATTR_UNUSED *f, void *_vl, void H5_ATTR_UNUSED *_bg)
{
    char *t=NULL;                   /* Pointer to temporary buffer allocated */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Set pointer in user's buffer with memcpy, to avoid alignment issues */
    H5MM_memcpy(_vl,&t,sizeof(char *));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5T_vlen_str_mem_setnull() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_disk_getlen
 *
 * Purpose:	Retrieves the length of a disk based VL element.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5T_vlen_disk_getlen(const void *_vl)
{
    const uint8_t *vl=(const uint8_t *)_vl; /* Pointer to the disk VL information */
    size_t	seq_len = 0;    /* Sequence length */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check parameters */
    HDassert(vl);

    UINT32DECODE(vl, seq_len);

    FUNC_LEAVE_NOAPI((ssize_t)seq_len)
} /* end H5T_vlen_disk_getlen() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_disk_getptr
 *
 * Purpose:	Retrieves the pointer to a disk based VL element.
 *
 * Return:	Non-NULL on success/NULL on failure
 *
 * Programmer:	Quincey Koziol
 *		Saturday, June 12, 2004
 *
 *-------------------------------------------------------------------------
 */
static void *
H5T_vlen_disk_getptr(void H5_ATTR_UNUSED *vl)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check parameters */
    HDassert(vl);

    FUNC_LEAVE_NOAPI(NULL)
} /* end H5T_vlen_disk_getptr() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_disk_isnull
 *
 * Purpose:	Checks if a disk VL info object is the "nil" object
 *
 * Return:	TRUE/FALSE on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Saturday, November 8, 2003
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5T_vlen_disk_isnull(const H5F_t *f, void *_vl)
{
    uint8_t *vl = (uint8_t *)_vl; /* Pointer to the disk VL information */
    haddr_t addr;               /* Sequence's heap address */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check parameters */
    HDassert(vl);

    /* Skip the sequence's length */
    vl += 4;

    /* Get the heap address */
    H5F_addr_decode(f, (const uint8_t **)&vl, &addr);

    FUNC_LEAVE_NOAPI(addr == 0 ? TRUE : FALSE)
} /* end H5T_vlen_disk_isnull() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_disk_read
 *
 * Purpose:	Reads the disk based VL element into a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_vlen_disk_read(H5F_t *f, void *_vl, void *buf, size_t H5_ATTR_UNUSED len)
{
    uint8_t *vl=(uint8_t *)_vl;   /* Pointer to the user's hvl_t information */
    H5HG_t hobjid;
    herr_t      ret_value=SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check parameters */
    HDassert(vl);
    HDassert(buf);
    HDassert(f);

    /* Skip the length of the sequence */
    vl += 4;

    /* Get the heap information */
    H5F_addr_decode(f, (const uint8_t **)&vl, &(hobjid.addr));
    UINT32DECODE(vl, hobjid.idx);

    /* Check if this sequence actually has any data */
    if(hobjid.addr > 0)
        /* Read the VL information from disk */
        if(NULL == H5HG_read(f, &hobjid, buf, NULL))
            HGOTO_ERROR(H5E_DATATYPE, H5E_READERROR, FAIL, "Unable to read VL information")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T_vlen_disk_read() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_disk_write
 *
 * Purpose:	Writes the disk based VL element from a buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June 2, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_vlen_disk_write(H5F_t *f, const H5T_vlen_alloc_info_t H5_ATTR_UNUSED *vl_alloc_info,
    void *_vl, void *buf, void *_bg, size_t seq_len, size_t base_size)
{
    uint8_t *vl = (uint8_t *)_vl; /*Pointer to the user's hvl_t information*/
    uint8_t *bg = (uint8_t *)_bg; /*Pointer to the old data hvl_t          */
    H5HG_t hobjid;              /* New VL sequence's heap ID */
    size_t len;                 /* Size of new sequence on disk (in bytes) */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check parameters */
    HDassert(vl);
    HDassert(seq_len == 0 || buf);
    HDassert(f);

    /* Free heap object for old data.  */
    if(bg!=NULL) {
        H5HG_t bg_hobjid;       /* "Background" VL info sequence's ID info */

        /* Skip the length of the sequence and heap object ID from background data. */
        bg += 4;

        /* Get heap information */
        H5F_addr_decode(f, (const uint8_t **)&bg, &(bg_hobjid.addr));
        UINT32DECODE(bg, bg_hobjid.idx);

        /* Free heap object for old data */
        if(bg_hobjid.addr > 0)
            /* Free heap object */
            if(H5HG_remove(f, &bg_hobjid) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_WRITEERROR, FAIL, "Unable to remove heap object")
    } /* end if */

    /* Set the length of the sequence */
    UINT32ENCODE(vl, seq_len);

    /* Write the VL information to disk (allocates space also) */
    len = (seq_len*base_size);
    if(H5HG_insert(f, len, buf, &hobjid) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_WRITEERROR, FAIL, "Unable to write VL information")

    /* Encode the heap information */
    H5F_addr_encode(f, &vl, hobjid.addr);
    UINT32ENCODE(vl, hobjid.idx);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T_vlen_disk_write() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_disk_setnull
 *
 * Purpose:	Sets a VL info object on disk to the "nil" value
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Saturday, November 8, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5T_vlen_disk_setnull(H5F_t *f, void *_vl, void *_bg)
{
    uint8_t *vl = (uint8_t *)_vl; /*Pointer to the user's hvl_t information*/
    uint8_t *bg = (uint8_t *)_bg; /*Pointer to the old data hvl_t          */
    uint32_t seq_len = 0;         /* Sequence length */
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check parameters */
    HDassert(f);
    HDassert(vl);

    /* Free heap object for old data.  */
    if(bg != NULL) {
        H5HG_t bg_hobjid;       /* "Background" VL info sequence's ID info */

        /* Skip the length of the sequence and heap object ID from background data. */
        bg += 4;

        /* Get heap information */
        H5F_addr_decode(f, (const uint8_t **)&bg, &(bg_hobjid.addr));
        UINT32DECODE(bg, bg_hobjid.idx);

        /* Free heap object for old data */
        if(bg_hobjid.addr > 0) {
            /* Free heap object */
            if(H5HG_remove(f, &bg_hobjid) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_WRITEERROR, FAIL, "Unable to remove heap object")
         } /* end if */
    } /* end if */

    /* Set the length of the sequence */
    UINT32ENCODE(vl, seq_len);

    /* Encode the "nil" heap pointer information */
    H5F_addr_encode(f, &vl, (haddr_t)0);
    UINT32ENCODE(vl, 0);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T_vlen_disk_setnull() */


/*--------------------------------------------------------------------------
 NAME
    H5T_vlen_reclaim_recurse
 PURPOSE
    Internal recursive routine to free VL datatypes
 USAGE
    herr_t H5T_vlen_reclaim_recurse(elem,dt)
        void *elem;  IN/OUT: Pointer to the dataset element
        H5T_t *dt;   IN: Datatype of dataset element

 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Frees any dynamic memory used by VL datatypes in the current dataset
    element.  Performs a recursive depth-first traversal of all compound
    datatypes to free all VL datatype information allocated by any field.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
static herr_t
H5T_vlen_reclaim_recurse(void *elem, const H5T_t *dt, H5MM_free_t free_func, void *free_info)
{
    unsigned u;                     /* Local index variable */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(elem);
    HDassert(dt);

    /* Check the datatype of this element */
    switch(dt->shared->type) {
        case H5T_ARRAY:
            /* Recurse on each element, if the array's base type is array, VL, enum or compound */
            if(H5T_IS_COMPLEX(dt->shared->parent->shared->type)) {
                void *off;     /* offset of field */

                /* Calculate the offset member and recurse on it */
                for(u = 0; u < dt->shared->u.array.nelem; u++) {
                    off = ((uint8_t *)elem) + u * (dt->shared->parent->shared->size);
                    if(H5T_vlen_reclaim_recurse(off, dt->shared->parent, free_func, free_info) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "unable to free array element")
                } /* end for */
            } /* end if */
            break;

        case H5T_COMPOUND:
            /* Check each field and recurse on VL, compound, enum or array ones */
            for(u = 0; u < dt->shared->u.compnd.nmembs; u++) {
                /* Recurse if it's VL, compound, enum or array */
                if(H5T_IS_COMPLEX(dt->shared->u.compnd.memb[u].type->shared->type)) {
                    void *off;     /* offset of field */

                    /* Calculate the offset member and recurse on it */
                    off = ((uint8_t *)elem) + dt->shared->u.compnd.memb[u].offset;
                    if(H5T_vlen_reclaim_recurse(off, dt->shared->u.compnd.memb[u].type, free_func, free_info) < 0)
                        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "unable to free compound field")
                } /* end if */
            } /* end for */
            break;

        case H5T_VLEN:
            /* Recurse on the VL information if it's VL, compound, enum or array, then free VL sequence */
            if(dt->shared->u.vlen.type == H5T_VLEN_SEQUENCE) {
                hvl_t *vl = (hvl_t *)elem;    /* Temp. ptr to the vl info */

                /* Check if there is anything actually in this sequence */
                if(vl->len!=0) {
                    /* Recurse if it's VL, array, enum or compound */
                    if(H5T_IS_COMPLEX(dt->shared->parent->shared->type)) {
                        void *off;     /* offset of field */

                        /* Calculate the offset of each array element and recurse on it */
                        while(vl->len > 0) {
                            off = ((uint8_t *)vl->p) + (vl->len - 1) * dt->shared->parent->shared->size;
                            if(H5T_vlen_reclaim_recurse(off, dt->shared->parent, free_func, free_info) < 0)
                                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "unable to free VL element")
                            vl->len--;
                        } /* end while */
                    } /* end if */

                    /* Free the VL sequence */
                    if(free_func != NULL)
                        (*free_func)(vl->p, free_info);
                    else
                        HDfree(vl->p);
                } /* end if */
            } else if(dt->shared->u.vlen.type == H5T_VLEN_STRING) {
                /* Free the VL string */
                if(free_func != NULL)
                    (*free_func)(*(char **)elem, free_info);
                else
                    HDfree(*(char **)elem);
            } else {
                HDassert(0 && "Invalid VL type");
            } /* end else */
            break;

        /* Don't do anything for simple types */
        case H5T_INTEGER:
        case H5T_FLOAT:
        case H5T_TIME:
        case H5T_STRING:
        case H5T_BITFIELD:
        case H5T_OPAQUE:
        case H5T_REFERENCE:
        case H5T_ENUM:
            break;

        /* Should never have these values */
        case H5T_NO_CLASS:
        case H5T_NCLASSES:
        default:
            HGOTO_ERROR(H5E_DATATYPE, H5E_BADRANGE, FAIL, "invalid VL datatype class")
            break;

    } /* end switch */ /*lint !e788 All appropriate cases are covered */

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5T_vlen_reclaim_recurse() */


/*--------------------------------------------------------------------------
 NAME
    H5T_vlen_reclaim
 PURPOSE
    Default method to reclaim any VL data for a buffer element
 USAGE
    herr_t H5T_vlen_reclaim(elem,type_id,ndim,point,op_data)
        void *elem;  IN/OUT: Pointer to the dataset element
        hid_t type_id;   IN: Datatype of dataset element
        unsigned ndim;    IN: Number of dimensions in dataspace
        hsize_t *point; IN: Coordinate location of element in dataspace
        void *op_data    IN: Operator data

 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Frees any dynamic memory used by VL datatypes in the current dataset
    element.  Recursively descends compound datatypes to free all VL datatype
    information allocated by any field.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5T_vlen_reclaim(void *elem, hid_t type_id, unsigned H5_ATTR_UNUSED ndim,
    const hsize_t H5_ATTR_UNUSED *point, void *op_data)
{
    H5T_vlen_alloc_info_t *vl_alloc_info = (H5T_vlen_alloc_info_t *)op_data; /* VL allocation info from iterator */
    H5T_t	*dt;
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(elem);
    HDassert(vl_alloc_info);
    HDassert(H5I_DATATYPE == H5I_get_type(type_id));

    /* Check args */
    if(NULL == (dt = (H5T_t *)H5I_object_verify(type_id, H5I_DATATYPE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a datatype")

    /* Pull the free function and free info pointer out of the op_data and call the recurse datatype free function */
    if(H5T_vlen_reclaim_recurse(elem, dt, vl_alloc_info->free_func, vl_alloc_info->free_info) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't reclaim vlen elements")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5T_vlen_reclaim() */


/*-------------------------------------------------------------------------
 * Function:	H5T_vlen_reclaim_elmt
 *
 * Purpose: Alternative method to reclaim any VL data for a buffer element.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Mike McGreevy
 *              May 11, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5T_vlen_reclaim_elmt(void *elem, H5T_t *dt)
{
    H5T_vlen_alloc_info_t   vl_alloc_info;              /* VL allocation info */
    herr_t                  ret_value = SUCCEED;        /* return value */

    HDassert(dt);
    HDassert(elem);

    FUNC_ENTER_NOAPI(FAIL)

    /* Get VL allocation info */
    if(H5CX_get_vlen_alloc_info(&vl_alloc_info) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTGET, FAIL, "unable to retrieve VL allocation info")

    /* Recurse on buffer to free dynamic fields */
    if(H5T_vlen_reclaim_recurse(elem, dt, vl_alloc_info.free_func, vl_alloc_info.free_info) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTFREE, FAIL, "can't reclaim vlen elements")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5T_vlen_reclaim_elmt */


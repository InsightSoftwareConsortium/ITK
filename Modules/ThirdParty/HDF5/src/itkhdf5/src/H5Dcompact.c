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
 * Programmer:  Raymond Lu <slu@ncsa.uiuc.edu>
 *              August 5, 2002
 *
 * Purpose:     Compact dataset I/O functions.  These routines are similar
 *              H5D_contig_* and H5D_chunk_*.
 */

/****************/
/* Module Setup */
/****************/

#include "H5Dmodule.h"          /* This source code file is part of the H5D module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Dpkg.h"		/* Dataset functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fprivate.h"		/* Files				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5FLprivate.h"	/* Free Lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5VMprivate.h"		/* Vector and array functions		*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/

/* Layout operation callbacks */
static herr_t H5D__compact_construct(H5F_t *f, H5D_t *dset);
static hbool_t H5D__compact_is_space_alloc(const H5O_storage_t *storage);
static herr_t H5D__compact_io_init(const H5D_io_info_t *io_info, const H5D_type_info_t *type_info,
    hsize_t nelmts, const H5S_t *file_space, const H5S_t *mem_space,
    H5D_chunk_map_t *cm);
static ssize_t H5D__compact_readvv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_size_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_size_arr[], hsize_t mem_offset_arr[]);
static ssize_t H5D__compact_writevv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_size_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_size_arr[], hsize_t mem_offset_arr[]);
static herr_t H5D__compact_flush(H5D_t *dset);
static herr_t H5D__compact_dest(H5D_t *dset);


/*********************/
/* Package Variables */
/*********************/

/* Compact storage layout I/O ops */
const H5D_layout_ops_t H5D_LOPS_COMPACT[1] = {{
    H5D__compact_construct,
    NULL,
    H5D__compact_is_space_alloc,
    NULL,
    H5D__compact_io_init,
    H5D__contig_read,
    H5D__contig_write,
#ifdef H5_HAVE_PARALLEL
    NULL,
    NULL,
#endif /* H5_HAVE_PARALLEL */
    H5D__compact_readvv,
    H5D__compact_writevv,
    H5D__compact_flush,
    NULL,
    H5D__compact_dest
}};


/*******************/
/* Local Variables */
/*******************/

/* Declare extern the free list to manage blocks of type conversion data */
H5FL_BLK_EXTERN(type_conv);



/*-------------------------------------------------------------------------
 * Function:	H5D__compact_fill
 *
 * Purpose:	Write fill values to a compactly stored dataset.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		May 6, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__compact_fill(const H5D_t *dset)
{
    H5D_fill_buf_info_t fb_info;        /* Dataset's fill buffer info */
    hbool_t     fb_info_init = FALSE;   /* Whether the fill value buffer has been initialized */
    herr_t	ret_value = SUCCEED;	/* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    HDassert(dset && H5D_COMPACT == dset->shared->layout.type);
    HDassert(dset->shared->layout.storage.u.compact.buf);
    HDassert(dset->shared->type);
    HDassert(dset->shared->space);

    /* Initialize the fill value buffer */
    /* (use the compact dataset storage buffer as the fill value buffer) */
    if(H5D__fill_init(&fb_info, dset->shared->layout.storage.u.compact.buf,
            NULL, NULL, NULL, NULL,
            &dset->shared->dcpl_cache.fill, dset->shared->type,
            dset->shared->type_id, (size_t)0, dset->shared->layout.storage.u.compact.size) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "can't initialize fill buffer info")
    fb_info_init = TRUE;

    /* Check for VL datatype & non-default fill value */
    if(fb_info.has_vlen_fill_type)
        /* Fill the buffer with VL datatype fill values */
        if(H5D__fill_refill_vl(&fb_info, fb_info.elmts_per_buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTCONVERT, FAIL, "can't refill fill value buffer")

done:
    /* Release the fill buffer info, if it's been initialized */
    if(fb_info_init && H5D__fill_term(&fb_info) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't release fill buffer info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__compact_fill() */


/*-------------------------------------------------------------------------
 * Function:	H5D__compact_construct
 *
 * Purpose:	Constructs new compact layout information for dataset
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, May 22, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__compact_construct(H5F_t *f, H5D_t *dset)
{
    hssize_t stmp_size;         /* Temporary holder for raw data size */
    hsize_t tmp_size;           /* Temporary holder for raw data size */
    hsize_t max_comp_data_size; /* Max. allowed size of compact data */
    unsigned u;                 /* Local index variable */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f);
    HDassert(dset);

    /* Check for invalid dataset dimensions */
    for(u = 0; u < dset->shared->ndims; u++)
        if(dset->shared->max_dims[u] > dset->shared->curr_dims[u])
            HGOTO_ERROR(H5E_DATASET, H5E_UNSUPPORTED, FAIL, "extendible compact dataset not allowed")

    /*
     * Compact dataset is stored in dataset object header message of
     * layout.
     */
    stmp_size = H5S_GET_EXTENT_NPOINTS(dset->shared->space);
    HDassert(stmp_size >= 0);
    tmp_size = H5T_get_size(dset->shared->type);
    HDassert(tmp_size > 0);
    tmp_size = tmp_size * (hsize_t)stmp_size;
    H5_CHECKED_ASSIGN(dset->shared->layout.storage.u.compact.size, size_t, tmp_size, hssize_t);

    /* Verify data size is smaller than maximum header message size
     * (64KB) minus other layout message fields.
     */
    max_comp_data_size = H5O_MESG_MAX_SIZE - H5D__layout_meta_size(f, &(dset->shared->layout), FALSE);
    if(dset->shared->layout.storage.u.compact.size > max_comp_data_size)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "compact dataset size is bigger than header message maximum size")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__compact_construct() */


/*-------------------------------------------------------------------------
 * Function:	H5D__compact_is_space_alloc
 *
 * Purpose:	Query if space is allocated for layout
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 15, 2009
 *
 *-------------------------------------------------------------------------
 */
static hbool_t
H5D__compact_is_space_alloc(const H5O_storage_t H5_ATTR_UNUSED *storage)
{
    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(storage);

    /* Compact storage is currently always allocated */
    FUNC_LEAVE_NOAPI(TRUE)
} /* end H5D__compact_is_space_alloc() */


/*-------------------------------------------------------------------------
 * Function:	H5D__compact_io_init
 *
 * Purpose:	Performs initialization before any sort of I/O on the raw data
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, March 20, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__compact_io_init(const H5D_io_info_t *io_info, const H5D_type_info_t H5_ATTR_UNUSED *type_info,
    hsize_t H5_ATTR_UNUSED nelmts, const H5S_t H5_ATTR_UNUSED *file_space, const H5S_t H5_ATTR_UNUSED *mem_space,
    H5D_chunk_map_t H5_ATTR_UNUSED *cm)
{
    FUNC_ENTER_STATIC_NOERR

    io_info->store->compact.buf = io_info->dset->shared->layout.storage.u.compact.buf;
    io_info->store->compact.dirty = &io_info->dset->shared->layout.storage.u.compact.dirty;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__compact_io_init() */


/*-------------------------------------------------------------------------
 * Function:    H5D__compact_readvv
 *
 * Purpose:     Reads some data vectors from a dataset into a buffer.
 *              The data is in compact dataset.  The address is relative
 *              to the beginning address of the dataset.  The offsets and
 *              sequence lengths are in bytes.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 7, 2003
 *
 * Notes:
 *              Offsets in the sequences must be monotonically increasing
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5D__compact_readvv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_size_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_size_arr[], hsize_t mem_offset_arr[])
{
    ssize_t ret_value = -1;     /* Return value */

    FUNC_ENTER_STATIC

    HDassert(io_info);

    /* Use the vectorized memory copy routine to do actual work */
    if((ret_value = H5VM_memcpyvv(io_info->u.rbuf, mem_max_nseq, mem_curr_seq, mem_size_arr, mem_offset_arr, io_info->store->compact.buf, dset_max_nseq, dset_curr_seq, dset_size_arr, dset_offset_arr)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "vectorized memcpy failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5D__compact_readvv() */


/*-------------------------------------------------------------------------
 * Function:    H5D__compact_writevv
 *
 * Purpose:     Writes some data vectors from a dataset into a buffer.
 *              The data is in compact dataset.  The address is relative
 *              to the beginning address for the file.  The offsets and
 *              sequence lengths are in bytes.  This function only copies
 *              data into the buffer in the LAYOUT struct and mark it
 *              as DIRTY.  Later in H5D_close, the data is copied into
 *              header message in memory.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 2, 2003
 *
 * Notes:
 *              Offsets in the sequences must be monotonically increasing
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5D__compact_writevv(const H5D_io_info_t *io_info,
    size_t dset_max_nseq, size_t *dset_curr_seq, size_t dset_size_arr[], hsize_t dset_offset_arr[],
    size_t mem_max_nseq, size_t *mem_curr_seq, size_t mem_size_arr[], hsize_t mem_offset_arr[])
{
    ssize_t ret_value = -1;             /* Return value */

    FUNC_ENTER_STATIC

    HDassert(io_info);

    /* Use the vectorized memory copy routine to do actual work */
    if((ret_value = H5VM_memcpyvv(io_info->store->compact.buf, dset_max_nseq, dset_curr_seq, dset_size_arr, dset_offset_arr, io_info->u.wbuf, mem_max_nseq, mem_curr_seq, mem_size_arr, mem_offset_arr)) < 0)
        HGOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "vectorized memcpy failed")

    /* Mark the compact dataset's buffer as dirty */
    *io_info->store->compact.dirty = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5D__compact_writevv() */


/*-------------------------------------------------------------------------
 * Function:	H5D__compact_flush
 *
 * Purpose:	Writes dirty compact data to object header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 27, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__compact_flush(H5D_t *dset)
{
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(dset);

    /* Check if the buffered compact information is dirty */
    if(dset->shared->layout.storage.u.compact.dirty) {
        dset->shared->layout.storage.u.compact.dirty = FALSE;
        if(H5O_msg_write(&(dset->oloc), H5O_LAYOUT_ID, 0, H5O_UPDATE_TIME, &(dset->shared->layout)) < 0) {
            dset->shared->layout.storage.u.compact.dirty = TRUE;
            HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to update layout message")
        }
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__compact_flush() */


/*-------------------------------------------------------------------------
 * Function:	H5D__compact_dest
 *
 * Purpose:	Free the compact buffer
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, Sept 3, 2015
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__compact_dest(H5D_t *dset)
{
    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(dset);

    /* Free the buffer for the raw data for compact datasets */
    dset->shared->layout.storage.u.compact.buf = H5MM_xfree(dset->shared->layout.storage.u.compact.buf);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5D__compact_dest() */


/*-------------------------------------------------------------------------
 * Function:    H5D__compact_copy
 *
 * Purpose:     Copy compact storage raw data from SRC file to DST file.
 *
 * Return:      Non-negative on success, negative on failure.
 *
 * Programmer:  Peter Cao
 *              December 11, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__compact_copy(H5F_t *f_src, H5O_storage_compact_t *_storage_src, H5F_t *f_dst,
    H5O_storage_compact_t *storage_dst, H5T_t *dt_src, H5O_copy_t *cpy_info)
{
    hid_t       tid_src = -1;           /* Datatype ID for source datatype */
    hid_t       tid_dst = -1;           /* Datatype ID for destination datatype */
    hid_t       tid_mem = -1;           /* Datatype ID for memory datatype */
    void       *buf = NULL;             /* Buffer for copying data */
    void       *bkg = NULL;             /* Temporary buffer for copying data */
    void       *reclaim_buf = NULL;     /* Buffer for reclaiming data */
    hid_t       buf_sid = -1;           /* ID for buffer dataspace */
    H5D_shared_t    *shared_fo = (H5D_shared_t *)cpy_info->shared_fo;   /* Pointer to the shared struct for dataset object */
    H5O_storage_compact_t *storage_src = _storage_src;  /* Pointer to storage_src */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check args */
    HDassert(f_src);
    HDassert(storage_src);
    HDassert(f_dst);
    HDassert(storage_dst);
    HDassert(storage_dst->buf);
    HDassert(dt_src);

    /* If the dataset is open in the file, point to "layout" in the shared struct */
    if(shared_fo != NULL)
        storage_src = &(shared_fo->layout.storage.u.compact);

    /* Create datatype ID for src datatype, so it gets freed */
    if((tid_src = H5I_register(H5I_DATATYPE, dt_src, FALSE)) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTREGISTER, FAIL, "unable to register source file datatype")

    /* If there's a VLEN source datatype, do type conversion information */
    if(H5T_detect_class(dt_src, H5T_VLEN, FALSE) > 0) {
        H5T_path_t  *tpath_src_mem, *tpath_mem_dst;   /* Datatype conversion paths */
        H5T_t *dt_dst;              /* Destination datatype */
        H5T_t *dt_mem;              /* Memory datatype */
        H5S_t *buf_space;           /* Dataspace describing buffer */
        size_t buf_size;            /* Size of copy buffer */
        size_t nelmts;              /* Number of elements in buffer */
        size_t src_dt_size;         /* Source datatype size */
        size_t tmp_dt_size;         /* Temporary datatype size */
        size_t max_dt_size;         /* Max atatype size */
        hsize_t buf_dim;            /* Dimension for buffer */

        /* create a memory copy of the variable-length datatype */
        if(NULL == (dt_mem = H5T_copy(dt_src, H5T_COPY_TRANSIENT)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to copy")
        if((tid_mem = H5I_register(H5I_DATATYPE, dt_mem, FALSE)) < 0) {
            (void)H5T_close_real(dt_mem);
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register memory datatype")
        } /* end if */

        /* create variable-length datatype at the destinaton file */
        if(NULL == (dt_dst = H5T_copy(dt_src, H5T_COPY_TRANSIENT)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to copy")
        if(H5T_set_loc(dt_dst, f_dst, H5T_LOC_DISK) < 0) {
            (void)H5T_close_real(dt_dst);
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "cannot mark datatype on disk")
        } /* end if */
        if((tid_dst = H5I_register(H5I_DATATYPE, dt_dst, FALSE)) < 0) {
            (void)H5T_close_real(dt_dst);
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, FAIL, "unable to register destination file datatype")
        } /* end if */

        /* Set up the conversion functions */
        if(NULL == (tpath_src_mem = H5T_path_find(dt_src, dt_mem)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to convert between src and mem datatypes")
        if(NULL == (tpath_mem_dst = H5T_path_find(dt_mem, dt_dst)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTINIT, FAIL, "unable to convert between mem and dst datatypes")

        /* Determine largest datatype size */
        if(0 == (src_dt_size = H5T_get_size(dt_src)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to determine datatype size")
        if(0 == (tmp_dt_size = H5T_get_size(dt_mem)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to determine datatype size")
        max_dt_size = MAX(src_dt_size, tmp_dt_size);
        if(0 == (tmp_dt_size = H5T_get_size(dt_dst)))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "unable to determine datatype size")
        max_dt_size = MAX(max_dt_size, tmp_dt_size);

        /* Set number of whole elements that fit in buffer */
        if(0 == (nelmts = storage_src->size / src_dt_size))
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "element size too large")

        /* Set up number of bytes to copy, and initial buffer size */
        buf_size = nelmts * max_dt_size;

        /* Create dataspace for number of elements in buffer */
        buf_dim = nelmts;

        /* Create the space and set the initial extent */
        if(NULL == (buf_space = H5S_create_simple((unsigned)1, &buf_dim, NULL)))
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "can't create simple dataspace")

        /* Atomize */
        if((buf_sid = H5I_register(H5I_DATASPACE, buf_space, FALSE)) < 0) {
            H5S_close(buf_space);
            HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace ID")
        } /* end if */

        /* Allocate memory for recclaim buf */
        if(NULL == (reclaim_buf = H5FL_BLK_MALLOC(type_conv, buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Allocate memory for copying the chunk */
        if(NULL == (buf = H5FL_BLK_MALLOC(type_conv, buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        HDmemcpy(buf, storage_src->buf, storage_src->size);

        /* allocate temporary bkg buff for data conversion */
        if(NULL == (bkg = H5FL_BLK_MALLOC(type_conv, buf_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Convert from source file to memory */
        if(H5T_convert(tpath_src_mem, tid_src, tid_mem, nelmts, (size_t)0, (size_t)0, buf, bkg) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "datatype conversion failed")

        /* Copy into another buffer, to reclaim memory later */
        HDmemcpy(reclaim_buf, buf, buf_size);

        /* Set background buffer to all zeros */
        HDmemset(bkg, 0, buf_size);

        /* Convert from memory to destination file */
        if(H5T_convert(tpath_mem_dst, tid_mem, tid_dst, nelmts, (size_t)0, (size_t)0, buf, bkg) < 0)
            HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, FAIL, "datatype conversion failed")

        HDmemcpy(storage_dst->buf, buf, storage_dst->size);

        if(H5D_vlen_reclaim(tid_mem, buf_space, reclaim_buf) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_BADITER, FAIL, "unable to reclaim variable-length data")
    } /* end if */
    else if(H5T_get_class(dt_src, FALSE) == H5T_REFERENCE) {
        if(f_src != f_dst) {
            /* Check for expanding references */
            if(cpy_info->expand_ref) {
                size_t ref_count;

                /* Determine # of reference elements to copy */
                ref_count = storage_src->size / H5T_get_size(dt_src);

                /* Copy objects referenced in source buffer to destination file and set destination elements */
                if(H5O_copy_expand_ref(f_src, storage_src->buf, f_dst,
                        storage_dst->buf, ref_count, H5T_get_ref_type(dt_src), cpy_info) < 0)
                    HGOTO_ERROR(H5E_DATASET, H5E_CANTCOPY, FAIL, "unable to copy reference attribute")
            } /* end if */
            else
                /* Reset value to zero */
                HDmemset(storage_dst->buf, 0, storage_src->size);
        } /* end if */
        else
            /* Type conversion not necessary */
            HDmemcpy(storage_dst->buf, storage_src->buf, storage_src->size);
    } /* end if */
    else
        /* Type conversion not necessary */
        HDmemcpy(storage_dst->buf, storage_src->buf, storage_src->size);

    /* Mark destination buffer as dirty */
    storage_dst->dirty = TRUE;

done:
    if(buf_sid > 0 && H5I_dec_ref(buf_sid) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "can't decrement temporary dataspace ID")
    if(tid_src > 0 && H5I_dec_ref(tid_src) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    if(tid_dst > 0 && H5I_dec_ref(tid_dst) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    if(tid_mem > 0 && H5I_dec_ref(tid_mem) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTFREE, FAIL, "Can't decrement temporary datatype ID")
    if(buf)
        buf = H5FL_BLK_FREE(type_conv, buf);
    if(reclaim_buf)
        reclaim_buf = H5FL_BLK_FREE(type_conv, reclaim_buf);
    if(bkg)
        bkg = H5FL_BLK_FREE(type_conv, bkg);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__compact_copy() */


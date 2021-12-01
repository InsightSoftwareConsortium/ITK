/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Programmer:  Quincey Koziol
 *              Thursday, September 30, 2004
 *
 * Purpose:	Dataspace I/O functions.
 */

/****************/
/* Module Setup */
/****************/

#include "H5Dmodule.h" /* This source code file is part of the H5D module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions			*/
#include "H5CXprivate.h" /* API Contexts                         */
#include "H5Dpkg.h"      /* Datasets				*/
#include "H5Eprivate.h"  /* Error handling		  	*/
#include "H5FLprivate.h" /* Free Lists                           */

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/

static herr_t H5D__select_io(const H5D_io_info_t *io_info, size_t elmt_size, size_t nelmts,
                             const H5S_t *file_space, const H5S_t *mem_space);

/*********************/
/* Package Variables */
/*********************/

/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage sequences of size_t */
H5FL_SEQ_DEFINE(size_t);

/* Declare a free list to manage sequences of hsize_t */
H5FL_SEQ_DEFINE(hsize_t);

/* Declare extern free list to manage the H5S_sel_iter_t struct */
H5FL_EXTERN(H5S_sel_iter_t);

/*-------------------------------------------------------------------------
 * Function:	H5D__select_io
 *
 * Purpose:	Perform I/O directly from application memory and a file
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November 27, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5D__select_io(const H5D_io_info_t *io_info, size_t elmt_size, size_t nelmts, const H5S_t *file_space,
               const H5S_t *mem_space)
{
    H5S_sel_iter_t *mem_iter       = NULL;  /* Memory selection iteration info */
    hbool_t         mem_iter_init  = FALSE; /* Memory selection iteration info has been initialized */
    H5S_sel_iter_t *file_iter      = NULL;  /* File selection iteration info */
    hbool_t         file_iter_init = FALSE; /* File selection iteration info has been initialized */
    hsize_t *       mem_off        = NULL;  /* Pointer to sequence offsets in memory */
    hsize_t *       file_off       = NULL;  /* Pointer to sequence offsets in the file */
    size_t *        mem_len        = NULL;  /* Pointer to sequence lengths in memory */
    size_t *        file_len       = NULL;  /* Pointer to sequence lengths in the file */
    size_t          curr_mem_seq;           /* Current memory sequence to operate on */
    size_t          curr_file_seq;          /* Current file sequence to operate on */
    size_t          mem_nseq;               /* Number of sequences generated in the file */
    size_t          file_nseq;              /* Number of sequences generated in memory */
    size_t          dxpl_vec_size;          /* Vector length from API context's DXPL */
    size_t          vec_size;               /* Vector length */
    ssize_t         tmp_file_len;           /* Temporary number of bytes in file sequence */
    herr_t          ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

    /* Check args */
    HDassert(io_info);
    HDassert(io_info->dset);
    HDassert(io_info->store);
    HDassert(io_info->u.rbuf);

    /* Check for only one element in selection */
    if (nelmts == 1) {
        hsize_t single_mem_off;  /* Offset in memory */
        hsize_t single_file_off; /* Offset in the file */
        size_t  single_mem_len;  /* Length in memory */
        size_t  single_file_len; /* Length in the file */

        /* Get offset of first element in selections */
        if (H5S_SELECT_OFFSET(file_space, &single_file_off) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "can't retrieve file selection offset")
        if (H5S_SELECT_OFFSET(mem_space, &single_mem_off) < 0)
            HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "can't retrieve memory selection offset")

        /* Set up necessary information for I/O operation */
        file_nseq = mem_nseq = 1;
        curr_mem_seq = curr_file_seq = 0;
        single_file_off *= elmt_size;
        single_mem_off *= elmt_size;
        single_file_len = single_mem_len = elmt_size;

        /* Perform I/O on memory and file sequences */
        if (io_info->op_type == H5D_IO_OP_READ) {
            if ((tmp_file_len = (*io_info->layout_ops.readvv)(
                     io_info, file_nseq, &curr_file_seq, &single_file_len, &single_file_off, mem_nseq,
                     &curr_mem_seq, &single_mem_len, &single_mem_off)) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_READERROR, FAIL, "read error")
        } /* end if */
        else {
            HDassert(io_info->op_type == H5D_IO_OP_WRITE);
            if ((tmp_file_len = (*io_info->layout_ops.writevv)(
                     io_info, file_nseq, &curr_file_seq, &single_file_len, &single_file_off, mem_nseq,
                     &curr_mem_seq, &single_mem_len, &single_mem_off)) < 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error")
        } /* end else */

        /* Decrement number of elements left to process */
        HDassert(((size_t)tmp_file_len % elmt_size) == 0);
    } /* end if */
    else {
        size_t mem_nelem;  /* Number of elements used in memory sequences */
        size_t file_nelem; /* Number of elements used in file sequences */

        /* Get info from API context */
        if (H5CX_get_vec_size(&dxpl_vec_size) < 0)
            HGOTO_ERROR(H5E_DATASET, H5E_CANTGET, FAIL, "can't retrieve I/O vector size")

        /* Allocate the vector I/O arrays */
        if (dxpl_vec_size > H5D_IO_VECTOR_SIZE)
            vec_size = dxpl_vec_size;
        else
            vec_size = H5D_IO_VECTOR_SIZE;
        if (NULL == (mem_len = H5FL_SEQ_MALLOC(size_t, vec_size)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate I/O length vector array")
        if (NULL == (mem_off = H5FL_SEQ_MALLOC(hsize_t, vec_size)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate I/O offset vector array")
        if (NULL == (file_len = H5FL_SEQ_MALLOC(size_t, vec_size)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate I/O length vector array")
        if (NULL == (file_off = H5FL_SEQ_MALLOC(hsize_t, vec_size)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate I/O offset vector array")

        /* Allocate the iterators */
        if (NULL == (mem_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate memory iterator")
        if (NULL == (file_iter = H5FL_MALLOC(H5S_sel_iter_t)))
            HGOTO_ERROR(H5E_DATASET, H5E_CANTALLOC, FAIL, "can't allocate file iterator")

        /* Initialize file iterator */
        if (H5S_select_iter_init(file_iter, file_space, elmt_size, H5S_SEL_ITER_GET_SEQ_LIST_SORTED) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator")
        file_iter_init = 1; /* File selection iteration info has been initialized */

        /* Initialize memory iterator */
        if (H5S_select_iter_init(mem_iter, mem_space, elmt_size, 0) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize selection iterator")
        mem_iter_init = 1; /* Memory selection iteration info has been initialized */

        /* Initialize sequence counts */
        curr_mem_seq = curr_file_seq = 0;
        mem_nseq = file_nseq = 0;

        /* Loop, until all bytes are processed */
        while (nelmts > 0) {
            /* Check if more file sequences are needed */
            if (curr_file_seq >= file_nseq) {
                /* Get sequences for file selection */
                if (H5S_SELECT_ITER_GET_SEQ_LIST(file_iter, vec_size, nelmts, &file_nseq, &file_nelem,
                                                 file_off, file_len) < 0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")

                /* Start at the beginning of the sequences again */
                curr_file_seq = 0;
            } /* end if */

            /* Check if more memory sequences are needed */
            if (curr_mem_seq >= mem_nseq) {
                /* Get sequences for memory selection */
                if (H5S_SELECT_ITER_GET_SEQ_LIST(mem_iter, vec_size, nelmts, &mem_nseq, &mem_nelem, mem_off,
                                                 mem_len) < 0)
                    HGOTO_ERROR(H5E_INTERNAL, H5E_UNSUPPORTED, FAIL, "sequence length generation failed")

                /* Start at the beginning of the sequences again */
                curr_mem_seq = 0;
            } /* end if */

            /* Perform I/O on memory and file sequences */
            if (io_info->op_type == H5D_IO_OP_READ) {
                if ((tmp_file_len =
                         (*io_info->layout_ops.readvv)(io_info, file_nseq, &curr_file_seq, file_len, file_off,
                                                       mem_nseq, &curr_mem_seq, mem_len, mem_off)) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_READERROR, FAIL, "read error")
            } /* end if */
            else {
                HDassert(io_info->op_type == H5D_IO_OP_WRITE);
                if ((tmp_file_len = (*io_info->layout_ops.writevv)(io_info, file_nseq, &curr_file_seq,
                                                                   file_len, file_off, mem_nseq,
                                                                   &curr_mem_seq, mem_len, mem_off)) < 0)
                    HGOTO_ERROR(H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error")
            } /* end else */

            /* Decrement number of elements left to process */
            HDassert(((size_t)tmp_file_len % elmt_size) == 0);
            if (elmt_size == 0)
                HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "Resulted in division by zero")
            nelmts -= ((size_t)tmp_file_len / elmt_size);
        } /* end while */
    }     /* end else */

done:
    /* Release selection iterators */
    if (file_iter_init && H5S_SELECT_ITER_RELEASE(file_iter) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "unable to release selection iterator")
    if (file_iter)
        file_iter = H5FL_FREE(H5S_sel_iter_t, file_iter);
    if (mem_iter_init && H5S_SELECT_ITER_RELEASE(mem_iter) < 0)
        HDONE_ERROR(H5E_DATASET, H5E_CANTRELEASE, FAIL, "unable to release selection iterator")
    if (mem_iter)
        mem_iter = H5FL_FREE(H5S_sel_iter_t, mem_iter);

    /* Release vector arrays, if allocated */
    if (file_len)
        file_len = H5FL_SEQ_FREE(size_t, file_len);
    if (file_off)
        file_off = H5FL_SEQ_FREE(hsize_t, file_off);
    if (mem_len)
        mem_len = H5FL_SEQ_FREE(size_t, mem_len);
    if (mem_off)
        mem_off = H5FL_SEQ_FREE(hsize_t, mem_off);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__select_io() */

/*-------------------------------------------------------------------------
 * Function:	H5D__select_read
 *
 * Purpose:	Reads directly from file into application memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 23, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__select_read(const H5D_io_info_t *io_info, const H5D_type_info_t *type_info, hsize_t nelmts,
                 const H5S_t *file_space, const H5S_t *mem_space)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Call generic selection operation */
    H5_CHECK_OVERFLOW(nelmts, hsize_t, size_t);
    if (H5D__select_io(io_info, type_info->src_type_size, (size_t)nelmts, file_space, mem_space) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_READERROR, FAIL, "read error")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__select_read() */

/*-------------------------------------------------------------------------
 * Function:	H5D__select_write
 *
 * Purpose:	Writes directly from application memory into a file
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 23, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5D__select_write(const H5D_io_info_t *io_info, const H5D_type_info_t *type_info, hsize_t nelmts,
                  const H5S_t *file_space, const H5S_t *mem_space)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Call generic selection operation */
    H5_CHECK_OVERFLOW(nelmts, hsize_t, size_t);
    if (H5D__select_io(io_info, type_info->dst_type_size, (size_t)nelmts, file_space, mem_space) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_WRITEERROR, FAIL, "write error")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5D__select_write() */

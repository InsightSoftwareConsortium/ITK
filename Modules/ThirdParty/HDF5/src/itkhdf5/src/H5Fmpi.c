/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:             H5Fmpi.c
 *                      Jan 10 2008
 *                      Quincey Koziol
 *
 * Purpose:             MPI-related routines.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Fmodule.h" /* This source code file is part of the H5F module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions                               */
#include "H5CXprivate.h" /* API Contexts                                    */
#include "H5Eprivate.h"  /* Error handling                                  */
#include "H5Fpkg.h"      /* File access                                     */
#include "H5FDprivate.h" /* File drivers                                    */
#include "H5Iprivate.h"  /* IDs                                             */

#include "H5VLnative_private.h" /* Native VOL connector                     */

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

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

#ifdef H5_HAVE_PARALLEL
/*-------------------------------------------------------------------------
 * Function:    H5F_mpi_get_rank
 *
 * Purpose:     Retrieves the rank of an MPI process.
 *
 * Return:      Success:    The rank (non-negative)
 *
 *              Failure:    Negative
 *
 * Programmer:  Quincey Koziol
 *              Friday, January 30, 2004
 *
 *-------------------------------------------------------------------------
 */
int
H5F_mpi_get_rank(const H5F_t *f)
{
    int ret_value = -1;

    FUNC_ENTER_NOAPI((-1))

    HDassert(f && f->shared);

    /* Dispatch to driver */
    if ((ret_value = H5FD_mpi_get_rank(f->shared->lf)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, (-1), "driver get_rank request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_mpi_get_rank() */

/*-------------------------------------------------------------------------
 * Function:    H5F_mpi_get_comm
 *
 * Purpose:     Retrieves the file's communicator
 *
 * Return:      Success:    The communicator (non-negative)
 *
 *              Failure:    Negative
 *
 * Programmer:  Quincey Koziol
 *              Friday, January 30, 2004
 *
 *-------------------------------------------------------------------------
 */
MPI_Comm
H5F_mpi_get_comm(const H5F_t *f)
{
    MPI_Comm ret_value = MPI_COMM_NULL;

    FUNC_ENTER_NOAPI(MPI_COMM_NULL)

    HDassert(f && f->shared);

    /* Dispatch to driver */
    if ((ret_value = H5FD_mpi_get_comm(f->shared->lf)) == MPI_COMM_NULL)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, MPI_COMM_NULL, "driver get_comm request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_mpi_get_comm() */

/*-------------------------------------------------------------------------
 * Function:    H5F_shared_mpi_get_size
 *
 * Purpose:     Retrieves the size of an MPI process.
 *
 * Return:      Success:        The size (positive)
 *
 *              Failure:        Negative
 *
 * Programmer:  John Mainzer
 *              Friday, May 6, 2005
 *
 *-------------------------------------------------------------------------
 */
int
H5F_shared_mpi_get_size(const H5F_shared_t *f_sh)
{
    int ret_value = -1;

    FUNC_ENTER_NOAPI((-1))

    HDassert(f_sh);

    /* Dispatch to driver */
    if ((ret_value = H5FD_mpi_get_size(f_sh->lf)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, (-1), "driver get_size request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_shared_mpi_get_size() */

/*-------------------------------------------------------------------------
 * Function:    H5F_mpi_get_size
 *
 * Purpose:     Retrieves the size of an MPI process.
 *
 * Return:      Success:        The size (positive)
 *
 *              Failure:        Negative
 *
 * Programmer:  John Mainzer
 *              Friday, May 6, 2005
 *
 *-------------------------------------------------------------------------
 */
int
H5F_mpi_get_size(const H5F_t *f)
{
    int ret_value = -1;

    FUNC_ENTER_NOAPI((-1))

    HDassert(f && f->shared);

    /* Dispatch to driver */
    if ((ret_value = H5FD_mpi_get_size(f->shared->lf)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, (-1), "driver get_size request failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_mpi_get_size() */

/*-------------------------------------------------------------------------
 * Function:    H5F_set_mpi_atomicity
 *
 * Purpose:     Private call to set the atomicity mode
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_set_mpi_atomicity(H5F_t *file, hbool_t flag)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL);

    /* Check args */
    HDassert(file);

    /* Check VFD */
    if (!H5F_HAS_FEATURE(file, H5FD_FEAT_HAS_MPI))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL,
                    "incorrect VFL driver, does not support MPI atomicity mode");

    /* Set atomicity value */
    if (H5FD_set_mpio_atomicity(file->shared->lf, flag) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "can't set atomicity flag");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5F_set_mpi_atomicity() */

/*-------------------------------------------------------------------------
 * Function:    H5Fset_mpi_atomicity
 *
 * Purpose:     Sets the atomicity mode
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              Feb 14, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fset_mpi_atomicity(hid_t file_id, hbool_t flag)
{
    H5VL_object_t *vol_obj   = NULL;
    int            va_flag   = (int)flag; /* C is grumpy about passing hbool_t via va_arg */
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL);
    H5TRACE2("e", "ib", file_id, flag);

    /* Get the file object */
    if (NULL == (vol_obj = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier");

    /* Set atomicity value */
    if (H5VL_file_optional(vol_obj, H5VL_NATIVE_FILE_SET_MPI_ATOMICITY, H5P_DATASET_XFER_DEFAULT,
                           H5_REQUEST_NULL, va_flag) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, FAIL, "unable to set MPI atomicity");

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Fset_mpi_atomicity() */

/*-------------------------------------------------------------------------
 * Function:    H5F_get_mpi_atomicity
 *
 * Purpose:     Private call to get the atomicity mode
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_get_mpi_atomicity(H5F_t *file, hbool_t *flag)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL);

    /* Check args */
    HDassert(file);
    HDassert(flag);

    /* Check VFD */
    if (!H5F_HAS_FEATURE(file, H5FD_FEAT_HAS_MPI))
        HGOTO_ERROR(H5E_FILE, H5E_BADVALUE, FAIL,
                    "incorrect VFL driver, does not support MPI atomicity mode");

    /* Get atomicity value */
    if (H5FD_get_mpio_atomicity(file->shared->lf, flag) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get atomicity flag");

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5F_get_mpi_atomicity() */

/*-------------------------------------------------------------------------
 * Function:    H5Fget_mpi_atomicity
 *
 * Purpose:     Returns the atomicity mode
 *
 * Return:      Success:    Non-negative
 *              Failure:    Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              Feb 14, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_mpi_atomicity(hid_t file_id, hbool_t *flag)
{
    H5VL_object_t *vol_obj   = NULL;
    herr_t         ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL);
    H5TRACE2("e", "i*b", file_id, flag);

    /* Get the file object */
    if (NULL == (vol_obj = (H5VL_object_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier");

    /* Get atomicity value */
    if (H5VL_file_optional(vol_obj, H5VL_NATIVE_FILE_GET_MPI_ATOMICITY, H5P_DATASET_XFER_DEFAULT,
                           H5_REQUEST_NULL, flag) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to get MPI atomicity");

done:
    FUNC_LEAVE_API(ret_value);
} /* end H5Fget_mpi_atomicity() */

/*-------------------------------------------------------------------------
 * Function:    H5F_mpi_retrieve_comm
 *
 * Purpose:     Retrieves an MPI communicator from the file the location ID
 *              is in. If the loc_id is invalid, the fapl_id is used to
 *              retrieve the communicator.
 *
 * Return:      Success:    Non-negative
 *
 *              Failure:    Negative
 *
 * Programmer:  Mohamad Chaarawi
 *              Feb 14, 2012
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_mpi_retrieve_comm(hid_t loc_id, hid_t acspl_id, MPI_Comm *mpi_comm)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(mpi_comm);

    /* Set value to return to invalid MPI comm */
    *mpi_comm = MPI_COMM_NULL;

    /* if the loc_id is valid, then get the comm from the file
       attached to the loc_id */
    if (H5I_INVALID_HID != loc_id) {
        H5G_loc_t loc;
        H5F_t    *f = NULL;

        /* Retrieve the file structure */
        if (H5G_loc(loc_id, &loc) < 0)
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a location")
        f = loc.oloc->file;
        HDassert(f);

        /* Check if MPIO driver is used */
        if (H5F_HAS_FEATURE(f, H5FD_FEAT_HAS_MPI)) {
            /* retrieve the file communicator */
            if (MPI_COMM_NULL == (*mpi_comm = H5F_mpi_get_comm(f)))
                HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get MPI communicator")
        }
    }
    /* otherwise, this is from H5Fopen or H5Fcreate and has to be collective */
    else {
        H5P_genplist_t *plist; /* Property list pointer */

        if (NULL == (plist = H5P_object_verify(acspl_id, H5P_FILE_ACCESS)))
            HGOTO_ERROR(H5E_FILE, H5E_BADTYPE, FAIL, "not a file access list")

        if (H5FD_MPIO == H5P_peek_driver(plist))
            if (H5P_peek(plist, H5F_ACS_MPI_PARAMS_COMM_NAME, mpi_comm) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't get MPI communicator")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_mpi_retrieve_comm */

/*-------------------------------------------------------------------------
 * Function:    H5F_get_coll_metadata_reads
 *
 * Purpose:     Determines whether collective metadata reads should be
 *              performed. This routine is meant to be the single source of
 *              truth for the collective metadata reads status, as it
 *              coordinates between the file-global flag and the flag set
 *              for the current operation in the current API context.
 *
 * Return:      TRUE/FALSE (can't fail)
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5F_get_coll_metadata_reads(const H5F_t *file)
{
    H5P_coll_md_read_flag_t file_flag = H5P_USER_FALSE;
    hbool_t                 ret_value = FALSE;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file && file->shared);

    /* Retrieve the file-global flag */
    file_flag = H5F_COLL_MD_READ(file);

    /* If file flag is set to H5P_FORCE_FALSE, exit early
     * with FALSE, since collective metadata reads have
     * been explicitly disabled somewhere in the library.
     */
    if (H5P_FORCE_FALSE == file_flag)
        ret_value = FALSE;
    else {
        /* If file flag is set to H5P_USER_TRUE, ignore
         * any settings in the API context. A file-global
         * setting of H5P_USER_TRUE for collective metadata
         * reads should ignore any settings on an Access
         * Property List for an individual operation.
         */
        if (H5P_USER_TRUE == file_flag)
            ret_value = TRUE;
        else {
            /* Get the collective metadata reads flag from
             * the current API context.
             */
            ret_value = H5CX_get_coll_metadata_read();
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_get_coll_metadata_reads() */

/*-------------------------------------------------------------------------
 * Function:    H5F_set_coll_metadata_reads
 *
 * Purpose:     Used to temporarily modify the collective metadata reads
 *              status. This is useful for cases where either:
 *
 *              * Collective metadata reads are enabled, but need to be
 *                disabled for an operation about to occur that may trigger
 *                an independent metadata read (such as only rank 0 doing
 *                something)
 *
 *              * Metadata reads are currently independent, but it is
 *                guaranteed that the application has maintained
 *                collectivity at the interface level (e.g., an operation
 *                that modifies metadata is being performed). In this case,
 *                it should be safe to enable collective metadata reads,
 *                barring any internal library issues that may occur
 *
 *              After completion, the `file_flag` parameter will be set to
 *              the previous value of the file-global collective metadata
 *              reads flag. The `context_flag` parameter will be set to the
 *              previous value of the API context's collective metadata
 *              reads flag. Another call to this routine should be made to
 *              restore these values (see below warning).
 *
 * !! WARNING !!
 *              It is dangerous to modify the collective metadata reads
 *              status, as this can cause crashes, hangs and corruption in
 *              the HDF5 file when improperly done. Therefore, the
 *              `file_flag` and `context_flag` parameters are both
 *              mandatory, and it is assumed that the caller will guarantee
 *              these settings are restored with another call to this
 *              routine once the bracketed operation is complete.
 * !! WARNING !!
 *
 * Return:      Nothing
 *
 *-------------------------------------------------------------------------
 */
void
H5F_set_coll_metadata_reads(H5F_t *file, H5P_coll_md_read_flag_t *file_flag, hbool_t *context_flag)
{
    H5P_coll_md_read_flag_t prev_file_flag    = H5P_USER_FALSE;
    hbool_t                 prev_context_flag = FALSE;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(file && file->shared);
    HDassert(file_flag);
    HDassert(context_flag);

    /* Save old state */
    prev_file_flag    = H5F_COLL_MD_READ(file);
    prev_context_flag = H5CX_get_coll_metadata_read();

    /* Set new desired state */
    if (prev_file_flag != *file_flag) {
        file->shared->coll_md_read = *file_flag;
        *file_flag                 = prev_file_flag;
    }
    if (prev_context_flag != *context_flag) {
        H5CX_set_coll_metadata_read(*context_flag);
        *context_flag = prev_context_flag;
    }

    FUNC_LEAVE_NOAPI_VOID
} /* end H5F_set_coll_metadata_reads() */

/*-------------------------------------------------------------------------
 * Function:    H5F_mpi_get_file_block_type
 *
 * Purpose:     Creates an MPI derived datatype for communicating an
 *              H5F_block_t structure. If `commit` is specified as TRUE,
 *              the resulting datatype will be committed and ready for
 *              use in communication. Otherwise, the type is only suitable
 *              for building other derived types.
 *
 *              If TRUE is returned through `new_type_derived`, this lets
 *              the caller know that the datatype has been derived and
 *              should be freed with MPI_Type_free once it is no longer
 *              needed.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_mpi_get_file_block_type(hbool_t commit, MPI_Datatype *new_type, hbool_t *new_type_derived)
{
    MPI_Datatype types[2];
    MPI_Aint     displacements[2];
    int          block_lengths[2];
    int          field_count;
    int          mpi_code;
    herr_t       ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(new_type);
    HDassert(new_type_derived);

    *new_type_derived = FALSE;

    field_count = 2;
    HDassert(field_count == sizeof(types) / sizeof(MPI_Datatype));

    block_lengths[0] = 1;
    block_lengths[1] = 1;
    displacements[0] = offsetof(H5F_block_t, offset);
    displacements[1] = offsetof(H5F_block_t, length);
    types[0]         = HADDR_AS_MPI_TYPE;
    types[1]         = HSIZE_AS_MPI_TYPE;
    if (MPI_SUCCESS !=
        (mpi_code = MPI_Type_create_struct(field_count, block_lengths, displacements, types, new_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_create_struct failed", mpi_code)
    *new_type_derived = TRUE;

    if (commit && MPI_SUCCESS != (mpi_code = MPI_Type_commit(new_type)))
        HMPI_GOTO_ERROR(FAIL, "MPI_Type_commit failed", mpi_code)

done:
    if (ret_value < 0) {
        if (*new_type_derived) {
            if (MPI_SUCCESS != (mpi_code = MPI_Type_free(new_type)))
                HMPI_DONE_ERROR(FAIL, "MPI_Type_free failed", mpi_code)
            *new_type_derived = FALSE;
        }
    }

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_mpi_get_file_block_type() */

#endif /* H5_HAVE_PARALLEL */

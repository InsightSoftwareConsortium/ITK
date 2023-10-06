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

/*
 * Purpose:     The IOC VFD implements a file driver which relays all the
 *              VFD calls to an underlying VFD, and send all the write calls to
 *              another underlying VFD. Maintains two files simultaneously.
 */

/* This source code file is part of the H5FD driver module */
#include "H5FDdrvr_module.h"

#include "H5private.h"    /* Generic Functions        */
#include "H5FDpublic.h"   /* Basic H5FD definitions   */
#include "H5Eprivate.h"   /* Error handling           */
#include "H5FDprivate.h"  /* File drivers             */
#include "H5FDioc.h"      /* IOC file driver          */
#include "H5FDioc_priv.h" /* IOC file driver          */
#include "H5FDmpio.h"     /* MPI I/O VFD              */
#include "H5FLprivate.h"  /* Free Lists               */
#include "H5Fprivate.h"   /* File access              */
#include "H5Iprivate.h"   /* IDs                      */
#include "H5MMprivate.h"  /* Memory management        */
#include "H5Pprivate.h"   /* Property lists           */

/* The driver identification number, initialized at runtime */
static hid_t H5FD_IOC_g = H5I_INVALID_HID;

/* Whether the driver initialized MPI on its own */
static hbool_t H5FD_mpi_self_initialized = FALSE;

/* Pointer to value for MPI_TAG_UB */
int *H5FD_IOC_tag_ub_val_ptr = NULL;

/* The information of this ioc */
typedef struct H5FD_ioc_t {
    H5FD_t            pub; /* public stuff, must be first    */
    int               fd;  /* the filesystem file descriptor */
    H5FD_ioc_config_t fa;  /* driver-specific file access properties */

    H5FD_subfiling_params_t subf_config;

    /* MPI Info */
    MPI_Comm comm;
    MPI_Info info;
    int      mpi_rank;
    int      mpi_size;

    uint64_t file_id;
    int64_t  context_id; /* The value used to lookup a subfiling context for the file */

    haddr_t eof;
    haddr_t eoa;
    haddr_t last_eoa;
    haddr_t local_eof;

    char *file_dir;  /* Directory where we find files */
    char *file_path; /* The user defined filename */
} H5FD_ioc_t;

/*
 * These macros check for overflow of various quantities.  These macros
 * assume that HDoff_t is signed and haddr_t and size_t are unsigned.
 *
 * ADDR_OVERFLOW:   Checks whether a file address of type `haddr_t'
 *                  is too large to be represented by the second argument
 *                  of the file seek function.
 *
 * SIZE_OVERFLOW:   Checks whether a buffer size of type `hsize_t' is too
 *                  large to be represented by the `size_t' type.
 *
 * REGION_OVERFLOW: Checks whether an address and size pair describe data
 *                  which can be addressed entirely by the second
 *                  argument of the file seek function.
 */
#define MAXADDR          (((haddr_t)1 << (8 * sizeof(HDoff_t) - 1)) - 1)
#define ADDR_OVERFLOW(A) (HADDR_UNDEF == (A) || ((A) & ~(haddr_t)MAXADDR))
#define SIZE_OVERFLOW(Z) ((Z) & ~(hsize_t)MAXADDR)
#define REGION_OVERFLOW(A, Z)                                                                                \
    (ADDR_OVERFLOW(A) || SIZE_OVERFLOW(Z) || HADDR_UNDEF == (A) + (Z) || (HDoff_t)((A) + (Z)) < (HDoff_t)(A))

#ifdef H5FD_IOC_DEBUG
#define H5FD_IOC_LOG_CALL(name)                                                                              \
    do {                                                                                                     \
        printf("called %s()\n", (name));                                                                     \
        fflush(stdout);                                                                                      \
    } while (0)
#else
#define H5FD_IOC_LOG_CALL(name) /* no-op */
#endif

/* Private functions */
/* Prototypes */
static herr_t  H5FD__ioc_term(void);
static hsize_t H5FD__ioc_sb_size(H5FD_t *_file);
static herr_t  H5FD__ioc_sb_encode(H5FD_t *_file, char *name /*out*/, unsigned char *buf /*out*/);
static herr_t  H5FD__ioc_sb_decode(H5FD_t *_file, const char *name, const unsigned char *buf);
static void   *H5FD__ioc_fapl_get(H5FD_t *_file);
static void   *H5FD__ioc_fapl_copy(const void *_old_fa);
static herr_t  H5FD__ioc_fapl_free(void *_fapl);
static H5FD_t *H5FD__ioc_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr);
static herr_t  H5FD__ioc_close(H5FD_t *_file);
static int     H5FD__ioc_cmp(const H5FD_t *_f1, const H5FD_t *_f2);
static herr_t  H5FD__ioc_query(const H5FD_t *_file, unsigned long *flags /* out */);
static herr_t  H5FD__ioc_get_type_map(const H5FD_t *_file, H5FD_mem_t *type_map);
static haddr_t H5FD__ioc_alloc(H5FD_t *file, H5FD_mem_t type, hid_t dxpl_id, hsize_t size);
static herr_t  H5FD__ioc_free(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, hsize_t size);
static haddr_t H5FD__ioc_get_eoa(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type);
static herr_t  H5FD__ioc_set_eoa(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, haddr_t addr);
static haddr_t H5FD__ioc_get_eof(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type);
static herr_t  H5FD__ioc_get_handle(H5FD_t *_file, hid_t H5_ATTR_UNUSED fapl, void **file_handle);
static herr_t  H5FD__ioc_read(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
                              void *buf);
static herr_t  H5FD__ioc_write(H5FD_t *_file, H5FD_mem_t type, hid_t dxpl_id, haddr_t addr, size_t size,
                               const void *buf);
static herr_t  H5FD__ioc_read_vector(H5FD_t *file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[],
                                     haddr_t addrs[], size_t sizes[], void *bufs[] /* out */);
static herr_t  H5FD__ioc_write_vector(H5FD_t *file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[],
                                      haddr_t addrs[], size_t sizes[], const void *bufs[] /* in */);
static herr_t  H5FD__ioc_flush(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
static herr_t  H5FD__ioc_truncate(H5FD_t *_file, hid_t dxpl_id, hbool_t closing);
static herr_t  H5FD__ioc_lock(H5FD_t *_file, hbool_t rw);
static herr_t  H5FD__ioc_unlock(H5FD_t *_file);
static herr_t  H5FD__ioc_del(const char *name, hid_t fapl);
/*
static herr_t H5FD__ioc_ctl(H5FD_t *file, uint64_t op_code, uint64_t flags,
                            const void *input, void **result);
*/

static herr_t H5FD__ioc_get_default_config(H5FD_ioc_config_t *config_out);
static herr_t H5FD__ioc_validate_config(const H5FD_ioc_config_t *fa);

static herr_t H5FD__ioc_close_int(H5FD_ioc_t *file_ptr);

static herr_t H5FD__ioc_write_vector_internal(H5FD_t *_file, uint32_t count, H5FD_mem_t types[],
                                              haddr_t addrs[], size_t sizes[],
                                              const void *bufs[] /* data_in */);
static herr_t H5FD__ioc_read_vector_internal(H5FD_t *_file, uint32_t count, haddr_t addrs[], size_t sizes[],
                                             void *bufs[] /* data_out */);

static const H5FD_class_t H5FD_ioc_g = {
    H5FD_CLASS_VERSION,        /* VFD interface version */
    H5_VFD_IOC,                /* value                 */
    H5FD_IOC_NAME,             /* name                  */
    MAXADDR,                   /* maxaddr               */
    H5F_CLOSE_WEAK,            /* fc_degree             */
    H5FD__ioc_term,            /* terminate             */
    H5FD__ioc_sb_size,         /* sb_size               */
    H5FD__ioc_sb_encode,       /* sb_encode             */
    H5FD__ioc_sb_decode,       /* sb_decode             */
    sizeof(H5FD_ioc_config_t), /* fapl_size             */
    H5FD__ioc_fapl_get,        /* fapl_get              */
    H5FD__ioc_fapl_copy,       /* fapl_copy             */
    H5FD__ioc_fapl_free,       /* fapl_free             */
    0,                         /* dxpl_size             */
    NULL,                      /* dxpl_copy             */
    NULL,                      /* dxpl_free             */
    H5FD__ioc_open,            /* open                  */
    H5FD__ioc_close,           /* close                 */
    H5FD__ioc_cmp,             /* cmp                   */
    H5FD__ioc_query,           /* query                 */
    H5FD__ioc_get_type_map,    /* get_type_map          */
    H5FD__ioc_alloc,           /* alloc                 */
    H5FD__ioc_free,            /* free                  */
    H5FD__ioc_get_eoa,         /* get_eoa               */
    H5FD__ioc_set_eoa,         /* set_eoa               */
    H5FD__ioc_get_eof,         /* get_eof               */
    H5FD__ioc_get_handle,      /* get_handle            */
    H5FD__ioc_read,            /* read                  */
    H5FD__ioc_write,           /* write                 */
    H5FD__ioc_read_vector,     /* read_vector           */
    H5FD__ioc_write_vector,    /* write_vector          */
    NULL,                      /* read_selection        */
    NULL,                      /* write_selection       */
    H5FD__ioc_flush,           /* flush                 */
    H5FD__ioc_truncate,        /* truncate              */
    H5FD__ioc_lock,            /* lock                  */
    H5FD__ioc_unlock,          /* unlock                */
    H5FD__ioc_del,             /* del                   */
    NULL,                      /* ctl                   */
    H5FD_FLMAP_DICHOTOMY       /* fl_map                */
};

/* Declare a free list to manage the H5FD_ioc_t struct */
H5FL_DEFINE_STATIC(H5FD_ioc_t);

/* Declare a free list to manage the H5FD_ioc_config_t struct */
H5FL_DEFINE_STATIC(H5FD_ioc_config_t);

/*-------------------------------------------------------------------------
 * Function:    H5FD_ioc_init
 *
 * Purpose:     Initialize the IOC driver by registering it with the
 *              library.
 *
 * Return:      Success:    The driver ID for the ioc driver.
 *              Failure:    Negative
 *-------------------------------------------------------------------------
 */
hid_t
H5FD_ioc_init(void)
{
    hid_t ret_value = H5I_INVALID_HID;

    H5FD_IOC_LOG_CALL(__func__);

    /* Register the IOC VFD, if it isn't already registered */
    if (H5I_VFL != H5I_get_type(H5FD_IOC_g)) {
        char *env_var;
        int   key_val_retrieved = 0;
        int   mpi_code;

        if ((H5FD_IOC_g = H5FD_register(&H5FD_ioc_g, sizeof(H5FD_class_t), FALSE)) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_ID, H5E_CANTREGISTER, H5I_INVALID_HID, "can't register IOC VFD");

        /* Check if IOC VFD has been loaded dynamically */
        env_var = HDgetenv(HDF5_DRIVER);
        if (env_var && !HDstrcmp(env_var, H5FD_IOC_NAME)) {
            int mpi_initialized = 0;
            int provided        = 0;

            /* Initialize MPI if not already initialized */
            if (MPI_SUCCESS != (mpi_code = MPI_Initialized(&mpi_initialized)))
                H5_SUBFILING_MPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Initialized failed", mpi_code);
            if (mpi_initialized) {
                /* If MPI is initialized, validate that it was initialized with MPI_THREAD_MULTIPLE */
                if (MPI_SUCCESS != (mpi_code = MPI_Query_thread(&provided)))
                    H5_SUBFILING_MPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Query_thread failed", mpi_code);
                if (provided != MPI_THREAD_MULTIPLE)
                    H5_SUBFILING_GOTO_ERROR(
                        H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID,
                        "IOC VFD requires the use of MPI_Init_thread with MPI_THREAD_MULTIPLE");
            }
            else {
                int required = MPI_THREAD_MULTIPLE;

                /* Otherwise, initialize MPI */
                if (MPI_SUCCESS != (mpi_code = MPI_Init_thread(NULL, NULL, required, &provided)))
                    H5_SUBFILING_MPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Init_thread failed", mpi_code);

                H5FD_mpi_self_initialized = TRUE;

                if (provided != required)
                    H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID,
                                            "MPI doesn't support MPI_Init_thread with MPI_THREAD_MULTIPLE");
            }
        }

        /* Retrieve upper bound for MPI message tag value */
        if (MPI_SUCCESS != (mpi_code = MPI_Comm_get_attr(MPI_COMM_WORLD, MPI_TAG_UB, &H5FD_IOC_tag_ub_val_ptr,
                                                         &key_val_retrieved)))
            H5_SUBFILING_MPI_GOTO_ERROR(H5I_INVALID_HID, "MPI_Comm_get_attr failed", mpi_code);

        if (!key_val_retrieved)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTINIT, H5I_INVALID_HID,
                                    "couldn't retrieve value for MPI_TAG_UB");
    }

    ret_value = H5FD_IOC_g;

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD_ioc_init() */

/*---------------------------------------------------------------------------
 * Function:    H5FD__ioc_term
 *
 * Purpose:     Shut down the IOC VFD.
 *
 * Returns:     SUCCEED (Can't fail)
 *---------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_term(void)
{
    herr_t ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    if (H5FD_IOC_g >= 0) {
        /* Terminate MPI if the driver initialized it */
        if (H5FD_mpi_self_initialized) {
            int mpi_finalized = 0;
            int mpi_code;

            if (MPI_SUCCESS != (mpi_code = MPI_Finalized(&mpi_finalized)))
                H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Finalized failed", mpi_code);
            if (!mpi_finalized) {
                if (MPI_SUCCESS != (mpi_code = MPI_Finalize()))
                    H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Finalize failed", mpi_code);
            }

            H5FD_mpi_self_initialized = FALSE;
        }
    }

done:
    /* Reset VFL ID */
    H5FD_IOC_g = H5I_INVALID_HID;

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_term() */

/*-------------------------------------------------------------------------
 * Function:    H5Pset_fapl_ioc
 *
 * Purpose:     Sets the file access property list to use the
 *              ioc driver.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_fapl_ioc(hid_t fapl_id, H5FD_ioc_config_t *vfd_config)
{
    H5FD_ioc_config_t *ioc_conf  = NULL;
    H5P_genplist_t    *plist_ptr = NULL;
    herr_t             ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    if (NULL == (plist_ptr = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");

    if (vfd_config == NULL) {
        if (NULL == (ioc_conf = H5FL_CALLOC(H5FD_ioc_config_t)))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate IOC VFD configuration");

        /* Get IOC VFD defaults */
        if (H5FD__ioc_get_default_config(ioc_conf) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't get default IOC VFD configuration");

        vfd_config = ioc_conf;
    }

    if (H5FD__ioc_validate_config(vfd_config) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid IOC VFD configuration");

    ret_value = H5P_set_driver(plist_ptr, H5FD_IOC, vfd_config, NULL);

done:
    if (ioc_conf) {
        H5FL_FREE(H5FD_ioc_config_t, ioc_conf);
    }

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5Pset_fapl_ioc() */

/*-------------------------------------------------------------------------
 * Function:    H5Pget_fapl_ioc
 *
 * Purpose:     Returns information about the ioc file access property
 *              list through the structure config_out.
 *
 *              Will fail if config_out is received without pre-set valid
 *              magic and version information.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_fapl_ioc(hid_t fapl_id, H5FD_ioc_config_t *config_out)
{
    const H5FD_ioc_config_t *config_ptr         = NULL;
    H5P_genplist_t          *plist_ptr          = NULL;
    hbool_t                  use_default_config = FALSE;
    herr_t                   ret_value          = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    /* Check arguments */
    if (config_out == NULL)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "config_out is NULL");

    if (NULL == (plist_ptr = H5P_object_verify(fapl_id, H5P_FILE_ACCESS)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");

    if (H5FD_IOC != H5P_peek_driver(plist_ptr))
        use_default_config = TRUE;
    else {
        config_ptr = H5P_peek_driver_info(plist_ptr);
        if (NULL == config_ptr)
            use_default_config = TRUE;
    }

    if (use_default_config) {
        if (H5FD__ioc_get_default_config(config_out) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get default IOC VFD configuration");
    }
    else {
        /* Copy the IOC fapl data out */
        memcpy(config_out, config_ptr, sizeof(H5FD_ioc_config_t));
    }

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5Pget_fapl_ioc() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_get_default_config
 *
 * Purpose:     This is called by H5Pset/get_fapl_ioc when called with no
 *              established configuration info.  This simply fills in
 *              the basics.   This avoids the necessity of having the
 *              user write code to initialize the config structure.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_get_default_config(H5FD_ioc_config_t *config_out)
{
    herr_t ret_value = SUCCEED;

    assert(config_out);

    memset(config_out, 0, sizeof(*config_out));

    config_out->magic            = H5FD_IOC_FAPL_MAGIC;
    config_out->version          = H5FD_IOC_CURR_FAPL_VERSION;
    config_out->thread_pool_size = H5FD_IOC_DEFAULT_THREAD_POOL_SIZE;

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_validate_config()
 *
 * Purpose:     Test to see if the supplied instance of
 *              H5FD_ioc_config_t contains internally consistent data.
 *              Return SUCCEED if so, and FAIL otherwise.
 *
 *              Note the difference between internally consistent and
 *              correct.  As we will have to try to setup the IOC to
 *              determine whether the supplied data is correct,
 *              we will settle for internal consistency at this point
 *
 * Return:      SUCCEED if instance of H5FD_ioc_config_t contains
 *              internally consistent data, FAIL otherwise.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_validate_config(const H5FD_ioc_config_t *fa)
{
    herr_t ret_value = SUCCEED;

    assert(fa != NULL);

    if (fa->version != H5FD_IOC_CURR_FAPL_VERSION)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Unknown H5FD_ioc_config_t version");

    if (fa->magic != H5FD_IOC_FAPL_MAGIC)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid H5FD_ioc_config_t magic value");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_validate_config() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_sb_size
 *
 * Purpose:     Obtains the number of bytes required to store the driver
 *              file access data in the HDF5 superblock.
 *
 * Return:      Success:    Number of bytes required.
 *
 *              Failure:    0 if an error occurs or if the driver has no
 *                          data to store in the superblock.
 *
 *-------------------------------------------------------------------------
 */
static hsize_t
H5FD__ioc_sb_size(H5FD_t H5_ATTR_UNUSED *_file)
{
    hsize_t ret_value = 0;

    H5FD_IOC_LOG_CALL(__func__);

    /* Configuration structure magic number */
    ret_value += sizeof(uint32_t);

    /* Configuration structure version number */
    ret_value += sizeof(uint32_t);

    /* IOC thread pool size */
    ret_value += sizeof(int32_t);

    /* Subfiling stripe size */
    ret_value += sizeof(int64_t);

    /* Subfiling stripe count (encoded as int64_t for future) */
    ret_value += sizeof(int64_t);

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_sb_size */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_sb_encode
 *
 * Purpose:     Encode driver-specific data into the output arguments.
 *
 * Return:      Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_sb_encode(H5FD_t *_file, char *name, unsigned char *buf)
{
    subfiling_context_t *sf_context = NULL;
    H5FD_ioc_t          *file       = (H5FD_ioc_t *)_file;
    uint8_t             *p          = (uint8_t *)buf;
    int64_t              tmp64;
    herr_t               ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    if (NULL == (sf_context = H5_get_subfiling_object(file->context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get subfiling context object");

    /* Encode driver name */
    HDstrncpy(name, "IOC", 9);
    name[8] = '\0';

    /* Encode configuration structure magic number */
    UINT32ENCODE(p, file->fa.magic);

    /* Encode configuration structure version number */
    UINT32ENCODE(p, file->fa.version);

    /* Encode thread pool size field */
    INT32ENCODE(p, file->fa.thread_pool_size);

    /* Encode subfiling stripe size */
    INT64ENCODE(p, sf_context->sf_stripe_size);

    /* Encode subfiling stripe count (number of subfiles) */
    tmp64 = sf_context->sf_num_subfiles;
    INT64ENCODE(p, tmp64);

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_sb_encode */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_sb_decode
 *
 * Purpose:     Decodes the driver information block.
 *
 * Return:      Non-negative on success/Negative on failure
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_sb_decode(H5FD_t *_file, const char *name, const unsigned char *buf)
{
    subfiling_context_t *sf_context = NULL;
    const uint8_t       *p          = (const uint8_t *)buf;
    H5FD_ioc_t          *file       = (H5FD_ioc_t *)_file;
    int64_t              tmp64;
    herr_t               ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    if (NULL == (sf_context = H5_get_subfiling_object(file->context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, FAIL, "can't get subfiling context object");

    if (HDstrncmp(name, "IOC", 9))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "invalid driver name in superblock");

    /* Decode configuration structure magic number */
    UINT32DECODE(p, file->fa.magic);

    /* Decode configuration structure version number */
    UINT32DECODE(p, file->fa.version);

    /* Decode thread pool size field */
    INT32DECODE(p, file->fa.thread_pool_size);

    /* Decode subfiling stripe size */
    INT64DECODE(p, file->subf_config.stripe_size);

    /* Decode subfiling stripe count */
    INT64DECODE(p, tmp64);
    H5_CHECK_OVERFLOW(tmp64, int64_t, int32_t);
    file->subf_config.stripe_count = (int32_t)tmp64;

    /* Validate the decoded configuration */
    if (H5FD__ioc_validate_config(&file->fa) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL, "decoded IOC VFD configuration info is invalid");

    if (H5_subfiling_validate_config(&file->subf_config) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL,
                                "decoded subfiling configuration parameters are invalid");

    if (file->subf_config.stripe_size != sf_context->sf_stripe_size)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, FAIL,
                                "specified subfiling stripe size (%" PRId64
                                ") doesn't match value stored in file (%" PRId64 ")",
                                sf_context->sf_stripe_size, file->subf_config.stripe_size);

    if (file->subf_config.stripe_count != sf_context->sf_num_subfiles)
        H5_SUBFILING_GOTO_ERROR(
            H5E_VFL, H5E_BADVALUE, FAIL,
            "specified subfiling stripe count (%d) doesn't match value stored in file (%" PRId32 ")",
            sf_context->sf_num_subfiles, file->subf_config.stripe_count);

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_sb_decode */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_fapl_get
 *
 * Purpose:     Returns a file access property list which indicates how the
 *              specified file is being accessed. The return list could be
 *              used to access another file the same way.
 *
 * Return:      Success:    Ptr to new file access property list with all
 *                          members copied from the file struct.
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
static void *
H5FD__ioc_fapl_get(H5FD_t *_file)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    void       *ret_value = NULL;

    H5FD_IOC_LOG_CALL(__func__);

    ret_value = H5FD__ioc_fapl_copy(&(file->fa));

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_fapl_get() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_fapl_copy
 *
 * Purpose:     Copies the file access properties.
 *
 * Return:      Success:    Pointer to a new property list info structure.
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
static void *
H5FD__ioc_fapl_copy(const void *_old_fa)
{
    const H5FD_ioc_config_t *old_fa_ptr = (const H5FD_ioc_config_t *)_old_fa;
    H5FD_ioc_config_t       *new_fa_ptr = NULL;
    void                    *ret_value  = NULL;

    H5FD_IOC_LOG_CALL(__func__);

    assert(old_fa_ptr);

    new_fa_ptr = H5FL_CALLOC(H5FD_ioc_config_t);
    if (NULL == new_fa_ptr)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "unable to allocate log file FAPL");

    memcpy(new_fa_ptr, old_fa_ptr, sizeof(H5FD_ioc_config_t));

    ret_value = (void *)new_fa_ptr;

done:
    if (NULL == ret_value)
        if (new_fa_ptr)
            new_fa_ptr = H5FL_FREE(H5FD_ioc_config_t, new_fa_ptr);

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_fapl_copy() */

/*--------------------------------------------------------------------------
 * Function:    H5FD__ioc_fapl_free
 *
 * Purpose:     Releases the file access lists
 *
 * Return:      SUCCEED/FAIL
 *--------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_fapl_free(void *_fapl)
{
    H5FD_ioc_config_t *fapl      = (H5FD_ioc_config_t *)_fapl;
    herr_t             ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    /* Check arguments */
    assert(fapl);

    /* Free the property list */
    fapl = H5FL_FREE(H5FD_ioc_config_t, fapl);

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_fapl_free() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_open
 *
 * Purpose:     Create and/or opens a file as an HDF5 file.
 *
 * Return:      Success:    A pointer to a new file data structure. The
 *                          public fields will be initialized by the
 *                          caller, which is always H5FD_open().
 *              Failure:    NULL
 *-------------------------------------------------------------------------
 */
static H5FD_t *
H5FD__ioc_open(const char *name, unsigned flags, hid_t fapl_id, haddr_t maxaddr)
{
    H5FD_ioc_t              *file_ptr   = NULL; /* Ioc VFD info */
    const H5FD_ioc_config_t *config_ptr = NULL; /* Driver-specific property list */
    subfiling_context_t     *sf_context = NULL;
    H5FD_ioc_config_t        default_config;
    H5P_genplist_t          *plist_ptr = NULL;
    int                      ioc_flags;
    int                      mpi_inited = 0;
    int                      mpi_code; /* MPI return code */
    H5FD_t                  *ret_value = NULL;

    H5FD_IOC_LOG_CALL(__func__);

    /* Check arguments */
    if (!name || !*name)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file name");
    if (0 == maxaddr || HADDR_UNDEF == maxaddr)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADRANGE, NULL, "bogus maxaddr");
    if (ADDR_OVERFLOW(maxaddr))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, NULL, "bogus maxaddr");

    if (NULL == (file_ptr = (H5FD_ioc_t *)H5FL_CALLOC(H5FD_ioc_t)))
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTALLOC, NULL, "unable to allocate file struct");
    file_ptr->comm       = MPI_COMM_NULL;
    file_ptr->info       = MPI_INFO_NULL;
    file_ptr->file_id    = UINT64_MAX;
    file_ptr->context_id = -1;

    /* Initialize file pointer's subfiling parameters */
    file_ptr->subf_config.ioc_selection = SELECT_IOC_ONE_PER_NODE;
    file_ptr->subf_config.stripe_size   = H5FD_SUBFILING_DEFAULT_STRIPE_SIZE;
    file_ptr->subf_config.stripe_count  = H5FD_SUBFILING_DEFAULT_STRIPE_COUNT;

    /* Get the driver-specific file access properties */
    if (NULL == (plist_ptr = (H5P_genplist_t *)H5I_object(fapl_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list");

    if (H5FD_mpi_self_initialized) {
        file_ptr->comm = MPI_COMM_WORLD;
        file_ptr->info = MPI_INFO_NULL;

        mpi_inited = 1;
    }
    else {
        /* Get the MPI communicator and info object from the property list */
        if (H5P_get(plist_ptr, H5F_ACS_MPI_PARAMS_COMM_NAME, &file_ptr->comm) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get MPI communicator");
        if (H5P_get(plist_ptr, H5F_ACS_MPI_PARAMS_INFO_NAME, &file_ptr->info) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get MPI info object");

        if (file_ptr->comm == MPI_COMM_NULL)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_BADVALUE, NULL, "invalid or unset MPI communicator in FAPL");

        /* Get the status of MPI initialization */
        if (MPI_SUCCESS != (mpi_code = MPI_Initialized(&mpi_inited)))
            H5_SUBFILING_MPI_GOTO_ERROR(NULL, "MPI_Initialized failed", mpi_code);
        if (!mpi_inited)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_UNINITIALIZED, NULL, "MPI has not been initialized");
    }

    /* Get the MPI rank of this process and the total number of processes */
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(file_ptr->comm, &file_ptr->mpi_rank)))
        H5_SUBFILING_MPI_GOTO_ERROR(NULL, "MPI_Comm_rank failed", mpi_code);
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(file_ptr->comm, &file_ptr->mpi_size)))
        H5_SUBFILING_MPI_GOTO_ERROR(NULL, "MPI_Comm_size failed", mpi_code);

    config_ptr = H5P_peek_driver_info(plist_ptr);
    if (!config_ptr || (H5P_FILE_ACCESS_DEFAULT == fapl_id)) {
        if (H5FD__ioc_get_default_config(&default_config) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get default IOC VFD configuration");
        config_ptr = &default_config;
    }

    /* Fill in the file config values */
    memcpy(&file_ptr->fa, config_ptr, sizeof(H5FD_ioc_config_t));

    /* Fully resolve the given filepath and get its dirname */
    if (H5_resolve_pathname(name, file_ptr->comm, &file_ptr->file_path) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't resolve filepath");
    if (H5_dirname(file_ptr->file_path, &file_ptr->file_dir) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTGET, NULL, "can't get filepath dirname");

    /* Translate the HDF5 file open flags into standard POSIX open flags */
    ioc_flags = (H5F_ACC_RDWR & flags) ? O_RDWR : O_RDONLY;
    if (H5F_ACC_TRUNC & flags)
        ioc_flags |= O_TRUNC;
    if (H5F_ACC_CREAT & flags)
        ioc_flags |= O_CREAT;
    if (H5F_ACC_EXCL & flags)
        ioc_flags |= O_EXCL;

    if (NULL == (plist_ptr = (H5P_genplist_t *)H5I_object(fapl_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a file access property list");

    /* Retrieve the subfiling configuration for the current file */
    if (H5_subfiling_get_config_prop(plist_ptr, &file_ptr->subf_config) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get subfiling configuration from FAPL");
    if (H5_subfiling_validate_config(&file_ptr->subf_config) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_BADVALUE, NULL, "invalid subfiling configuration");

    /* Retrieve the HDF5 stub file ID for the current file */
    if (H5_subfiling_get_file_id_prop(plist_ptr, &file_ptr->file_id) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get stub file ID from FAPL");
    if (file_ptr->file_id == UINT64_MAX) {
        /* Since this VFD does no displaying of error stacks itself
         * (it relies on the Subfiling VFD to do this), we must print
         * the error stack here if we know it wasn't stacked under the
         * Subfiling VFD.
         */
        H5_SUBFILING_DONE_ERROR(
            H5E_PLIST, H5E_BADVALUE, NULL,
            "subfiling stub file ID property was missing from FAPL - IOC VFD wasn't correctly stacked under "
            "the Subfiling VFD and cannot currently be used alone");
        PRINT_ERROR_STACK;
        goto done;
    }

    /*
     * Open the subfiles for this HDF5 file. A subfiling
     * context ID will be returned, which is used for
     * further interactions with this file's subfiles.
     */
    if (H5_open_subfiles(file_ptr->file_path, file_ptr->file_id, &file_ptr->subf_config, ioc_flags,
                         file_ptr->comm, &file_ptr->context_id) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL, "unable to open subfiles for file '%s'",
                                name);

    /* Initialize I/O concentrator threads if this MPI rank is an I/O concentrator */
    sf_context = H5_get_subfiling_object(file_ptr->context_id);
    if (sf_context && sf_context->topology->rank_is_ioc) {
        if (initialize_ioc_threads(sf_context) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTINIT, NULL,
                                    "unable to initialize I/O concentrator threads");
    }

    ret_value = (H5FD_t *)file_ptr;

done:
    /*
     * Check if any ranks failed before exit. The objective
     * here is twofold:
     *
     *   - prevent possible hangs caused by ranks sending
     *     messages to I/O concentrators that failed and
     *     didn't spin up
     *   - use the barrier semantics of MPI_Allreduce to
     *     ensure that the I/O concentrators are fully up
     *     and running before proceeding.
     */
    if (mpi_inited) {
        MPI_Comm reduce_comm = MPI_COMM_WORLD;
        int      mpi_size    = -1;
        int      err_result  = (ret_value == NULL);

        if (file_ptr && (file_ptr->comm != MPI_COMM_NULL))
            reduce_comm = file_ptr->comm;

        if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(reduce_comm, &mpi_size)))
            H5_SUBFILING_MPI_DONE_ERROR(NULL, "MPI_Comm_size failed", mpi_code);

        if (mpi_size > 1) {
            if (MPI_SUCCESS !=
                (mpi_code = MPI_Allreduce(MPI_IN_PLACE, &err_result, 1, MPI_INT, MPI_MAX, reduce_comm)))
                H5_SUBFILING_MPI_DONE_ERROR(NULL, "MPI_Allreduce failed", mpi_code);
        }

        if (err_result)
            H5_SUBFILING_DONE_ERROR(H5E_FILE, H5E_CANTOPENFILE, NULL,
                                    "one or more MPI ranks were unable to open file '%s'", name);
    }

    if (NULL == ret_value) {
        if (file_ptr) {
            if (H5FD__ioc_close_int(file_ptr) < 0)
                H5_SUBFILING_DONE_ERROR(H5E_FILE, H5E_CLOSEERROR, NULL, "can't close IOC file");
        }
    } /* end if error */

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_open() */

static herr_t
H5FD__ioc_close_int(H5FD_ioc_t *file_ptr)
{
    int    mpi_finalized;
    int    mpi_code;
    herr_t ret_value = SUCCEED;

    assert(file_ptr);

    if (MPI_SUCCESS != (mpi_code = MPI_Finalized(&mpi_finalized)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Finalized failed", mpi_code);

    if (file_ptr->context_id >= 0) {
        subfiling_context_t *sf_context = H5_get_subfiling_object(file_ptr->context_id);

        /* Don't allow IOC threads to be finalized until everyone gets here */
        if (!mpi_finalized && (file_ptr->mpi_size > 1))
            if (MPI_SUCCESS != (mpi_code = MPI_Barrier(file_ptr->comm)))
                H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Barrier failed", mpi_code);

        if (sf_context && sf_context->topology->rank_is_ioc) {
            if (finalize_ioc_threads(sf_context) < 0)
                /* Note that closing of subfiles is collective */
                H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, FAIL, "unable to finalize IOC threads");
        }

        if (H5_close_subfiles(file_ptr->context_id, file_ptr->comm) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTCLOSEFILE, FAIL, "unable to close subfiling file(s)");
        file_ptr->context_id = -1;
    }

    if (!mpi_finalized) {
        if (H5_mpi_comm_free(&file_ptr->comm) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "unable to free MPI Communicator");
        if (H5_mpi_info_free(&file_ptr->info) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "unable to free MPI Info object");
    }

done:
    free(file_ptr->file_path);
    file_ptr->file_path = NULL;

    H5MM_free(file_ptr->file_dir);
    file_ptr->file_dir = NULL;

    /* Release the file info */
    file_ptr = H5FL_FREE(H5FD_ioc_t, file_ptr);

    H5_SUBFILING_FUNC_LEAVE;
}

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_close
 *
 * Purpose:     Closes files
 *
 * Return:      Success:    SUCCEED
 *              Failure:    FAIL, file not closed.
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_close(H5FD_t *_file)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    if (H5FD__ioc_close_int(file) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close IOC file");

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_close() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_cmp
 *
 * Purpose:     Compare the keys of two files.
 *
 * Return:      Success:    A value like strcmp()
 *              Failure:    Must never fail
 *-------------------------------------------------------------------------
 */
static int
H5FD__ioc_cmp(const H5FD_t *_f1, const H5FD_t *_f2)
{
    const H5FD_ioc_t *f1        = (const H5FD_ioc_t *)_f1;
    const H5FD_ioc_t *f2        = (const H5FD_ioc_t *)_f2;
    herr_t            ret_value = 0; /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    assert(f1);
    assert(f2);

    ret_value = (f1->file_id > f2->file_id) - (f1->file_id < f2->file_id);

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_cmp */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_query
 *
 * Purpose:     Set the flags that this VFL driver is capable of supporting.
 *              (listed in H5FDpublic.h)
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_query(const H5FD_t H5_ATTR_UNUSED *_file, unsigned long *flags /* out */)
{
    herr_t ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    /* Set the VFL feature flags that this driver supports */
    if (flags) {
        *flags = 0;
        *flags |= H5FD_FEAT_AGGREGATE_METADATA;  /* OK to aggregate metadata allocations  */
        *flags |= H5FD_FEAT_AGGREGATE_SMALLDATA; /* OK to aggregate "small" raw data allocations */
        *flags |= H5FD_FEAT_HAS_MPI;             /* This driver uses MPI */
    }

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_query() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_get_type_map
 *
 * Purpose:     Retrieve the memory type mapping for this file
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_get_type_map(const H5FD_t H5_ATTR_UNUSED *_file, H5FD_mem_t H5_ATTR_UNUSED *type_map)
{
    herr_t ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    /* TODO: placeholder for now */

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_get_type_map() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_alloc
 *
 * Purpose:     Allocate file memory.
 *
 * Return:      Address of allocated space (HADDR_UNDEF if error).
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD__ioc_alloc(H5FD_t H5_ATTR_UNUSED *_file, H5FD_mem_t H5_ATTR_UNUSED type, hid_t H5_ATTR_UNUSED dxpl_id,
                hsize_t H5_ATTR_UNUSED size)
{
    haddr_t ret_value = HADDR_UNDEF; /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    /* TODO: placeholder for now */

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_alloc() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_free
 *
 * Purpose:     Free the resources for the ioc VFD.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_free(H5FD_t H5_ATTR_UNUSED *_file, H5FD_mem_t H5_ATTR_UNUSED type, hid_t H5_ATTR_UNUSED dxpl_id,
               haddr_t H5_ATTR_UNUSED addr, hsize_t H5_ATTR_UNUSED size)
{
    herr_t ret_value = SUCCEED; /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    /* TODO: placeholder for now */

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_free() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_get_eoa
 *
 * Purpose:     Returns the end-of-address marker for the file. The EOA
 *              marker is the first address past the last byte allocated in
 *              the format address space.
 *
 * Return:      Success:    The end-of-address-marker
 *
 *              Failure:    HADDR_UNDEF
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD__ioc_get_eoa(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_ioc_t *file      = (const H5FD_ioc_t *)_file;
    haddr_t           ret_value = HADDR_UNDEF;

    H5FD_IOC_LOG_CALL(__func__);

    /* Sanity check */
    assert(file);

    ret_value = file->eoa;

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_get_eoa */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_set_eoa
 *
 * Purpose:     Set the end-of-address marker for the file. This function is
 *              called shortly after an existing HDF5 file is opened in order
 *              to tell the driver where the end of the HDF5 data is located.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_set_eoa(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, haddr_t addr)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */

    H5FD_IOC_LOG_CALL(__func__);

    /* Sanity check */
    assert(file);

    file->eoa = addr;

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_set_eoa() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_get_eof
 *
 * Purpose:     Returns the end-of-address marker for the file. The EOA
 *              marker is the first address past the last byte allocated in
 *              the format address space.
 *
 * Return:      Success:    The end-of-address-marker
 *
 *              Failure:    HADDR_UNDEF
 *-------------------------------------------------------------------------
 */
static haddr_t
H5FD__ioc_get_eof(const H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type)
{
    const H5FD_ioc_t    *file       = (const H5FD_ioc_t *)_file;
    haddr_t              ret_value  = HADDR_UNDEF; /* Return value */
    subfiling_context_t *sf_context = NULL;

    H5FD_IOC_LOG_CALL(__func__);

    /* Sanity check */
    assert(file);

    sf_context = H5_get_subfiling_object(file->context_id);
    if (sf_context) {
        ret_value = sf_context->sf_eof;
        goto done;
    }
    else
        ret_value = file->eof;

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_get_eof */

/*--------------------------------------------------------------------------
 * Function:    H5FD__ioc_get_handle
 *
 * Purpose:     Returns a pointer to the file handle of low-level virtual
 *              file driver.
 *
 * Return:      SUCCEED/FAIL
 *--------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_get_handle(H5FD_t H5_ATTR_UNUSED *_file, hid_t H5_ATTR_UNUSED fapl,
                     void H5_ATTR_UNUSED **file_handle)
{
    herr_t ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    /* TODO: placeholder for now */

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_get_handle */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_read
 *
 * Purpose:     Reads SIZE bytes of data from the R/W channel, beginning at
 *              address ADDR into buffer BUF according to data transfer
 *              properties in DXPL_ID.
 *
 * Return:      Success:    SUCCEED
 *                          The read result is written into the BUF buffer
 *                          which should be allocated by the caller.
 *              Failure:    FAIL
 *                          The contents of BUF are undefined.
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_read(H5FD_t *_file, H5FD_mem_t H5_ATTR_UNUSED type, hid_t H5_ATTR_UNUSED dxpl_id, haddr_t addr,
               size_t size, void *buf)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    assert(file && file->pub.cls);
    assert(buf);

    /* Check for overflow conditions */
    if (!H5_addr_defined(addr))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "addr undefined, addr = %" PRIuHADDR, addr);
    if (REGION_OVERFLOW(addr, size))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_OVERFLOW, FAIL, "addr overflow, addr = %" PRIuHADDR, addr);

    ret_value = H5FD__ioc_read_vector_internal(_file, 1, &addr, &size, &buf);

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_read() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_write
 *
 * Purpose:     Writes SIZE bytes of data to IOC file, beginning at address
 *              ADDR from buffer BUF according to data transfer properties
 *              in DXPL_ID.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_write(H5FD_t *_file, H5FD_mem_t type, hid_t H5_ATTR_UNUSED dxpl_id, haddr_t addr, size_t size,
                const void *buf)
{
    herr_t ret_value = SUCCEED;

    addr += _file->base_addr;

    ret_value = H5FD__ioc_write_vector_internal(_file, 1, &type, &addr, &size, &buf);

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_write() */

static herr_t
H5FD__ioc_read_vector(H5FD_t *_file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[], haddr_t addrs[],
                      size_t sizes[], void *bufs[] /* out */)
{
    H5FD_ioc_t *file_ptr  = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */

    /* Check arguments */
    if (!file_ptr)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file pointer cannot be NULL");

    if ((!types) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "types parameter can't be NULL if count is positive");

    if ((!addrs) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "addrs parameter can't be NULL if count is positive");

    if ((!sizes) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "sizes parameter can't be NULL if count is positive");

    if ((!bufs) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "bufs parameter can't be NULL if count is positive");

    /* Get the default dataset transfer property list if the user didn't provide
     * one */
    if (H5P_DEFAULT == dxpl_id) {
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    }
    else {
        if (TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list");
    }

    ret_value = H5FD__ioc_read_vector_internal(_file, count, addrs, sizes, bufs);

done:
    H5_SUBFILING_FUNC_LEAVE;
}

static herr_t
H5FD__ioc_write_vector(H5FD_t *_file, hid_t dxpl_id, uint32_t count, H5FD_mem_t types[], haddr_t addrs[],
                       size_t sizes[], const void *bufs[] /* in */)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED; /* Return value */

    /* Check arguments */
    if (!file)
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "file pointer cannot be NULL");

    if ((!types) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "types parameter can't be NULL if count is positive");

    if ((!addrs) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "addrs parameter can't be NULL if count is positive");

    if ((!sizes) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "sizes parameter can't be NULL if count is positive");

    if ((!bufs) && (count > 0))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL,
                                "bufs parameter can't be NULL if count is positive");

    /* Get the default dataset transfer property list if the user didn't provide
     * one */
    if (H5P_DEFAULT == dxpl_id) {
        dxpl_id = H5P_DATASET_XFER_DEFAULT;
    }
    else {
        if (TRUE != H5P_isa_class(dxpl_id, H5P_DATASET_XFER))
            H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a data transfer property list");
    }

    ret_value = H5FD__ioc_write_vector_internal(_file, count, types, addrs, sizes, bufs);

done:
    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FDioc__write_vector() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_flush
 *
 * Purpose:     Flushes all data to disk for underlying VFD.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_flush(H5FD_t H5_ATTR_UNUSED *_file, hid_t H5_ATTR_UNUSED dxpl_id, hbool_t H5_ATTR_UNUSED closing)
{
    herr_t ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    /* TODO: placeholder for now */

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_flush() */

/*-------------------------------------------------------------------------
 * Function:    H5FD__ioc_truncate
 *
 * Purpose:     Notify driver to truncate the file back to the allocated size.
 *
 * Return:      SUCCEED/FAIL
 *-------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_truncate(H5FD_t *_file, hid_t H5_ATTR_UNUSED dxpl_id, hbool_t H5_ATTR_UNUSED closing)
{
    H5FD_ioc_t *file      = (H5FD_ioc_t *)_file;
    herr_t      ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    assert(file);

    /* TODO: placeholder for now since Subfiling does the truncation */
    if (!H5_addr_eq(file->eoa, file->last_eoa)) {
        file->last_eoa = file->eoa;
    }

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_truncate */

/*--------------------------------------------------------------------------
 * Function:    H5FD__ioc_lock
 *
 * Purpose:     Sets a file lock.
 *
 * Return:      SUCCEED/FAIL
 *--------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_lock(H5FD_t H5_ATTR_UNUSED *_file, hbool_t H5_ATTR_UNUSED rw)
{
    herr_t ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    /* TODO: placeholder for now */

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_lock */

/*--------------------------------------------------------------------------
 * Function:    H5FD__ioc_unlock
 *
 * Purpose:     Removes a file lock.
 *
 * Return:      SUCCEED/FAIL
 *--------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_unlock(H5FD_t H5_ATTR_UNUSED *_file)
{
    herr_t ret_value = SUCCEED;

    H5FD_IOC_LOG_CALL(__func__);

    /* TODO: placeholder for now */

    H5_SUBFILING_FUNC_LEAVE;
} /* end H5FD__ioc_unlock */

static herr_t
H5FD__ioc_del(const char *name, hid_t fapl)
{
    H5P_genplist_t *plist;
    h5_stat_t       st;
    MPI_Comm        comm          = MPI_COMM_NULL;
    MPI_Info        info          = MPI_INFO_NULL;
    FILE           *config_file   = NULL;
    char           *tmp_filename  = NULL;
    char           *base_filename = NULL;
    char           *file_dirname  = NULL;
    int             mpi_rank      = INT_MAX;
    int             mpi_code;
    herr_t          ret_value = SUCCEED;

    /* TODO: Eventually this routine should share common code
     * with H5_subfiling_common's routines so it doesn't get
     * out of sync
     */

    if (NULL == (plist = H5P_object_verify(fapl, H5P_FILE_ACCESS)))
        H5_SUBFILING_GOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file access property list");
    assert(H5FD_IOC == H5P_peek_driver(plist));

    if (H5FD_mpi_self_initialized) {
        comm = MPI_COMM_WORLD;
    }
    else {
        /* Get the MPI communicator and info from the fapl */
        if (H5P_get(plist, H5F_ACS_MPI_PARAMS_INFO_NAME, &info) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI info object");
        if (H5P_get(plist, H5F_ACS_MPI_PARAMS_COMM_NAME, &comm) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get MPI communicator");
    }

    /* Get the MPI rank of this process */
    if (MPI_SUCCESS != (mpi_code = MPI_Comm_rank(comm, &mpi_rank)))
        H5_SUBFILING_MPI_GOTO_ERROR(FAIL, "MPI_Comm_rank failed", mpi_code);

    if (mpi_rank == 0) {
        int64_t read_n_subfiles = 0;
        int32_t n_subfiles      = 0;
        char   *prefix_env      = NULL;
        int     num_digits      = 0;

        if (HDstat(name, &st) < 0)
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_SYSERRSTR, FAIL, "HDstat failed");

        if (H5_basename(name, &base_filename) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't get file basename");
        if (H5_dirname(name, &file_dirname) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't get file dirname");

        /* Try to open the subfiling configuration file and get the number of IOCs */
        if (NULL == (tmp_filename = malloc(PATH_MAX)))
            H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL,
                                    "can't allocate config file name buffer");

        /* Check if a prefix has been set for the configuration file name */
        prefix_env = HDgetenv(H5FD_SUBFILING_CONFIG_FILE_PREFIX);

        /* TODO: No support for subfile directory prefix currently */
        /* TODO: Possibly try loading config file prefix from file before deleting */
        HDsnprintf(tmp_filename, PATH_MAX, "%s/" H5FD_SUBFILING_CONFIG_FILENAME_TEMPLATE,
                   prefix_env ? prefix_env : file_dirname, base_filename, (uint64_t)st.st_ino);

        if (NULL == (config_file = fopen(tmp_filename, "r"))) {
            if (ENOENT == errno) {
#ifdef H5FD_IOC_DEBUG
                printf("** WARNING: couldn't delete Subfiling configuration file '%s'\n", tmp_filename);
#endif

                H5_SUBFILING_GOTO_DONE(SUCCEED);
            }
            else
                H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL,
                                            "can't open subfiling config file");
        }

        if (H5_get_subfiling_config_from_file(config_file, NULL, &read_n_subfiles) < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_FILE, H5E_READERROR, FAIL, "can't read subfiling config file");

        H5_CHECK_OVERFLOW(read_n_subfiles, int64_t, int32_t);
        n_subfiles = (int32_t)read_n_subfiles;

        /* Delete the Subfiling configuration file */
        if (EOF == fclose(config_file)) {
            config_file = NULL;
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL,
                                        "can't close subfiling config file");
        }

        config_file = NULL;

        if (HDremove(tmp_filename) < 0)
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL,
                                        "can't delete subfiling config file");

        /* Try to delete each of the subfiles */
        num_digits = (int)(HDlog10(n_subfiles) + 1);

        for (int i = 0; i < n_subfiles; i++) {
            /* TODO: No support for subfile directory prefix currently */
            HDsnprintf(tmp_filename, PATH_MAX, "%s/" H5FD_SUBFILING_FILENAME_TEMPLATE, file_dirname,
                       base_filename, (uint64_t)st.st_ino, num_digits, i + 1, n_subfiles);

            if (HDremove(tmp_filename) < 0) {
#ifdef H5FD_IOC_DEBUG
                printf("** WARNING: couldn't delete subfile '%s'\n", tmp_filename);
#endif

                if (ENOENT != errno)
                    H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_CANTDELETEFILE, FAIL, "can't delete subfile");
            }
        }

        /* Delete the HDF5 stub file */
        if (HDremove(name) < 0)
            H5_SUBFILING_SYS_GOTO_ERROR(H5E_FILE, H5E_CANTDELETEFILE, FAIL, "can't delete HDF5 file");
    }

done:
    if (config_file)
        if (EOF == fclose(config_file))
            H5_SUBFILING_DONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close subfiling config file");

    /* Set up a barrier (don't want processes to run ahead of the delete) */
    if (comm != MPI_COMM_NULL) {
        int comm_size = -1;

        if (MPI_SUCCESS != (mpi_code = MPI_Comm_size(comm, &comm_size)))
            H5_SUBFILING_MPI_DONE_ERROR(FAIL, "MPI_Comm_size failed", mpi_code);

        if (comm_size > 1)
            if (MPI_SUCCESS != (mpi_code = MPI_Barrier(comm)))
                H5_SUBFILING_MPI_DONE_ERROR(FAIL, "MPI_Barrier failed", mpi_code);
    }

    /* Free duplicated MPI Communicator and Info objects */
    if (H5_mpi_comm_free(&comm) < 0)
        H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "unable to free MPI communicator");
    if (H5_mpi_info_free(&info) < 0)
        H5_SUBFILING_DONE_ERROR(H5E_VFL, H5E_CANTFREE, FAIL, "unable to free MPI info object");

    free(tmp_filename);
    H5MM_free(file_dirname);
    H5MM_free(base_filename);

    H5_SUBFILING_FUNC_LEAVE;
}

/*--------------------------------------------------------------------------
 * Function:   H5FD__ioc_write_vector_internal
 *
 * Purpose:    This function takes 'count' vector entries
 *             and initiates an asynch write operation for each.
 *             By asynchronous, we mean that MPI_Isends are utilized
 *             to communicate the write operations to the 'count'
 *             IO Concentrators.  The calling function will have
 *             decomposed the actual user IO request into the
 *             component segments, each IO having a maximum size
 *             of "stripe_depth", which is recorded in the
 *             subfiling_context_t 'sf_context' structure.
 *
 * Return:     SUCCEED if no errors, FAIL otherwise.
 *--------------------------------------------------------------------------
 */
static herr_t
H5FD__ioc_write_vector_internal(H5FD_t *_file, uint32_t count, H5FD_mem_t H5_ATTR_UNUSED types[],
                                haddr_t addrs[], size_t sizes[], const void *bufs[] /* in */)
{
    subfiling_context_t *sf_context    = NULL;
    MPI_Request         *mpi_reqs      = NULL;
    H5FD_ioc_t          *file_ptr      = (H5FD_ioc_t *)_file;
    io_req_t           **sf_io_reqs    = NULL;
    int64_t              sf_context_id = -1;
    herr_t               ret_value     = SUCCEED;

    assert(_file);
    assert(addrs);
    assert(sizes);
    assert(bufs);

    if (count == 0)
        H5_SUBFILING_GOTO_DONE(SUCCEED);

    sf_context_id = file_ptr->context_id;

    if (NULL == (sf_context = H5_get_subfiling_object(sf_context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "can't get subfiling context from ID");
    assert(sf_context->topology);

    /*
     * Allocate an array of I/O requests and an array twice that size for
     * MPI_Request objects. Each write I/O request has an MPI_Request
     * object for the I/O data transfer and an MPI_Request object that,
     * when waited on until completion, signifies that the actual I/O
     * call (currently, HDpwrite) has completed. This is needed for ensuring
     * that blocking write calls do not return early before the data is
     * actually written.
     */
    if (NULL == (sf_io_reqs = calloc((size_t)count, sizeof(*sf_io_reqs))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate I/O request array");
    if (NULL == (mpi_reqs = malloc(2 * (size_t)count * sizeof(*mpi_reqs))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate MPI request array");

    /* Each pass thru the following should queue an MPI write
     * to a new IOC. Both the IOC selection and offset within the
     * particular subfile are based on the combination of striping
     * factors and the virtual file offset (addrs[i]).
     */
    for (size_t i = 0; i < (size_t)count; i++) {
        herr_t write_status;

        if (sizes[i] == 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "invalid size argument of 0");

        H5_CHECK_OVERFLOW(addrs[i], haddr_t, int64_t);
        H5_CHECK_OVERFLOW(sizes[i], size_t, int64_t);
        write_status = ioc__write_independent_async(sf_context_id, (int64_t)addrs[i], (int64_t)sizes[i],
                                                    bufs[i], &sf_io_reqs[i]);

        if (write_status < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "couldn't queue write operation");

        mpi_reqs[(2 * i)]     = sf_io_reqs[i]->io_transfer_req;
        mpi_reqs[(2 * i) + 1] = sf_io_reqs[i]->io_comp_req;
    }

    /* Here, we should have queued 'count' async requests.
     * We can can now try to complete those before returning
     * to the caller for the next set of IO operations.
     */
    if (ioc__async_completion(mpi_reqs, 2 * (size_t)count) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_WRITEERROR, FAIL, "can't complete I/O requests");

done:
    free(mpi_reqs);

    if (sf_io_reqs) {
        for (size_t i = 0; i < count; i++)
            free(sf_io_reqs[i]);
        free(sf_io_reqs);
    }

    H5_SUBFILING_FUNC_LEAVE;
}

static herr_t
H5FD__ioc_read_vector_internal(H5FD_t *_file, uint32_t count, haddr_t addrs[], size_t sizes[],
                               void *bufs[] /* out */)
{
    subfiling_context_t *sf_context    = NULL;
    MPI_Request         *mpi_reqs      = NULL;
    H5FD_ioc_t          *file_ptr      = (H5FD_ioc_t *)_file;
    io_req_t           **sf_io_reqs    = NULL;
    int64_t              sf_context_id = -1;
    herr_t               ret_value     = SUCCEED;

    assert(_file);
    assert(addrs);
    assert(sizes);
    assert(bufs);

    if (count == 0)
        H5_SUBFILING_GOTO_DONE(SUCCEED);

    sf_context_id = file_ptr->context_id;

    if (NULL == (sf_context = H5_get_subfiling_object(sf_context_id)))
        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_CANTGET, FAIL, "can't get subfiling context from ID");
    assert(sf_context->topology);

    /*
     * Allocate an array of I/O requests and an array for MPI_Request
     * objects. Each read I/O request has an MPI_Request object for the
     * I/O data transfer that, when waited on until completion, signifies
     * that the actual I/O call (currently, HDpread) has completed and
     * the data read from the file has been transferred to the caller.
     */
    if (NULL == (sf_io_reqs = calloc((size_t)count, sizeof(*sf_io_reqs))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate I/O request array");
    if (NULL == (mpi_reqs = malloc((size_t)count * sizeof(*mpi_reqs))))
        H5_SUBFILING_GOTO_ERROR(H5E_RESOURCE, H5E_CANTALLOC, FAIL, "can't allocate MPI request array");

    for (size_t i = 0; i < (size_t)count; i++) {
        int read_status;

        H5_CHECK_OVERFLOW(addrs[i], haddr_t, int64_t);
        H5_CHECK_OVERFLOW(sizes[i], size_t, int64_t);
        read_status = ioc__read_independent_async(sf_context_id, (int64_t)addrs[i], (int64_t)sizes[i],
                                                  bufs[i], &sf_io_reqs[i]);

        if (read_status < 0)
            H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "couldn't queue read operation");

        mpi_reqs[i] = sf_io_reqs[i]->io_transfer_req;
    }

    /* Here, we should have queued 'count' async requests
     * (one to each required IOC).
     *
     * We can can now try to complete those before returning
     * to the caller for the next set of IO operations.
     */
    if (ioc__async_completion(mpi_reqs, (size_t)count) < 0)
        H5_SUBFILING_GOTO_ERROR(H5E_IO, H5E_READERROR, FAIL, "can't complete I/O requests");

done:
    free(mpi_reqs);

    if (sf_io_reqs) {
        for (size_t i = 0; i < count; i++)
            free(sf_io_reqs[i]);
        free(sf_io_reqs);
    }

    H5_SUBFILING_FUNC_LEAVE;
}

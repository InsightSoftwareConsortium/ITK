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
 * Created:		H5Pdapl.c
 *              October 27, 2008
 *              Neil Fortner <nfortne2@hdfgroup.org>
 *
 * Purpose:		Dataset access property list class routines
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/
#define H5P_PACKAGE		/*suppress error about including H5Ppkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions */
#include "H5Dprivate.h"		/* Datasets */
#include "H5Eprivate.h"		/* Error handling */
#include "H5Fprivate.h"		/* Files */
#include "H5Iprivate.h"		/* IDs */
#include "H5Ppkg.h"         /* Property lists */
#include "H5MMprivate.h"	/* Memory management */


/****************/
/* Local Macros */
/****************/

/* ========= Dataset Access properties ============ */
/* Definitions for size of raw data chunk cache(slots) */
#define H5D_ACS_DATA_CACHE_NUM_SLOTS_SIZE       sizeof(size_t)
#define H5D_ACS_DATA_CACHE_NUM_SLOTS_DEF        H5D_CHUNK_CACHE_NSLOTS_DEFAULT
/* Definition for size of raw data chunk cache(bytes) */
#define H5D_ACS_DATA_CACHE_BYTE_SIZE_SIZE       sizeof(size_t)
#define H5D_ACS_DATA_CACHE_BYTE_SIZE_DEF        H5D_CHUNK_CACHE_NBYTES_DEFAULT
/* Definition for preemption read chunks first */
#define H5D_ACS_PREEMPT_READ_CHUNKS_SIZE        sizeof(double)
#define H5D_ACS_PREEMPT_READ_CHUNKS_DEF         H5D_CHUNK_CACHE_W0_DEFAULT
/* Definitions for external file prefix */
#define H5D_ACS_EFILE_PREFIX_SIZE               sizeof(char *)
#define H5D_ACS_EFILE_PREFIX_DEF                NULL /*default is no prefix */
#define H5D_ACS_EFILE_PREFIX_SET                H5P__dapl_efile_pref_set
#define H5D_ACS_EFILE_PREFIX_GET                H5P__dapl_efile_pref_get
#define H5D_ACS_EFILE_PREFIX_DEL                H5P__dapl_efile_pref_del
#define H5D_ACS_EFILE_PREFIX_COPY               H5P__dapl_efile_pref_copy
#define H5D_ACS_EFILE_PREFIX_CMP                H5P__dapl_efile_pref_cmp
#define H5D_ACS_EFILE_PREFIX_CLOSE              H5P__dapl_efile_pref_close


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Property class callbacks */
static herr_t H5P__dacc_reg_prop(H5P_genclass_t *pclass);

/* Property list callbacks */
static herr_t H5P__dapl_efile_pref_set(hid_t prop_id, const char* name, size_t size, void* value);
static herr_t H5P__dapl_efile_pref_get(hid_t prop_id, const char* name, size_t size, void* value);
static herr_t H5P__dapl_efile_pref_del(hid_t prop_id, const char* name, size_t size, void* value);
static herr_t H5P__dapl_efile_pref_copy(const char* name, size_t size, void* value);
static int H5P__dapl_efile_pref_cmp(const void *value1, const void *value2, size_t size);
static herr_t H5P__dapl_efile_pref_close(const char* name, size_t size, void* value);


/*********************/
/* Package Variables */
/*********************/

/* Dataset access property list class library initialization object */
const H5P_libclass_t H5P_CLS_DACC[1] = {{
    "dataset access",		/* Class name for debugging     */
    H5P_TYPE_DATASET_ACCESS,    /* Class type                   */

    &H5P_CLS_LINK_ACCESS_g,	/* Parent class                 */
    &H5P_CLS_DATASET_ACCESS_g,	/* Pointer to class             */
    &H5P_CLS_DATASET_ACCESS_ID_g,	/* Pointer to class ID          */
    &H5P_LST_DATASET_ACCESS_ID_g,	/* Pointer to default property list ID */
    H5P__dacc_reg_prop,		/* Default property registration routine */

    NULL,		        /* Class creation callback      */
    NULL,		        /* Class creation callback info */
    NULL,		        /* Class copy callback          */
    NULL,		        /* Class copy callback info     */
    NULL,		        /* Class close callback         */
    NULL 		        /* Class close callback info    */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Property value defaults */
static const char *H5D_def_efile_prefix_g = H5D_ACS_EFILE_PREFIX_DEF; /* Default external file prefix string */



/*-------------------------------------------------------------------------
 * Function:    H5P__dacc_reg_prop
 *
 * Purpose:     Register the dataset access property list class's
 *              properties
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              October 27, 2008
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__dacc_reg_prop(H5P_genclass_t *pclass)
{
    size_t rdcc_nslots = H5D_ACS_DATA_CACHE_NUM_SLOTS_DEF;      /* Default raw data chunk cache # of slots */
    size_t rdcc_nbytes = H5D_ACS_DATA_CACHE_BYTE_SIZE_DEF;      /* Default raw data chunk cache # of bytes */
    double rdcc_w0 = H5D_ACS_PREEMPT_READ_CHUNKS_DEF;           /* Default raw data chunk cache dirty ratio */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Register the size of raw data chunk cache (elements) */
    if(H5P_register_real(pclass, H5D_ACS_DATA_CACHE_NUM_SLOTS_NAME, H5D_ACS_DATA_CACHE_NUM_SLOTS_SIZE, &rdcc_nslots, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the size of raw data chunk cache(bytes) */
    if(H5P_register_real(pclass, H5D_ACS_DATA_CACHE_BYTE_SIZE_NAME, H5D_ACS_DATA_CACHE_BYTE_SIZE_SIZE, &rdcc_nbytes, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register the preemption for reading chunks */
    if(H5P_register_real(pclass, H5D_ACS_PREEMPT_READ_CHUNKS_NAME, H5D_ACS_PREEMPT_READ_CHUNKS_SIZE, &rdcc_w0, NULL, NULL, NULL, NULL, NULL, NULL, NULL) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

    /* Register property for external file prefix */
    if(H5P_register_real(pclass, H5D_ACS_EFILE_PREFIX_NAME, H5D_ACS_EFILE_PREFIX_SIZE, &H5D_def_efile_prefix_g, 
            NULL, H5D_ACS_EFILE_PREFIX_SET, H5D_ACS_EFILE_PREFIX_GET,
            H5D_ACS_EFILE_PREFIX_DEL, H5D_ACS_EFILE_PREFIX_COPY, H5D_ACS_EFILE_PREFIX_CMP, H5D_ACS_EFILE_PREFIX_CLOSE) < 0)
         HGOTO_ERROR(H5E_PLIST, H5E_CANTINSERT, FAIL, "can't insert property into class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__dacc_reg_prop() */


/*-------------------------------------------------------------------------
 * Function:    H5P__dapl_efile_pref_set
 *
 * Purpose:     Copies an external file prefix property when it's set
 *              for a property list
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__dapl_efile_pref_set(hid_t H5_ATTR_UNUSED prop_id, const char H5_ATTR_UNUSED *name,
    size_t H5_ATTR_UNUSED size, void *value)
{
    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(value);

    /* Copy the prefix */
    *(char **)value = H5MM_xstrdup(*(const char **)value);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__dapl_efile_pref_set() */


/*-------------------------------------------------------------------------
 * Function:    H5P__dapl_efile_pref_get
 *
 * Purpose:     Copies an external file prefix property when it's retrieved
 *              from a property list
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__dapl_efile_pref_get(hid_t H5_ATTR_UNUSED prop_id, const char H5_ATTR_UNUSED *name,
    size_t H5_ATTR_UNUSED size, void *value)
{
    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(value);

    /* Copy the prefix */
    *(char **)value = H5MM_xstrdup(*(const char **)value);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__dapl_efile_pref_get() */


/*-------------------------------------------------------------------------
 * Function:    H5P__dapl_efile_pref_del
 *
 * Purpose:     Frees memory used to store the external file prefix string
 *
 * Return:      SUCCEED (Can't fail)
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__dapl_efile_pref_del(hid_t H5_ATTR_UNUSED prop_id, const char H5_ATTR_UNUSED *name,
    size_t H5_ATTR_UNUSED size, void *value)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(value);

    H5MM_xfree(*(void **)value);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__dapl_efile_pref_del() */


/*-------------------------------------------------------------------------
 * Function:    H5P__dapl_efile_pref_copy
 *
 * Purpose:     Creates a copy of the external file prefix string
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__dapl_efile_pref_copy(const char H5_ATTR_UNUSED *name, size_t H5_ATTR_UNUSED size, void *value)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(value);

    *(char **)value = H5MM_xstrdup(*(const char **)value);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__dapl_efile_pref_copy() */


/*-------------------------------------------------------------------------
 * Function:       H5P__dapl_efile_pref_cmp
 *
 * Purpose:        Callback routine which is called whenever the efile prefix
 *                 property in the dataset creation property list is
 *                 compared.
 *
 * Return:         zero if VALUE1 and VALUE2 are equal, non zero otherwise.
 *
 *-------------------------------------------------------------------------
 */
static int
H5P__dapl_efile_pref_cmp(const void *value1, const void *value2, size_t H5_ATTR_UNUSED size)
{
    const char *pref1 = *(const char * const *)value1;
    const char *pref2 = *(const char * const *)value2;
    int ret_value = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(NULL == pref1 && NULL != pref2)
        HGOTO_DONE(1);
    if(NULL != pref1 && NULL == pref2)
        HGOTO_DONE(-1);
    if(NULL != pref1 && NULL != pref2)
        ret_value = HDstrcmp(pref1, pref2);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5P__dapl_efile_pref_cmp() */


/*-------------------------------------------------------------------------
 * Function:    H5P__dapl_efile_pref_close
 *
 * Purpose:     Frees memory used to store the external file prefix string
 *
 * Return:      SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5P__dapl_efile_pref_close(const char H5_ATTR_UNUSED *name, size_t H5_ATTR_UNUSED size, void *value)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(value);

    H5MM_xfree(*(void **)value);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5P__dapl_efile_pref_close() */


/*-------------------------------------------------------------------------
 * Function:	H5Pset_chunk_cache
 *
 * Purpose:	Set the number of objects in the meta data cache and the
 *		maximum number of chunks and bytes in the raw data chunk cache.
 *      Once set, these values will override the values in the file access
 *      property list.  Each of thhese values can be individually unset
 *      (or not set at all) by passing the macros:
 *      H5D_CHUNK_CACHE_NCHUNKS_DEFAULT,
 *      H5D_CHUNK_CACHE_NSLOTS_DEFAULT, and/or
 *      H5D_CHUNK_CACHE_W0_DEFAULT
 *      as appropriate.
 *
 * 		The RDCC_W0 value should be between 0 and 1 inclusive and
 *		indicates how much chunks that have been fully read or fully
 *		written are favored for preemption.  A value of zero means
 *		fully read or written chunks are treated no differently than
 *		other chunks (the preemption is strictly LRU) while a value
 *		of one means fully read chunks are always preempted before
 *		other chunks.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Neil Fortner
 *              Monday, October 27, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_chunk_cache(hid_t dapl_id, size_t rdcc_nslots, size_t rdcc_nbytes, double rdcc_w0)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "izzd", dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0);

    /* Check arguments.  Note that we allow negative values - they are
     * considered to "unset" the property. */
    if(rdcc_w0 > (double)1.0f)
        HGOTO_ERROR (H5E_ARGS, H5E_BADVALUE, FAIL, "raw data cache w0 value must be between 0.0 and 1.0 inclusive, or H5D_CHUNK_CACHE_W0_DEFAULT");

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(dapl_id,H5P_DATASET_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Set sizes */
    if(H5P_set(plist, H5D_ACS_DATA_CACHE_NUM_SLOTS_NAME, &rdcc_nslots) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data cache number of chunks");
    if(H5P_set(plist, H5D_ACS_DATA_CACHE_BYTE_SIZE_NAME, &rdcc_nbytes) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set data cache byte size");
    if(H5P_set(plist, H5D_ACS_PREEMPT_READ_CHUNKS_NAME, &rdcc_w0) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET,FAIL, "can't set preempt read chunks");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_chunk_cache() */


/*-------------------------------------------------------------------------
 * Function:	H5Pget_chunk_cache
 *
 * Purpose:	Retrieves the maximum possible number of elements in the meta
 *		data cache and the maximum possible number of elements and
 *		bytes and the RDCC_W0 value in the raw data chunk cache.  Any
 *		(or all) arguments may be null pointers in which case the
 *		corresponding datum is not returned.  If these properties have
 *      not been set on this property list, the default values for a
 *      file access property list are returned.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Neil Fortner
 *              Monday, October 27, 2008
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pget_chunk_cache(hid_t dapl_id, size_t *rdcc_nslots, size_t *rdcc_nbytes, double *rdcc_w0)
{
    H5P_genplist_t *plist;      /* Property list pointer */
    H5P_genplist_t *def_plist;  /* Default file access property list */
    herr_t ret_value = SUCCEED; /* return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "i*z*z*d", dapl_id, rdcc_nslots, rdcc_nbytes, rdcc_w0);

    /* Get the plist structure */
    if (NULL == (plist = H5P_object_verify(dapl_id, H5P_DATASET_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID");

    /* Get default file access plist */
    if (NULL == (def_plist = (H5P_genplist_t *)H5I_object(H5P_FILE_ACCESS_DEFAULT)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for default fapl ID");

    /* Get the properties.  If a property is set to the default value, the value
     * from the default fapl is used. */
    if (rdcc_nslots) {
        if (H5P_get(plist, H5D_ACS_DATA_CACHE_NUM_SLOTS_NAME, rdcc_nslots) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get data cache number of slots");
        if (*rdcc_nslots == H5D_CHUNK_CACHE_NSLOTS_DEFAULT)
            if (H5P_get(def_plist, H5F_ACS_DATA_CACHE_NUM_SLOTS_NAME, rdcc_nslots) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get default data cache number of slots");
    } /* end if */
    if (rdcc_nbytes) {
        if (H5P_get(plist, H5D_ACS_DATA_CACHE_BYTE_SIZE_NAME, rdcc_nbytes) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get data cache byte size");
        if (*rdcc_nbytes == H5D_CHUNK_CACHE_NBYTES_DEFAULT)
            if (H5P_get(def_plist, H5F_ACS_DATA_CACHE_BYTE_SIZE_NAME, rdcc_nbytes) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get default data cache byte size");
    } /* end if */
    if (rdcc_w0) {
        if (H5P_get(plist, H5D_ACS_PREEMPT_READ_CHUNKS_NAME, rdcc_w0) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get preempt read chunks");
        if (*rdcc_w0 < 0)
            if (H5P_get(def_plist, H5F_ACS_PREEMPT_READ_CHUNKS_NAME, rdcc_w0) < 0)
                HGOTO_ERROR(H5E_PLIST, H5E_CANTGET,FAIL, "can't get default preempt read chunks");
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:    H5Pset_efile_prefix
 *
 * Purpose:     Set a prefix to be used for any external files.
 *
 *              If the prefix starts with ${ORIGIN}, this will be replaced by
 *              the absolute path of the directory of the HDF5 file containing
 *              the dataset.
 *
 *              If the prefix is ".", no prefix will be applied.
 *
 *              This property can be overwritten by the environment variable
 *              HDF5_EXTFILE_PREFIX.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Pset_efile_prefix(hid_t plist_id, const char *prefix)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*s", plist_id, prefix);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_DATASET_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Set prefix */
    if(H5P_set(plist, H5D_ACS_EFILE_PREFIX_NAME, &prefix) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set prefix info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pset_efile_prefix() */


/*-------------------------------------------------------------------------
 * Function:    H5Pget_efile_prefix
 *
 * Purpose:	Gets the prefix to be used for any external files.
 *
 *              If the pointer is not NULL, it points to a user-allocated
 *              buffer.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Pget_efile_prefix(hid_t plist_id, char *prefix, size_t size)
{
    H5P_genplist_t *plist;              /* Property list pointer */
    char *my_prefix;                    /* Library's copy of the prefix */
    size_t	len;                    /* Length of prefix string */
    ssize_t 	ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("Zs", "i*sz", plist_id, prefix, size);

    /* Get the plist structure */
    if(NULL == (plist = H5P_object_verify(plist_id, H5P_DATASET_ACCESS)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "can't find object for ID")

    /* Get the current prefix */
    /* No error checking possible */
    my_prefix = (char *)H5P_peek_voidp(plist, H5D_ACS_EFILE_PREFIX_NAME);

    /* Check for prefix being set */
    if(my_prefix) {
        /* Copy to user's buffer, if given */
        len = HDstrlen(my_prefix);
        if(prefix) {
            HDstrncpy(prefix, my_prefix, MIN(len + 1, size));
            if(len >= size)
                prefix[size - 1] = '\0';
        } /* end if */
    } /* end if */
    else
        len = 0;

    /* Set return value */
    ret_value = (ssize_t)len;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Pget_efile_prefix() */


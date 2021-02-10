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

/*-------------------------------------------------------------------------
 *
 * Created:		H5Aint.c
 *			Dec 18 2006
 *			Quincey Koziol <koziol@hdfgroup.org>
 *
 * Purpose:		Internal routines for managing attributes.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Amodule.h"          /* This source code file is part of the H5A module */
#define H5O_FRIEND		/*suppress error about including H5Opkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                        */
#include "H5Apkg.h"             /* Attributes                               */
#include "H5CXprivate.h"        /* API Contexts                             */
#include "H5Dprivate.h"         /* Datasets                                 */
#include "H5Eprivate.h"         /* Error handling                           */
#include "H5Iprivate.h"         /* IDs                                      */
#include "H5MMprivate.h"        /* Memory management                        */
#include "H5Opkg.h"             /* Object headers                           */
#include "H5SMprivate.h"        /* Shared Object Header Messages            */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/* Data exchange structure to use when building table of compact attributes for an object */
typedef struct {
    H5F_t       *f;             /* Pointer to file that fractal heap is in */
    H5A_attr_table_t *atable;   /* Pointer to attribute table to build */
    size_t curr_attr;           /* Current attribute to operate on */
    hbool_t bogus_crt_idx;      /* Whether bogus creation index values need to be set */
} H5A_compact_bt_ud_t;

/* Data exchange structure to use when building table of dense attributes for an object */
typedef struct {
    H5A_attr_table_t *atable;   /* Pointer to attribute table to build */
    size_t curr_attr;           /* Current attribute to operate on */
} H5A_dense_bt_ud_t;

/* Data exchange structure to use when copying an attribute from _SRC to _DST */
typedef struct {
    const H5O_ainfo_t *ainfo;   /* dense information    */
    H5F_t *file;                /* file                 */
    hbool_t *recompute_size;    /* Flag to indicate if size changed */
    H5O_copy_t *cpy_info;       /* Information on copying options   */
    const H5O_loc_t *oloc_src;
    H5O_loc_t *oloc_dst;
} H5A_dense_file_cp_ud_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

static herr_t H5A__open_common(const H5G_loc_t *loc, H5A_t *attr);
static herr_t H5A__compact_build_table_cb(H5O_t *oh, H5O_mesg_t *mesg/*in,out*/,
    unsigned sequence, unsigned *oh_flags_ptr, void *_udata/*in,out*/);
static herr_t H5A__dense_build_table_cb(const H5A_t *attr, void *_udata);
static int H5A__attr_cmp_name_inc(const void *attr1, const void *attr2);
static int H5A__attr_cmp_name_dec(const void *attr1, const void *attr2);
static int H5A__attr_cmp_corder_inc(const void *attr1, const void *attr2);
static int H5A__attr_cmp_corder_dec(const void *attr1, const void *attr2);
static herr_t H5A__attr_sort_table(H5A_attr_table_t *atable, H5_index_t idx_type,
    H5_iter_order_t order);
static herr_t H5A__iterate_common(hid_t loc_id, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t *idx, H5A_attr_iter_op_t *attr_op, void *op_data);

/*********************/
/* Package Variables */
/*********************/

/* Format version bounds for attribute */
const unsigned H5O_attr_ver_bounds[] = {
    H5O_ATTR_VERSION_1,         /* H5F_LIBVER_EARLIEST */
    H5O_ATTR_VERSION_3,         /* H5F_LIBVER_V18 */
    H5O_ATTR_VERSION_LATEST     /* H5F_LIBVER_LATEST */
};

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

typedef H5A_t*	H5A_t_ptr;
H5FL_SEQ_DEFINE(H5A_t_ptr);


/*-------------------------------------------------------------------------
 * Function:	H5A__create
 *
 * Purpose:     This is the guts of creating an attribute.
 *
 * Return:      Attribute structure on success, NULL on Failure.
 *
 * Programmer:	Quincey Koziol
 *		April 2, 1998
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5A__create(const H5G_loc_t *loc, const char *name, const H5T_t *type,
    const H5S_t *space, hid_t acpl_id)
{
    H5A_t	*attr = NULL;           /* Attribute created */
    hssize_t	snelmts;	        /* elements in attribute */
    size_t	nelmts;		        /* elements in attribute */
    htri_t      exists;                 /* Whether attribute exists */
    H5A_t	*ret_value = NULL;      /* Return value */

    FUNC_ENTER_PACKAGE_TAG(loc->oloc->addr)

    /* Check args */
    HDassert(loc);
    HDassert(name);
    HDassert(type);
    HDassert(space);

    /* Check for existing attribute with same name */
    /* (technically, the "attribute create" operation will fail for a duplicated
     *  name, but it's going to be hard to unwind all the special cases on
     *  failure, so just check first, for now - QAK)
     */
    if((exists = H5O__attr_exists(loc->oloc, name)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, NULL, "error checking attributes")
    else if(exists > 0)
        HGOTO_ERROR(H5E_ATTR, H5E_ALREADYEXISTS, NULL, "attribute already exists")

    /* Check if the dataspace has an extent set (or is NULL) */
    if(!(H5S_has_extent(space)))
        HGOTO_ERROR(H5E_ATTR, H5E_BADVALUE, NULL, "dataspace extent has not been set")

    /* Check if the datatype is "sensible" for use in a dataset */
    if(H5T_is_sensible(type) != TRUE)
        HGOTO_ERROR(H5E_ATTR, H5E_BADTYPE, NULL, "datatype is not sensible")

    /* Build the attribute information */
    if(NULL == (attr = H5FL_CALLOC(H5A_t)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTALLOC, NULL, "memory allocation failed for attribute info")

    if(NULL == (attr->shared = H5FL_CALLOC(H5A_shared_t)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTALLOC, NULL, "can't allocate shared attr structure")

    /* If the creation property list is H5P_DEFAULT, use the default character encoding */
    if(acpl_id == H5P_DEFAULT)
        attr->shared->encoding = H5F_DEFAULT_CSET;
    else {
        H5P_genplist_t  *ac_plist;      /* ACPL Property list */

        /* Get a local copy of the attribute creation property list */
        if(NULL == (ac_plist = (H5P_genplist_t *)H5I_object(acpl_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, NULL, "not a property list")

        if(H5P_get(ac_plist, H5P_STRCRT_CHAR_ENCODING_NAME, &(attr->shared->encoding)) < 0)
            HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, NULL, "can't get character encoding flag")
    } /* end else */

    /* Copy the attribute name */
    attr->shared->name = H5MM_xstrdup(name);

    /* Copy datatype */
    if(NULL == (attr->shared->dt = H5T_copy(type, H5T_COPY_ALL)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, NULL, "can't get shared datatype info")

    /* Convert a datatype (if committed) to a transient type if the committed datatype's file
       location is different from the file location where the attribute will be created */
    if(H5T_convert_committed_datatype(attr->shared->dt, loc->oloc->file) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, NULL, "can't get shared datatype info")

    /* Mark datatype as being on disk now */
    if(H5T_set_loc(attr->shared->dt, loc->oloc->file, H5T_LOC_DISK) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "invalid datatype location")

    /* Set the version for datatype */
    if(H5T_set_version(loc->oloc->file, attr->shared->dt) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, NULL, "can't set version of datatype")

    /* Copy the dataspace for the attribute */
    attr->shared->ds = H5S_copy(space, FALSE, TRUE);

    /* Set the version for dataspace */
    if(H5S_set_version(loc->oloc->file, attr->shared->ds) < 0)
        HGOTO_ERROR(H5E_DATASET, H5E_CANTSET, NULL, "can't set version of dataspace")

    /* Copy the object header information */
    if(H5O_loc_copy(&(attr->oloc), loc->oloc, H5_COPY_DEEP) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "unable to copy entry")

    /* Deep copy of the group hierarchy path */
    if(H5G_name_copy(&(attr->path), loc->path, H5_COPY_DEEP) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, NULL, "unable to copy path")

    /* Check if any of the pieces should be (or are already) shared in the
     * SOHM table
     */
    if(H5SM_try_share(attr->oloc.file, NULL, 0, H5O_DTYPE_ID, attr->shared->dt, NULL) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, NULL, "trying to share datatype failed")
    if(H5SM_try_share(attr->oloc.file, NULL, 0, H5O_SDSPACE_ID, attr->shared->ds, NULL) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, NULL, "trying to share dataspace failed")

    /* Check whether datatype is committed & increment ref count
     * (to maintain ref. count incr/decr similarity with "shared message"
     *      type of datatype sharing)
     */
    if(H5T_committed(attr->shared->dt))
        /* Increment the reference count on the shared datatype */
        if(H5T_link(attr->shared->dt, 1) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_LINKCOUNT, NULL, "unable to adjust shared datatype link count")

    /* Compute the size of pieces on disk.  This is either the size of the
     * datatype and dataspace messages themselves, or the size of the "shared"
     * messages if either or both of them are shared.
     */
    attr->shared->dt_size = H5O_msg_raw_size(attr->oloc.file, H5O_DTYPE_ID, FALSE, attr->shared->dt);
    attr->shared->ds_size = H5O_msg_raw_size(attr->oloc.file, H5O_SDSPACE_ID, FALSE, attr->shared->ds);

    /* Get # of elements for attribute's dataspace */
    if((snelmts = H5S_GET_EXTENT_NPOINTS(attr->shared->ds)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOUNT, NULL, "dataspace is invalid")
    H5_CHECKED_ASSIGN(nelmts, size_t, snelmts, hssize_t);

    HDassert(attr->shared->dt_size > 0);
    HDassert(attr->shared->ds_size > 0);
    attr->shared->data_size = nelmts * H5T_GET_SIZE(attr->shared->dt);

    /* Hold the symbol table entry (and file) open */
    if(H5O_open(&(attr->oloc)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "unable to open")
    attr->obj_opened = TRUE;

    /* Set the version to encode the attribute with */
    if(H5A__set_version(attr->oloc.file, attr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTSET, NULL, "unable to update attribute version")

    /* Insert the attribute into the object header */
    if(H5O__attr_create(&(attr->oloc), attr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINSERT, NULL, "unable to create attribute in object header")

    /* Set return value */
    ret_value = attr;

done:
    /* Cleanup on failure */
    if(NULL == ret_value && attr && H5A__close(attr))
        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "can't close attribute")

    FUNC_LEAVE_NOAPI_TAG(ret_value)
} /* H5A__create() */


/*-------------------------------------------------------------------------
 * Function:	H5A__create_by_name
 *
 * Purpose:	Create an attribute on object, according to it's name
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		December 6, 2017
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5A__create_by_name(const H5G_loc_t *loc, const char *obj_name, const char *attr_name,
    const H5T_t *type, const H5S_t *space, hid_t acpl_id)
{
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    H5A_t       *attr = NULL;           /* Attribute from object header */
    H5A_t       *ret_value = NULL;      /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(loc);
    HDassert(obj_name);
    HDassert(attr_name);

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(loc, obj_name, &obj_loc/*out*/) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, NULL, "object not found")
    loc_found = TRUE;

    /* Go do the real work for attaching the attribute to the object */
    if(NULL == (attr = H5A__create(&obj_loc, attr_name, type, space, acpl_id)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "unable to create attribute")

    /* Set return value */
    ret_value = attr;

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, NULL, "can't free location")

    /* Cleanup on failure */
    if(ret_value == NULL)
        if(attr && H5A__close(attr) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "can't close attribute")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__create_by_name() */


/*-------------------------------------------------------------------------
 * Function:	H5A__open_common
 *
 * Purpose:
 *      Finishes initializing an attributes the open
 *
 * Usage:
 *  herr_t H5A__open_common(loc, name)
 *      const H5G_loc_t *loc;   IN: Pointer to group location for object
 *      H5A_t *attr;            IN/OUT: Pointer to attribute to initialize
 *
 * Return: Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		December 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A__open_common(const H5G_loc_t *loc, H5A_t *attr)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(loc);
    HDassert(attr);

#if defined(H5_USING_MEMCHECKER) || !defined(NDEBUG)
    /* Clear object location */
    if(H5O_loc_reset(&(attr->oloc)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to reset location")
#endif /* H5_USING_MEMCHECKER */

    /* Free any previous group hier. path */
    if(H5G_name_free(&(attr->path)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't release group hier. path")

    /* Deep copy of the symbol table entry */
    if(H5O_loc_copy(&(attr->oloc), loc->oloc, H5_COPY_DEEP) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to copy entry")

    /* Deep copy of the group hier. path */
    if(H5G_name_copy(&(attr->path), loc->path, H5_COPY_DEEP) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, FAIL, "unable to copy entry")

    /* Hold the symbol table entry (and file) open */
    if(H5O_open(&(attr->oloc)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open")
    attr->obj_opened = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__open_common() */


/*-------------------------------------------------------------------------
 * Function:	H5A__open
 *
 * Purpose:	Open an attribute in an object header
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		December 9, 2017
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5A__open(const H5G_loc_t *loc, const char *attr_name)
{
    H5A_t       *attr = NULL;           /* Attribute from object header */
    H5A_t       *ret_value = NULL;      /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(loc);
    HDassert(attr_name);

    /* Read in attribute from object header */
    if(NULL == (attr = H5O__attr_open_by_name(loc->oloc, attr_name)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "unable to load attribute info from object header for attribute: '%s'", attr_name)

    /* Finish initializing attribute */
    if(H5A__open_common(loc, attr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "unable to initialize attribute")

    /* Set return value */
    ret_value = attr;

done:
    /* Cleanup on failure */
    if(ret_value == NULL)
        if(attr && H5A__close(attr) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "can't close attribute")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__open() */


/*-------------------------------------------------------------------------
 * Function:	H5A__open_by_idx
 *
 * Purpose: 	Open an attribute according to its index order
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		April 2, 1998
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5A__open_by_idx(const H5G_loc_t *loc, const char *obj_name, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t n)
{
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    H5A_t       *attr = NULL;           /* Attribute from object header */
    H5A_t       *ret_value = NULL;      /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(loc);
    HDassert(obj_name);

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(loc, obj_name, &obj_loc/*out*/) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, NULL, "object not found")
    loc_found = TRUE;

    /* Read in attribute from object header */
    if(NULL == (attr = H5O__attr_open_by_idx(obj_loc.oloc, idx_type, order, n)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, NULL, "unable to load attribute info from object header")

    /* Finish initializing attribute */
    if(H5A__open_common(&obj_loc, attr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "unable to initialize attribute")

    /* Set return value */
    ret_value = attr;

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, NULL, "can't free location")

    /* Cleanup on failure */
    if(ret_value == NULL)
        if(attr && H5A__close(attr) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "can't close attribute")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__open_by_idx() */


/*-------------------------------------------------------------------------
 * Function:	H5A__open_by_name
 *
 * Purpose:	Open an attribute in an object header, according to it's name
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		December 11, 2006
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5A__open_by_name(const H5G_loc_t *loc, const char *obj_name, const char *attr_name)
{
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    H5A_t       *attr = NULL;           /* Attribute from object header */
    H5A_t       *ret_value = NULL;      /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(loc);
    HDassert(obj_name);
    HDassert(attr_name);

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(loc, obj_name, &obj_loc/*out*/) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, NULL, "object not found")
    loc_found = TRUE;

    /* Read in attribute from object header */
    if(NULL == (attr = H5O__attr_open_by_name(obj_loc.oloc, attr_name)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "unable to load attribute info from object header")

    /* Finish initializing attribute */
    if(H5A__open_common(loc, attr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "unable to initialize attribute")

    /* Set return value */
    ret_value = attr;

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, NULL, "can't free location")

    /* Cleanup on failure */
    if(ret_value == NULL)
        if(attr && H5A__close(attr) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "can't close attribute")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__open_by_name() */


/*--------------------------------------------------------------------------
 NAME
    H5A__read
 PURPOSE
    Actually read in data from an attribute
 USAGE
    herr_t H5A__read (attr, mem_type, buf)
        H5A_t *attr;         IN: Attribute to read
        const H5T_t *mem_type;     IN: Memory datatype of buffer
        void *buf;           IN: Buffer for data to read
 RETURNS
    Non-negative on success/Negative on failure

 DESCRIPTION
    This function reads a complete attribute from disk.
--------------------------------------------------------------------------*/
herr_t
H5A__read(const H5A_t *attr, const H5T_t *mem_type, void *buf)
{
    uint8_t		*tconv_buf = NULL;	/* datatype conv buffer*/
    uint8_t		*bkg_buf = NULL;	/* background buffer */
    hssize_t		snelmts;		/* elements in attribute */
    size_t		nelmts;			/* elements in attribute*/
    H5T_path_t		*tpath = NULL;		/* type conversion info	*/
    hid_t		src_id = -1, dst_id = -1;/* temporary type atoms*/
    size_t		src_type_size;		/* size of source type 	*/
    size_t		dst_type_size;		/* size of destination type */
    size_t		buf_size;		/* desired buffer size	*/
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_TAG(attr->oloc.addr)

    HDassert(attr);
    HDassert(mem_type);
    HDassert(buf);

    /* Create buffer for data to store on disk */
    if((snelmts = H5S_GET_EXTENT_NPOINTS(attr->shared->ds)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOUNT, FAIL, "dataspace is invalid")
    H5_CHECKED_ASSIGN(nelmts, size_t, snelmts, hssize_t);

    if(nelmts > 0) {
        /* Get the memory and file datatype sizes */
        src_type_size = H5T_GET_SIZE(attr->shared->dt);
        dst_type_size = H5T_GET_SIZE(mem_type);

        /* Check if the attribute has any data yet, if not, fill with zeroes */
        if(attr->obj_opened && !attr->shared->data)
            HDmemset(buf, 0, (dst_type_size * nelmts));
        else {  /* Attribute exists and has a value */
            /* Convert memory buffer into disk buffer */
            /* Set up type conversion function */
            if(NULL == (tpath = H5T_path_find(attr->shared->dt, mem_type)))
                HGOTO_ERROR(H5E_ATTR, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dst datatypes")

            /* Check for type conversion required */
            if(!H5T_path_noop(tpath)) {
                if((src_id = H5I_register(H5I_DATATYPE, H5T_copy(attr->shared->dt, H5T_COPY_ALL), FALSE)) < 0 ||
                        (dst_id = H5I_register(H5I_DATATYPE, H5T_copy(mem_type, H5T_COPY_ALL), FALSE)) < 0)
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTREGISTER, FAIL, "unable to register types for conversion")

                /* Get the maximum buffer size needed and allocate it */
                buf_size = nelmts * MAX(src_type_size, dst_type_size);
                if(NULL == (tconv_buf = H5FL_BLK_MALLOC(attr_buf, buf_size)))
                    HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "memory allocation failed")
                if(NULL == (bkg_buf = H5FL_BLK_CALLOC(attr_buf, buf_size)))
                    HGOTO_ERROR(H5E_ATTR, H5E_NOSPACE, FAIL, "memory allocation failed")

                /* Copy the attribute data into the buffer for conversion */
                HDmemcpy(tconv_buf, attr->shared->data, (src_type_size * nelmts));

                /* Perform datatype conversion.  */
                if(H5T_convert(tpath, src_id, dst_id, nelmts, (size_t)0, (size_t)0, tconv_buf, bkg_buf) < 0)
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "datatype conversion failed")

                /* Copy the converted data into the user's buffer */
                HDmemcpy(buf, tconv_buf, (dst_type_size * nelmts));
            } /* end if */
            /* No type conversion necessary */
            else {
                HDassert(dst_type_size == src_type_size);

                /* Copy the attribute data into the user's buffer */
                HDmemcpy(buf, attr->shared->data, (dst_type_size * nelmts));
            } /* end else */
        } /* end else */
    } /* end if */

done:
    /* Release resources */
    if(src_id >= 0 && H5I_dec_ref(src_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "unable to close temporary object")
    if(dst_id >= 0 && H5I_dec_ref(dst_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "unable to close temporary object")
    if(tconv_buf)
        tconv_buf = H5FL_BLK_FREE(attr_buf, tconv_buf);
    if(bkg_buf)
	bkg_buf = H5FL_BLK_FREE(attr_buf, bkg_buf);

    FUNC_LEAVE_NOAPI_TAG(ret_value)
} /* H5A__read() */


/*--------------------------------------------------------------------------
 NAME
    H5A__write
 PURPOSE
    Actually write out data to an attribute
 USAGE
    herr_t H5A__write (attr, mem_type, buf)
        H5A_t *attr;         IN: Attribute to write
        const H5T_t *mem_type;     IN: Memory datatype of buffer
        const void *buf;           IN: Buffer of data to write
 RETURNS
    Non-negative on success/Negative on failure

 DESCRIPTION
    This function writes a complete attribute to disk.
--------------------------------------------------------------------------*/
herr_t
H5A__write(H5A_t *attr, const H5T_t *mem_type, const void *buf)
{
    uint8_t		*tconv_buf = NULL;	/* datatype conv buffer */
    hbool_t             tconv_owned = FALSE;    /* Whether the datatype conv buffer is owned by attribute */
    uint8_t		*bkg_buf = NULL;	/* temp conversion buffer */
    hssize_t		snelmts;		/* elements in attribute */
    size_t		nelmts;		    	/* elements in attribute */
    H5T_path_t		*tpath = NULL;		/* conversion information*/
    hid_t		src_id = -1, dst_id = -1;/* temporary type atoms */
    size_t		src_type_size;		/* size of source type	*/
    size_t		dst_type_size;		/* size of destination type*/
    size_t		buf_size;		/* desired buffer size	*/
    herr_t		ret_value = SUCCEED;

    FUNC_ENTER_PACKAGE_TAG(attr->oloc.addr)

    HDassert(attr);
    HDassert(mem_type);
    HDassert(buf);

    /* Get # of elements for attribute's dataspace */
    if((snelmts = H5S_GET_EXTENT_NPOINTS(attr->shared->ds)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOUNT, FAIL, "dataspace is invalid")
    H5_CHECKED_ASSIGN(nelmts, size_t, snelmts, hssize_t);

    /* If there's actually data elements for the attribute, make a copy of the data passed in */
    if(nelmts > 0) {
        /* Get the memory and file datatype sizes */
        src_type_size = H5T_GET_SIZE(mem_type);
        dst_type_size = H5T_GET_SIZE(attr->shared->dt);

        /* Convert memory buffer into disk buffer */
        /* Set up type conversion function */
        if(NULL == (tpath = H5T_path_find(mem_type, attr->shared->dt)))
            HGOTO_ERROR(H5E_ATTR, H5E_UNSUPPORTED, FAIL, "unable to convert between src and dst datatypes")

        /* Check for type conversion required */
        if(!H5T_path_noop(tpath)) {
            if((src_id = H5I_register(H5I_DATATYPE, H5T_copy(mem_type, H5T_COPY_ALL), FALSE)) < 0 ||
                    (dst_id = H5I_register(H5I_DATATYPE, H5T_copy(attr->shared->dt, H5T_COPY_ALL), FALSE)) < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTREGISTER, FAIL, "unable to register types for conversion")

            /* Get the maximum buffer size needed and allocate it */
            buf_size = nelmts * MAX(src_type_size, dst_type_size);
            if(NULL == (tconv_buf = H5FL_BLK_MALLOC(attr_buf, buf_size)))
                HGOTO_ERROR(H5E_ATTR, H5E_CANTALLOC, FAIL, "memory allocation failed")
            if(NULL == (bkg_buf = H5FL_BLK_CALLOC(attr_buf, buf_size)))
                HGOTO_ERROR(H5E_ATTR, H5E_CANTALLOC, FAIL, "memory allocation failed")

            /* Copy the user's data into the buffer for conversion */
            HDmemcpy(tconv_buf, buf, (src_type_size * nelmts));

            /* Perform datatype conversion */
            if(H5T_convert(tpath, src_id, dst_id, nelmts, (size_t)0, (size_t)0, tconv_buf, bkg_buf) < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "datatype conversion failed")

            /* Free the previous attribute data buffer, if there is one */
            if(attr->shared->data)
                attr->shared->data = H5FL_BLK_FREE(attr_buf, attr->shared->data);

            /* Set the pointer to the attribute data to the converted information */
            attr->shared->data = tconv_buf;
            tconv_owned = TRUE;
        } /* end if */
        /* No type conversion necessary */
        else {
            HDassert(dst_type_size == src_type_size);

            /* Allocate the attribute buffer, if there isn't one */
            if(attr->shared->data == NULL)
                if(NULL == (attr->shared->data = H5FL_BLK_MALLOC(attr_buf, dst_type_size * nelmts)))
                    HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

            /* Copy the attribute data into the user's buffer */
            HDmemcpy(attr->shared->data, buf, (dst_type_size * nelmts));
        } /* end else */

        /* Modify the attribute in the object header */
        if(H5O__attr_write(&(attr->oloc), attr) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "unable to modify attribute")
    } /* end if */

done:
    /* Release resources */
    if(src_id >= 0 && H5I_dec_ref(src_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "unable to close temporary object")
    if(dst_id >= 0 && H5I_dec_ref(dst_id) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "unable to close temporary object")
    if(tconv_buf && !tconv_owned)
        tconv_buf = H5FL_BLK_FREE(attr_buf, tconv_buf);
    if(bkg_buf)
        bkg_buf = H5FL_BLK_FREE(attr_buf, bkg_buf);

    FUNC_LEAVE_NOAPI_TAG(ret_value)
} /* H5A__write() */


/*--------------------------------------------------------------------------
 NAME
    H5A__get_name
 PURPOSE
    Private function for H5Aget_name.  Gets a copy of the name for an
    attribute
 RETURNS
    This function returns the length of the attribute's name (which may be
    longer than 'buf_size') on success or negative for failure.
 DESCRIPTION
        This function retrieves the name of an attribute for an attribute ID.
    Up to 'buf_size' characters are stored in 'buf' followed by a '\0' string
    terminator.  If the name of the attribute is longer than 'buf_size'-1,
    the string terminator is stored in the last position of the buffer to
    properly terminate the string.
--------------------------------------------------------------------------*/
ssize_t
H5A__get_name(H5A_t *attr, size_t buf_size, char *buf)
{
    size_t              copy_len, nbytes;
    ssize_t		ret_value = -1;         /* Return value */

    FUNC_ENTER_PACKAGE_NOERR

    /* get the real attribute length */
    nbytes = HDstrlen(attr->shared->name);
    HDassert((ssize_t)nbytes >= 0); /*overflow, pretty unlikely --rpm*/

    /* compute the string length which will fit into the user's buffer */
    copy_len = MIN(buf_size - 1, nbytes);

    /* Copy all/some of the name */
    if(buf && copy_len > 0) {
        HDmemcpy(buf, attr->shared->name, copy_len);

        /* Terminate the string */
        buf[copy_len]='\0';
    } /* end if */

    /* Set return value */
    ret_value = (ssize_t)nbytes;

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__get_name() */


/*-------------------------------------------------------------------------
 * Function:    H5A_get_space
 *
 * Purpose:     Returns dataspace of the attribute.
 *
 * Return:      Success:    A valid ID for the dataspace of an attribute
 *
 *              Failure:    H5I_INVALID_ID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5A_get_space(H5A_t *attr)
{
    H5S_t      *ds = NULL;
    hid_t       ret_value = H5I_INVALID_HID;

    FUNC_ENTER_NOAPI_NOINIT

    HDassert(attr);

    /* Copy the attribute's dataspace */
    if (NULL == (ds = H5S_copy(attr->shared->ds, FALSE, TRUE)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, H5I_INVALID_HID, "unable to copy dataspace")

    /* Atomize */
    if ((ret_value = H5I_register(H5I_DATASPACE, ds, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register dataspace atom")

done:
    if (H5I_INVALID_HID == ret_value && ds && H5S_close(ds) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, H5I_INVALID_HID, "unable to release dataspace")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_get_space() */


/*-------------------------------------------------------------------------
 * Function:    H5A__get_type
 *
 * Purpose:     Returns an ID for the datatype of an attribute
 *
 * Return:      Success:    A valid ID for the datatype of an attribute
 *
 *              Failure:	H5I_INVALID_HID
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5A__get_type(H5A_t *attr)
{
    H5T_t *dt = NULL;
    hid_t ret_value = H5I_INVALID_HID;

    FUNC_ENTER_PACKAGE

    HDassert(attr);

    /* Patch the datatype's "top level" file pointer */
    if (H5T_patch_file(attr->shared->dt, attr->oloc.file) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, H5I_INVALID_HID, "unable to patch datatype's file pointer")

    /* Copy the attribute's datatype.  If the type is a named type then
     * reopen the type before returning it to the user. Make the type
     * read-only.
     */
    if (NULL == (dt = H5T_copy(attr->shared->dt, H5T_COPY_REOPEN)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, H5I_INVALID_HID, "unable to copy datatype")

    /* Mark any datatypes as being in memory now */
    if (H5T_set_loc(dt, NULL, H5T_LOC_MEMORY) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, H5I_INVALID_HID, "invalid datatype location")

    /* Lock copied type */
    if (H5T_lock(dt, FALSE) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, H5I_INVALID_HID, "unable to lock transient datatype")

    /* Atomize */
    if ((ret_value = H5I_register(H5I_DATATYPE, dt, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, H5I_INVALID_HID, "unable to register datatype")

done:
    if(H5I_INVALID_HID == ret_value && dt && (H5T_close(dt) < 0))
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, H5I_INVALID_HID, "unable to release datatype")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__get_type() */


/*--------------------------------------------------------------------------
 NAME
    H5A__get_create_plist
 PURPOSE
    private version of H5Aget_create_plist
 RETURNS
    This function returns the ID of a copy of the attribute's creation
    property list, or negative on failure.

 ERRORS

 DESCRIPTION
        This function returns a copy of the creation property list for
    an attribute.  The resulting ID must be closed with H5Pclose() or
    resource leaks will occur.
--------------------------------------------------------------------------*/
hid_t
H5A__get_create_plist(H5A_t* attr)
{
    H5P_genplist_t      *plist;              /* Default property list */
    hid_t               new_plist_id;        /* ID of ACPL to return */
    H5P_genplist_t      *new_plist;          /* ACPL to return */
    hid_t               ret_value = H5I_INVALID_HID;    /* Return value */

    FUNC_ENTER_PACKAGE

    if(NULL == (plist = (H5P_genplist_t *)H5I_object(H5P_LST_ATTRIBUTE_CREATE_ID_g)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "can't get default ACPL")

    /* Create the property list object to return */
    if((new_plist_id = H5P_copy_plist(plist, TRUE)) < 0)
	HGOTO_ERROR(H5E_PLIST, H5E_CANTINIT, FAIL, "unable to copy attribute creation properties")
    if(NULL == (new_plist = (H5P_genplist_t *)H5I_object(new_plist_id)))
        HGOTO_ERROR(H5E_PLIST, H5E_BADTYPE, FAIL, "can't get property list")

    /* Set the character encoding on the new property list */
    if(H5P_set(new_plist, H5P_STRCRT_CHAR_ENCODING_NAME, &(attr->shared->encoding)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTSET, FAIL, "can't set character encoding")

    ret_value = new_plist_id;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__get_create_plist() */


/*-------------------------------------------------------------------------
 * Function:	H5A__get_info
 *
 * Purpose:	Retrieve information about an attribute.
 *
 * Return:	Success:	Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              February  6, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__get_info(const H5A_t *attr, H5A_info_t *ainfo)
{
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check args */
    HDassert(attr);
    HDassert(ainfo);

    /* Set info for attribute */
    ainfo->cset = attr->shared->encoding;
    ainfo->data_size = attr->shared->data_size;
    if(attr->shared->crt_idx == H5O_MAX_CRT_ORDER_IDX) {
        ainfo->corder_valid = FALSE;
        ainfo->corder = 0;
    } /* end if */
    else {
        ainfo->corder_valid = TRUE;
        ainfo->corder = attr->shared->crt_idx;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__get_info() */


/*-------------------------------------------------------------------------
 * Function:	H5A__copy
 *
 * Purpose:	Copies attribute OLD_ATTR.
 *
 * Return:	Success:	Pointer to a new copy of the OLD_ATTR argument.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modification:Raymond Lu
 *              4 June 2008
 *              Changed some attribute information to be shared.
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5A__copy(H5A_t *_new_attr, const H5A_t *old_attr)
{
    H5A_t	*new_attr = NULL;
    hbool_t     allocated_attr = FALSE;   /* Whether the attribute was allocated */
    H5A_t	*ret_value = NULL;        /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(old_attr);

    /* Allocate attribute structure */
    if(_new_attr == NULL) {
        if(NULL == (new_attr = H5FL_CALLOC(H5A_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        allocated_attr = TRUE;
    } /* end if */
    else
        new_attr = _new_attr;

    /* Copy the top level of the attribute */
    new_attr->sh_loc = old_attr->sh_loc;

    /* Deep copy of the group hierarchy path */
    if(H5G_name_copy(&(new_attr->path), &(old_attr->path), H5_COPY_DEEP) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, NULL, "unable to copy path")

    /* Share some attribute information */
    new_attr->shared = old_attr->shared;

    /* Increment reference count for shared object */
    new_attr->shared->nrefs++;

    /* Don't open the object header for a copy */
    new_attr->obj_opened = FALSE;

    /* Set the return value */
    ret_value = new_attr;

done:
    if(ret_value == NULL)
        if(allocated_attr && new_attr && H5A__close(new_attr) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "can't close attribute")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__copy() */


/*-------------------------------------------------------------------------
 * Function:	H5A__free
 *
 * Purpose:	Frees all memory associated with an attribute, but does not
 *              free the H5A_t structure (which should be done in H5T_close).
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, November 15, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__free(H5A_t *attr)
{
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_PACKAGE

    HDassert(attr);

    /* Free dynamically allocated items */
    if(attr->shared->name) {
        H5MM_xfree(attr->shared->name);
        attr->shared->name = NULL;
    }
    if(attr->shared->dt) {
        if(H5T_close_real(attr->shared->dt) < 0)
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't release datatype info")
        attr->shared->dt = NULL;
    }
    if(attr->shared->ds) {
        if(H5S_close(attr->shared->ds) < 0)
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't release dataspace info")
        attr->shared->ds = NULL;
    }
    if(attr->shared->data)
        attr->shared->data = H5FL_BLK_FREE(attr_buf, attr->shared->data);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__free() */


/*-------------------------------------------------------------------------
 * Function:	H5A__close_cb
 *
 * Purpose:	Frees an attribute and all associated memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Sunday, February 18, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__close_cb(H5A_t *attr)
{
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(attr);
    HDassert(attr->shared);

    /* Call the actual close routine */
    if(H5A__close(attr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCLOSEOBJ, FAIL, "problem closing attribute")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__close_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5A__close
 *
 * Purpose:	Frees an attribute and all associated memory.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Monday, December  8, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__close(H5A_t *attr)
{
    herr_t ret_value = SUCCEED;           /* Return value */

    FUNC_ENTER_PACKAGE

    HDassert(attr);
    HDassert(attr->shared);

    /* Close the object's symbol-table entry */
    if(attr->obj_opened && (H5O_close(&(attr->oloc), NULL) < 0))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't release object header info")

    /* Reference count can be 0.  It only happens when H5A__create fails. */
    if(attr->shared->nrefs <= 1) {
        /* Free dynamically allocated items */
        if(H5A__free(attr) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't release attribute info")

        /* Destroy shared attribute struct */
        attr->shared = H5FL_FREE(H5A_shared_t, attr->shared);
    } /* end if */
    else {
        /* There are other references to the shared part of the attribute.
         * Only decrement the reference count. */
        --attr->shared->nrefs;
    } /* end else */

    /* Free group hierarchy path */
    if(H5G_name_free(&(attr->path)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't release group hier. path")

    attr->shared = NULL;
    attr = H5FL_FREE(H5A_t, attr);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__close() */


/*-------------------------------------------------------------------------
 * Function:	H5A_oloc
 *
 * Purpose:	Return the object location for an attribute.  It's the
 *		object location for the object to which the attribute
 *		belongs, not the attribute itself.
 *
 * Return:	Success:	Ptr to entry
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *              Thursday, August  6, 1998
 *
 *-------------------------------------------------------------------------
 */
H5O_loc_t *
H5A_oloc(H5A_t *attr)
{
    H5O_loc_t *ret_value = NULL;   /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    HDassert(attr);

    /* Set return value */
    ret_value = &(attr->oloc);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_oloc() */


/*-------------------------------------------------------------------------
 * Function:	H5A_nameof
 *
 * Purpose:	Return the group hier. path for an attribute.  It's the
 *		group hier. path for the object to which the attribute
 *		belongs, not the attribute itself.
 *
 * Return:	Success:	Ptr to entry
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 12, 2005
 *
 *-------------------------------------------------------------------------
 */
H5G_name_t *
H5A_nameof(H5A_t *attr)
{
    H5G_name_t *ret_value = NULL;   /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    HDassert(attr);

    /* Set return value */
    ret_value = &(attr->path);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_nameof() */


/*-------------------------------------------------------------------------
 * Function:    H5A_type
 *
 * Purpose:     Return the datatype for an attribute.
 *
 * Return:      Success:        Ptr to entry
 *              Failure:        NULL
 *
 * Programmer:  Neil Fortner
 *              Friday, November  11, 2011
 *
 *-------------------------------------------------------------------------
 */
H5T_t *
H5A_type(const H5A_t *attr)
{
    H5T_t *ret_value = NULL;   /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    HDassert(attr);

    /* Set return value */
    ret_value = attr->shared->dt;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A_type() */


/*-------------------------------------------------------------------------
 * Function:	H5A__exists_by_name
 *
 * Purpose:	Private version of H5Aexists_by_name
 *
 * Return:	Success:	TRUE/FALSE
 * 		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *              Thursday, November 1, 2007
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5A__exists_by_name(H5G_loc_t loc, const char *obj_name, const char *attr_name)
{
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    htri_t	ret_value = FAIL;       /* Return value */

    FUNC_ENTER_PACKAGE

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(&loc, obj_name, &obj_loc/*out*/) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "object not found")
    loc_found = TRUE;

    /* Check if the attribute exists */
    if((ret_value = H5O__attr_exists(obj_loc.oloc, attr_name)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "unable to determine if attribute exists")

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__exists_by_name() */


/*-------------------------------------------------------------------------
 * Function:	H5A__compact_build_table_cb
 *
 * Purpose:	Object header iterator callback routine to copy attribute
 *              into table.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec 18 2006
 *
 * Modification:Raymond Lu
 *              24 June 2008
 *              Changed the table of attribute objects to be the table of
 *              pointers to attribute objects for the ease of operation.
 *-------------------------------------------------------------------------
 */
static herr_t
H5A__compact_build_table_cb(H5O_t H5_ATTR_UNUSED *oh, H5O_mesg_t *mesg/*in,out*/,
    unsigned sequence, unsigned H5_ATTR_UNUSED *oh_modified, void *_udata/*in,out*/)
{
    H5A_compact_bt_ud_t *udata = (H5A_compact_bt_ud_t *)_udata;   /* Operator user data */
    herr_t ret_value = H5_ITER_CONT;    /* Return value */

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(mesg);

    /* Re-allocate the table if necessary */
    if(udata->curr_attr == udata->atable->nattrs) {
        H5A_t **new_table;          /* New table for attributes */
        size_t new_table_size;      /* Number of attributes in new table */

        /* Allocate larger table */
        new_table_size = MAX(1, 2 * udata->atable->nattrs);
        if(NULL == (new_table = (H5A_t **)H5FL_SEQ_REALLOC(H5A_t_ptr, udata->atable->attrs, new_table_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, H5_ITER_ERROR, "unable to extend attribute table")

        /* Update table information in user data */
        udata->atable->attrs = new_table;
        udata->atable->nattrs = new_table_size;
    } /* end if */

    /* Copy attribute into table */
    if(NULL == (udata->atable->attrs[udata->curr_attr] = H5A__copy(NULL, (const H5A_t *)mesg->native)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, H5_ITER_ERROR, "can't copy attribute")

    /* Assign [somewhat arbitrary] creation order value, if requested */
    if(udata->bogus_crt_idx)
        ((udata->atable->attrs[udata->curr_attr])->shared)->crt_idx = sequence;

    /* Increment current attribute */
    udata->curr_attr++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__compact_build_table_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5A__compact_build_table
 *
 * Purpose:     Builds a table containing a sorted list of attributes for
 *              an object
 *
 * Note:	Used for building table of attributes in non-native iteration
 *              order for an index
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Dec 18, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__compact_build_table(H5F_t *f, H5O_t *oh, H5_index_t idx_type,
    H5_iter_order_t order, H5A_attr_table_t *atable)
{
    H5A_compact_bt_ud_t udata;                  /* User data for iteration callback */
    H5O_mesg_operator_t op;             /* Wrapper for operator */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(f);
    HDassert(oh);
    HDassert(atable);

    /* Initialize table */
    atable->attrs = NULL;
    atable->nattrs = 0;

    /* Set up user data for iteration */
    udata.f = f;
    udata.atable = atable;
    udata.curr_attr = 0;
    udata.bogus_crt_idx = (hbool_t)((oh->version == H5O_VERSION_1 ||
            !(oh->flags & H5O_HDR_ATTR_CRT_ORDER_TRACKED)) ? TRUE : FALSE);

    /* Iterate over existing attributes, checking for attribute with same name */
    op.op_type = H5O_MESG_OP_LIB;
    op.u.lib_op = H5A__compact_build_table_cb;
    if(H5O__msg_iterate_real(f, oh, H5O_MSG_ATTR, &op, &udata) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_BADITER, FAIL, "error building attribute table")

    /* Correct # of attributes in table */
    atable->nattrs = udata.curr_attr;

    /* Don't sort an empty table. */
    if(atable->nattrs > 0) {
        /* Sort attribute table in correct iteration order */
        if(H5A__attr_sort_table(atable, idx_type, order) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTSORT, FAIL, "error sorting attribute table")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__compact_build_table() */


/*-------------------------------------------------------------------------
 * Function:	H5A__dense_build_table_cb
 *
 * Purpose:	Callback routine for building table of attributes from dense
 *              attribute storage.
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec 11 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A__dense_build_table_cb(const H5A_t *attr, void *_udata)
{
    H5A_dense_bt_ud_t *udata = (H5A_dense_bt_ud_t *)_udata;     /* 'User data' passed in */
    herr_t ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_STATIC

    /* check arguments */
    HDassert(attr);
    HDassert(udata);
    HDassert(udata->curr_attr < udata->atable->nattrs);

    /* Allocate attribute for entry in the table */
    if(NULL == (udata->atable->attrs[udata->curr_attr] = H5FL_CALLOC(H5A_t)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTALLOC, H5_ITER_ERROR, "can't allocate attribute")

    /* Copy attribute information.  Share the attribute object in copying. */
    if(NULL == H5A__copy(udata->atable->attrs[udata->curr_attr], attr))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, H5_ITER_ERROR, "can't copy attribute")

    /* Increment number of attributes stored */
    udata->curr_attr++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__dense_build_table_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5A__dense_build_table
 *
 * Purpose:     Builds a table containing a sorted list of attributes for
 *              an object
 *
 * Note:	Used for building table of attributes in non-native iteration
 *              order for an index.  Uses the "name" index to retrieve records,
 *		but the 'idx_type' index for sorting them.
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Dec 11, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__dense_build_table(H5F_t *f, const H5O_ainfo_t *ainfo,
    H5_index_t idx_type, H5_iter_order_t order, H5A_attr_table_t *atable)
{
    H5B2_t *bt2_name = NULL;            /* v2 B-tree handle for name index */
    hsize_t nrec;                       /* # of records in v2 B-tree */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(f);
    HDassert(ainfo);
    HDassert(H5F_addr_defined(ainfo->fheap_addr));
    HDassert(H5F_addr_defined(ainfo->name_bt2_addr));
    HDassert(atable);

    /* Open the name index v2 B-tree */
    if(NULL == (bt2_name = H5B2_open(f, ainfo->name_bt2_addr, NULL)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open v2 B-tree for name index")

    /* Retrieve # of records in "name" B-tree */
    /* (should be same # of records in all indices) */
    if(H5B2_get_nrec(bt2_name, &nrec) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't retrieve # of records in index")

    /* Set size of table */
    H5_CHECK_OVERFLOW(nrec, /* From: */ hsize_t, /* To: */ size_t);
    atable->nattrs = (size_t)nrec;

    /* Allocate space for the table entries */
    if(atable->nattrs > 0) {
        H5A_dense_bt_ud_t udata;       /* User data for iteration callback */
        H5A_attr_iter_op_t attr_op;    /* Attribute operator */

        /* Allocate the table to store the attributes */
        if((atable->attrs = (H5A_t **)H5FL_SEQ_CALLOC(H5A_t_ptr, atable->nattrs)) == NULL)
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

        /* Set up user data for iteration */
        udata.atable = atable;
        udata.curr_attr = 0;

        /* Build iterator operator */
        attr_op.op_type = H5A_ATTR_OP_LIB;
        attr_op.u.lib_op = H5A__dense_build_table_cb;

        /* Iterate over the links in the group, building a table of the link messages */
        if(H5A__dense_iterate(f, (hid_t)0, ainfo, H5_INDEX_NAME, H5_ITER_NATIVE,
                (hsize_t)0, NULL, &attr_op, &udata) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "error building attribute table")

        /* Sort attribute table in correct iteration order */
        if(H5A__attr_sort_table(atable, idx_type, order) < 0)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTSORT, FAIL, "error sorting attribute table")
    } /* end if */
    else
        atable->attrs = NULL;

done:
    /* Release resources */
    if(bt2_name && H5B2_close(bt2_name) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close v2 B-tree for name index")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__dense_build_table() */


/*-------------------------------------------------------------------------
 * Function:	H5A__attr_cmp_name_inc
 *
 * Purpose:	Callback routine for comparing two attribute names, in
 *              increasing alphabetic order
 *
 * Return:	An integer less than, equal to, or greater than zero if the
 *              first argument is considered to be respectively less than,
 *              equal to, or greater than the second.  If two members compare
 *              as equal, their order in the sorted array is undefined.
 *              (i.e. same as strcmp())
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Dec 11 2006
 *
 *-------------------------------------------------------------------------
 */
static int
H5A__attr_cmp_name_inc(const void *attr1, const void *attr2)
{
    FUNC_ENTER_STATIC_NOERR

    FUNC_LEAVE_NOAPI(HDstrcmp((*(const H5A_t * const *)attr1)->shared->name,
            (*(const H5A_t * const *)attr2)->shared->name))
} /* end H5A__attr_cmp_name_inc() */


/*-------------------------------------------------------------------------
 * Function:	H5A__attr_cmp_name_dec
 *
 * Purpose:	Callback routine for comparing two attribute names, in
 *              decreasing alphabetic order
 *
 * Return:	An integer less than, equal to, or greater than zero if the
 *              second argument is considered to be respectively less than,
 *              equal to, or greater than the first.  If two members compare
 *              as equal, their order in the sorted array is undefined.
 *              (i.e. opposite of strcmp())
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Feb  8 2007
 *
 *-------------------------------------------------------------------------
 */
static int
H5A__attr_cmp_name_dec(const void *attr1, const void *attr2)
{
    FUNC_ENTER_STATIC_NOERR

    FUNC_LEAVE_NOAPI(HDstrcmp((*(const H5A_t * const *)attr2)->shared->name,
            (*(const H5A_t * const *)attr1)->shared->name))
} /* end H5A__attr_cmp_name_dec() */


/*-------------------------------------------------------------------------
 * Function:	H5A__attr_cmp_corder_inc
 *
 * Purpose:	Callback routine for comparing two attributes, in
 *              increasing creation order
 *
 * Return:	An integer less than, equal to, or greater than zero if the
 *              first argument is considered to be respectively less than,
 *              equal to, or greater than the second.  If two members compare
 *              as equal, their order in the sorted array is undefined.
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Feb  8 2007
 *
 *-------------------------------------------------------------------------
 */
static int
H5A__attr_cmp_corder_inc(const void *attr1, const void *attr2)
{
    int ret_value = 0;          /* Return value */

    FUNC_ENTER_STATIC_NOERR

    if((*(const H5A_t * const *)attr1)->shared->crt_idx < (*(const H5A_t * const *)attr2)->shared->crt_idx)
        ret_value = -1;
    else if((*(const H5A_t * const *)attr1)->shared->crt_idx > (*(const H5A_t * const *)attr2)->shared->crt_idx)
        ret_value = 1;
    else
        ret_value = 0;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__attr_cmp_corder_inc() */


/*-------------------------------------------------------------------------
 * Function:	H5A__attr_cmp_corder_dec
 *
 * Purpose:	Callback routine for comparing two attributes, in
 *              decreasing creation order
 *
 * Return:	An integer less than, equal to, or greater than zero if the
 *              second argument is considered to be respectively less than,
 *              equal to, or greater than the first.  If two members compare
 *              as equal, their order in the sorted array is undefined.
 *
 * Programmer:	Quincey Koziol
 *		koziol@hdfgroup.org
 *		Feb  8 2007
 *
 *-------------------------------------------------------------------------
 */
static int
H5A__attr_cmp_corder_dec(const void *attr1, const void *attr2)
{
    int ret_value = 0;              /* Return value */

    FUNC_ENTER_STATIC_NOERR

    if((*(const H5A_t * const *)attr1)->shared->crt_idx < (*(const H5A_t * const *)attr2)->shared->crt_idx)
        ret_value = 1;
    else if((*(const H5A_t * const *)attr1)->shared->crt_idx > (*(const H5A_t * const *)attr2)->shared->crt_idx)
        ret_value = -1;
    else
        ret_value = 0;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__attr_cmp_corder_dec() */


/*-------------------------------------------------------------------------
 * Function:	H5A__attr_sort_table
 *
 * Purpose:     Sort table containing a list of attributes for an object
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Dec 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A__attr_sort_table(H5A_attr_table_t *atable, H5_index_t idx_type,
    H5_iter_order_t order)
{
    FUNC_ENTER_STATIC_NOERR

    /* Sanity check */
    HDassert(atable);

    /* Pick appropriate comparison routine */
    if(idx_type == H5_INDEX_NAME) {
        if(order == H5_ITER_INC)
            HDqsort(atable->attrs, atable->nattrs, sizeof(H5A_t*), H5A__attr_cmp_name_inc);
        else if(order == H5_ITER_DEC)
            HDqsort(atable->attrs, atable->nattrs, sizeof(H5A_t*), H5A__attr_cmp_name_dec);
        else
            HDassert(order == H5_ITER_NATIVE);
    } /* end if */
    else {
        HDassert(idx_type == H5_INDEX_CRT_ORDER);
        if(order == H5_ITER_INC)
            HDqsort(atable->attrs, atable->nattrs, sizeof(H5A_t*), H5A__attr_cmp_corder_inc);
        else if(order == H5_ITER_DEC)
            HDqsort(atable->attrs, atable->nattrs, sizeof(H5A_t*), H5A__attr_cmp_corder_dec);
        else
            HDassert(order == H5_ITER_NATIVE);
    } /* end else */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5A__attr_sort_table() */


/*-------------------------------------------------------------------------
 * Function:	H5A__attr_iterate_table
 *
 * Purpose:     Iterate over table containing a list of attributes for an object,
 *              making appropriate callbacks
 *
 * Return:	Success:        Non-negative
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *	        Dec 18, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__attr_iterate_table(const H5A_attr_table_t *atable, hsize_t skip,
    hsize_t *last_attr, hid_t loc_id, const H5A_attr_iter_op_t *attr_op,
    void *op_data)
{
    size_t u;                           /* Local index variable */
    herr_t ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(atable);
    HDassert(attr_op);

    /* Skip over attributes, if requested */
    if(last_attr)
        *last_attr = skip;

    /* Iterate over attribute messages */
    H5_CHECKED_ASSIGN(u, size_t, skip, hsize_t)
    for(; u < atable->nattrs && !ret_value; u++) {
        /* Check which type of callback to make */
        switch(attr_op->op_type) {
            case H5A_ATTR_OP_APP2:
            {
                H5A_info_t ainfo;               /* Info for attribute */

                /* Get the attribute information */
                if(H5A__get_info(atable->attrs[u], &ainfo) < 0)
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, H5_ITER_ERROR, "unable to get attribute info")

                /* Make the application callback */
                ret_value = (attr_op->u.app_op2)(loc_id, ((atable->attrs[u])->shared)->name, &ainfo, op_data);
                break;
            }

#ifndef H5_NO_DEPRECATED_SYMBOLS
            case H5A_ATTR_OP_APP:
                /* Make the application callback */
                ret_value = (attr_op->u.app_op)(loc_id, ((atable->attrs[u])->shared)->name, op_data);
                break;
#endif /* H5_NO_DEPRECATED_SYMBOLS */

            case H5A_ATTR_OP_LIB:
                /* Call the library's callback */
                ret_value = (attr_op->u.lib_op)((atable->attrs[u]), op_data);
                break;

            default:
                HDassert("unknown attribute op type" && 0);
#ifdef NDEBUG
                HGOTO_ERROR(H5E_ATTR, H5E_UNSUPPORTED, FAIL, "unsupported attribute op type")
#endif /* NDEBUG */
        } /* end switch */

        /* Increment the number of entries passed through */
        if(last_attr)
            (*last_attr)++;
    } /* end for */

    /* Check for callback failure and pass along return value */
    if(ret_value < 0)
        HERROR(H5E_ATTR, H5E_CANTNEXT, "iteration operator failed");

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__attr_iterate_table() */


/*-------------------------------------------------------------------------
 * Function:	  H5A__attr_release_table
 *
 * Purpose:       Release table containing a list of attributes for an object
 *
 * Return:	  Success:        Non-negative
 *		  Failure:	Negative
 *
 * Programmer:	  Quincey Koziol
 *	          Dec 11, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__attr_release_table(H5A_attr_table_t *atable)
{
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_PACKAGE

    /* Sanity check */
    HDassert(atable);

    /* Release attribute info, if any. */
    if(atable->nattrs > 0) {
        size_t      u;               /* Local index variable */

        /* Free attribute message information */
        for(u = 0; u < atable->nattrs; u++)
            if(atable->attrs[u] && H5A__close(atable->attrs[u]) < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTFREE, FAIL, "unable to release attribute")
    } /* end if */
    else
        HDassert(atable->attrs == NULL);

    atable->attrs = (H5A_t **)H5FL_SEQ_FREE(H5A_t_ptr, atable->attrs);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__attr_release_table() */


/*-------------------------------------------------------------------------
 * Function:    H5A__get_ainfo
 *
 * Purpose:     Retrieves the "attribute info" message for an object.  Also
 *              sets the number of attributes correctly, if it isn't set up yet.
 *
 * Return:	Success:	TRUE/FALSE whether message was found & retrieved
 *              Failure:        FAIL if error occurred
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Mar 11 2007
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5A__get_ainfo(H5F_t *f, H5O_t *oh, H5O_ainfo_t *ainfo)
{
    H5B2_t *bt2_name = NULL;    /* v2 B-tree handle for name index */
    htri_t ret_value = FAIL;    /* Return value */

    FUNC_ENTER_NOAPI_TAG(oh->cache_info.addr, FAIL)

    /* check arguments */
    HDassert(f);
    HDassert(oh);
    HDassert(ainfo);

    /* Check if the "attribute info" message exists */
    if((ret_value = H5O_msg_exists_oh(oh, H5O_AINFO_ID)) < 0)
	HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "unable to check object header")
    if(ret_value > 0) {
        /* Retrieve the "attribute info" structure */
        if(NULL == H5O_msg_read_oh(f, oh, H5O_AINFO_ID, ainfo))
	    HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't read AINFO message")

        /* Check if we don't know how many attributes there are */
        if(ainfo->nattrs == HSIZET_MAX) {
            /* Check if we are using "dense" attribute storage */
            if(H5F_addr_defined(ainfo->fheap_addr)) {
                /* Open the name index v2 B-tree */
                if(NULL == (bt2_name = H5B2_open(f, ainfo->name_bt2_addr, NULL)))
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open v2 B-tree for name index")

                /* Retrieve # of records in "name" B-tree */
                /* (should be same # of records in all indices) */
                if(H5B2_get_nrec(bt2_name, &ainfo->nattrs) < 0)
                    HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, FAIL, "can't retrieve # of records in index")
            } /* end if */
            else
                /* Retrieve # of attributes from object header */
                ainfo->nattrs = oh->attr_msgs_seen;
        } /* end if */
    } /* end if */

done:
    /* Release resources */
    if(bt2_name && H5B2_close(bt2_name) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close v2 B-tree for name index")

    FUNC_LEAVE_NOAPI_TAG(ret_value)
} /* end H5A__get_ainfo() */


/*-------------------------------------------------------------------------
 * Function:    H5A__set_version
 *
 * Purpose:     Sets the correct version to encode attribute with.
 *              Chooses the oldest version possible, unless the
 *              file's low bound indicates otherwise.
 *
 * Return:	Success:    Non-negative
 *          Failure:    Negative
 *
 * Programmer:  Quincey Koziol
 *              koziol@hdfgroup.org
 *              Jul 17 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__set_version(const H5F_t *f, H5A_t *attr)
{
    hbool_t type_shared, space_shared;  /* Flags to indicate that shared messages are used for this attribute */
    uint8_t version;                    /* Message version */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* check arguments */
    HDassert(f);
    HDassert(attr);

    /* Check whether datatype and dataspace are shared */
    if(H5O_msg_is_shared(H5O_DTYPE_ID, attr->shared->dt) > 0)
        type_shared = TRUE;
    else
        type_shared = FALSE;

    if(H5O_msg_is_shared(H5O_SDSPACE_ID, attr->shared->ds) > 0)
        space_shared = TRUE;
    else
        space_shared = FALSE;

    /* Check which version to encode attribute with */
    if(attr->shared->encoding != H5T_CSET_ASCII)
        version = H5O_ATTR_VERSION_3;   /* Write version which includes the character encoding */
    else if(type_shared || space_shared)
        version = H5O_ATTR_VERSION_2;   /* Write out version with flag for indicating shared datatype or dataspace */
    else
        version = H5O_ATTR_VERSION_1;   /* Write out basic version */

    /* Upgrade to the version indicated by the file's low bound if higher */
    version = MAX(version, (uint8_t)H5O_attr_ver_bounds[H5F_LOW_BOUND(f)]);

    /* Version bounds check */
    if(version > H5O_attr_ver_bounds[H5F_HIGH_BOUND(f)])
        HGOTO_ERROR(H5E_ATTR, H5E_BADRANGE, FAIL, "attribute version out of bounds")

    /* Set the message version */
    attr->shared->version = version;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__set_version() */


/*-------------------------------------------------------------------------
 * Function:    H5A__attr_copy_file
 *
 * Purpose:     Copies a message from _MESG to _DEST in file
 *
 *              Note that this function assumes that it is copying *all*
 *              the attributes in the object, specifically when it copies
 *              the creation order from source to destination.  If this is
 *              to be used to copy only a single attribute, then the
 *              creation order must be handled differently.  -NAF
 *
 * Return:      Success:        Ptr to _DEST
 *
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              November 1, 2005
 *
 *-------------------------------------------------------------------------
 */
H5A_t *
H5A__attr_copy_file(const H5A_t *attr_src, H5F_t *file_dst, hbool_t *recompute_size,
    H5O_copy_t *cpy_info)
{
    H5A_t      *attr_dst = NULL;        /* Destination attribute */
    hid_t       tid_src = -1;           /* Datatype ID for source datatype */
    hid_t       tid_dst = -1;           /* Datatype ID for destination datatype */
    hid_t       tid_mem = -1;           /* Datatype ID for memory datatype */
    void       *buf = NULL;             /* Buffer for copying data */
    void       *reclaim_buf = NULL;     /* Buffer for reclaiming data */
    void       *bkg_buf = NULL;     	/* Background buffer */
    hid_t       buf_sid = -1;           /* ID for buffer dataspace */
    hssize_t	sdst_nelmts;	        /* # of elements in destination attribute (signed) */
    size_t	dst_nelmts;		/* # of elements in destination attribute */
    size_t	dst_dt_size;		/* Size of destination attribute datatype */
    H5A_t      *ret_value = NULL;       /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(attr_src);
    HDassert(file_dst);
    HDassert(cpy_info);
    HDassert(!cpy_info->copy_without_attr);

    /* Allocate space for the destination message */
    if(NULL == (attr_dst = H5FL_CALLOC(H5A_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy the top level of the attribute */
    *attr_dst = *attr_src;

    if(NULL == (attr_dst->shared = H5FL_CALLOC(H5A_shared_t)))
        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate shared attr structure")

    /* Don't have an opened group location for copy */
    H5O_loc_reset(&(attr_dst->oloc));
    H5G_name_reset(&(attr_dst->path));
    attr_dst->obj_opened = FALSE;

    /* Reference count for the header message in the cache */
    attr_dst->shared->nrefs = 1;

    /* Copy attribute's name */
    attr_dst->shared->name = H5MM_strdup(attr_src->shared->name);
    HDassert(attr_dst->shared->name);
    attr_dst->shared->encoding = attr_src->shared->encoding;

    /* Copy attribute's datatype */
    /* If source is named, we will keep dst as named, but we will not actually
     * copy the target and update the message until post copy */
    if(NULL == (attr_dst->shared->dt = H5T_copy(attr_src->shared->dt, H5T_COPY_ALL)))
        HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, NULL, "cannot copy datatype")

    /* Set the location of the destination datatype */
    if(H5T_set_loc(attr_dst->shared->dt, file_dst, H5T_LOC_DISK) < 0)
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "cannot mark datatype on disk")

    if(!H5T_committed(attr_src->shared->dt)) {
        /* If the datatype is not named, it may have been shared in the
         * source file's heap.  Un-share it for now. We'll try to shared
         * it in the destination file below.
         */
        if(H5O_msg_reset_share(H5O_DTYPE_ID, attr_dst->shared->dt) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to reset datatype sharing")
    } /* end if */

    /* Copy the dataspace for the attribute. Make sure the maximal dimension is also copied.
     * Otherwise the comparison in the test may complain about it. SLU 2011/4/12 */
    attr_dst->shared->ds = H5S_copy(attr_src->shared->ds, FALSE, TRUE);
    HDassert(attr_dst->shared->ds);

    /* Reset the dataspace's sharing in the source file before trying to share
     * it in the destination.
     */
    if(H5O_msg_reset_share(H5O_SDSPACE_ID, attr_dst->shared->ds) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, NULL, "unable to reset dataspace sharing")

    /* Simulate trying to share both the datatype and dataset, to determine the
     * final size of the messages.  This does nothing if the datatype is
     * committed or sharing is disabled.
     */
    if(H5SM_try_share(file_dst, NULL, H5SM_DEFER, H5O_DTYPE_ID, attr_dst->shared->dt, NULL) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, NULL, "can't share attribute datatype")
    if(H5SM_try_share(file_dst, NULL, H5SM_DEFER, H5O_SDSPACE_ID, attr_dst->shared->ds, NULL) < 0)
	HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, NULL, "can't share attribute dataspace")

    /* Compute the sizes of the datatype and dataspace. This is their raw
     * size unless they're shared.
     */
    attr_dst->shared->dt_size = H5O_msg_raw_size(file_dst, H5O_DTYPE_ID, FALSE, attr_dst->shared->dt);
    HDassert(attr_dst->shared->dt_size > 0);
    attr_dst->shared->ds_size = H5O_msg_raw_size(file_dst, H5O_SDSPACE_ID, FALSE, attr_dst->shared->ds);
    HDassert(attr_dst->shared->ds_size > 0);

    /* Check whether to recompute the size of the attribute */
    /* (happens when the datatype or dataspace changes sharing status) */
    if(attr_dst->shared->dt_size != attr_src->shared->dt_size || attr_dst->shared->ds_size != attr_src->shared->ds_size)
        *recompute_size = TRUE;

    /* Get # of elements for destination attribute's dataspace */
    if((sdst_nelmts = H5S_GET_EXTENT_NPOINTS(attr_dst->shared->ds)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOUNT, NULL, "dataspace is invalid")
    H5_CHECKED_ASSIGN(dst_nelmts, size_t, sdst_nelmts, hssize_t);

    /* Get size of destination attribute's datatype */
    if(0 == (dst_dt_size = H5T_get_size(attr_dst->shared->dt)))
        HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to determine datatype size")

    /* Compute the size of the data */
    attr_dst->shared->data_size =  dst_nelmts * dst_dt_size;

    /* Copy (& convert) the data, if necessary */
    if(attr_src->shared->data) {
        if(NULL == (attr_dst->shared->data = H5FL_BLK_MALLOC(attr_buf, attr_dst->shared->data_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

        /* Check if we need to convert data */
        if(H5T_detect_class(attr_src->shared->dt, H5T_VLEN, FALSE) > 0) {
            H5T_path_t  *tpath_src_mem, *tpath_mem_dst;   /* Datatype conversion paths */
            H5T_t *dt_mem;              /* Memory datatype */
            size_t src_dt_size;         /* Source datatype size */
            size_t tmp_dt_size;         /* Temp. datatype size */
            size_t max_dt_size;         /* Max atatype size */
            H5S_t *buf_space;           /* Dataspace describing buffer */
            hsize_t buf_dim;            /* Dimension for buffer */
            size_t nelmts;              /* Number of elements in buffer */
            size_t buf_size;            /* Size of copy buffer */

            /* Create datatype ID for src datatype */
            if((tid_src = H5I_register(H5I_DATATYPE, attr_src->shared->dt, FALSE)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL, "unable to register source file datatype")

            /* create a memory copy of the variable-length datatype */
            if(NULL == (dt_mem = H5T_copy(attr_src->shared->dt, H5T_COPY_TRANSIENT)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to copy")
            if((tid_mem = H5I_register(H5I_DATATYPE, dt_mem, FALSE)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL, "unable to register memory datatype")

            /* create variable-length datatype at the destinaton file */
            if((tid_dst = H5I_register(H5I_DATATYPE, attr_dst->shared->dt, FALSE)) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTREGISTER, NULL, "unable to register destination file datatype")

            /* Set up the conversion functions */
            if(NULL == (tpath_src_mem = H5T_path_find(attr_src->shared->dt, dt_mem)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to convert between src and mem datatypes")
            if(NULL == (tpath_mem_dst = H5T_path_find(dt_mem, attr_dst->shared->dt)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to convert between mem and dst datatypes")

            /* Determine largest datatype size */
            if(0 == (src_dt_size = H5T_get_size(attr_src->shared->dt)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to determine datatype size")
            if(0 == (tmp_dt_size = H5T_get_size(dt_mem)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to determine datatype size")
            max_dt_size = MAX(src_dt_size, tmp_dt_size);
            if(0 == (tmp_dt_size = H5T_get_size(attr_dst->shared->dt)))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "unable to determine datatype size")
            max_dt_size = MAX(max_dt_size, tmp_dt_size);

            /* Set number of whole elements that fit in buffer */
            if(0 == (nelmts = attr_src->shared->data_size / src_dt_size))
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "element size too large")

            /* Set up number of bytes to copy, and initial buffer size */
            buf_size = nelmts * max_dt_size;

            /* Create dataspace for number of elements in buffer */
            buf_dim = nelmts;

            /* Create the space and set the initial extent */
            if(NULL == (buf_space = H5S_create_simple((unsigned)1, &buf_dim, NULL)))
                HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, NULL, "can't create simple dataspace")

            /* Atomize */
            if((buf_sid = H5I_register(H5I_DATASPACE, buf_space, FALSE)) < 0) {
                H5S_close(buf_space);
                HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, NULL, "unable to register dataspace ID")
            } /* end if */

            /* Allocate memory for recclaim buf */
            if(NULL == (reclaim_buf = H5FL_BLK_MALLOC(attr_buf, buf_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation NULLed for raw data chunk")

            /* Allocate memory for copying the chunk */
            if(NULL == (buf = H5FL_BLK_MALLOC(attr_buf, buf_size)))
                HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation NULLed for raw data chunk")

            HDmemcpy(buf, attr_src->shared->data, attr_src->shared->data_size);

	    /* Allocate background memory */
	    if(H5T_path_bkg(tpath_src_mem) || H5T_path_bkg(tpath_mem_dst))
		if(NULL == (bkg_buf = H5FL_BLK_CALLOC(attr_buf, buf_size)))
		    HGOTO_ERROR(H5E_ATTR, H5E_CANTALLOC, NULL, "memory allocation failed")

            /* Convert from source file to memory */
            if(H5T_convert(tpath_src_mem, tid_src, tid_mem, nelmts, (size_t)0, (size_t)0, buf, bkg_buf) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "datatype conversion NULLed")

            HDmemcpy(reclaim_buf, buf, buf_size);

	    /* Set background buffer to all zeros */
	    if(bkg_buf)
		HDmemset(bkg_buf, 0, buf_size);

            /* Convert from memory to destination file */
            if(H5T_convert(tpath_mem_dst, tid_mem, tid_dst, nelmts, (size_t)0, (size_t)0, buf, bkg_buf) < 0)
                HGOTO_ERROR(H5E_DATATYPE, H5E_CANTINIT, NULL, "datatype conversion NULLed")

            HDmemcpy(attr_dst->shared->data, buf, attr_dst->shared->data_size);

            if(H5D_vlen_reclaim(tid_mem, buf_space, reclaim_buf) < 0)
                HGOTO_ERROR(H5E_DATASET, H5E_BADITER, NULL, "unable to reclaim variable-length data")
        }  /* end if */
        else {
            HDassert(attr_dst->shared->data_size == attr_src->shared->data_size);
            HDmemcpy(attr_dst->shared->data, attr_src->shared->data, attr_src->shared->data_size);
        } /* end else */
    } /* end if(attr_src->shared->data) */

    /* Copy the creation order */
    attr_dst->shared->crt_idx = attr_src->shared->crt_idx;

    /* Recompute the version to encode the destination attribute */
    if(H5A__set_version(file_dst, attr_dst) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTSET, NULL, "unable to update attribute version")

    /* Recompute the destination attribute's size, if it's a different version */
    if(attr_src->shared->version != attr_dst->shared->version)
        *recompute_size = TRUE;

    /* Set return value */
    ret_value = attr_dst;

done:
    if(buf_sid > 0 && H5I_dec_ref(buf_sid) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "Can't decrement temporary dataspace ID")
    if(tid_src > 0)
        /* Don't decrement ID, we want to keep underlying datatype */
        if(NULL == H5I_remove(tid_src))
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "Can't decrement temporary datatype ID")
    if(tid_dst > 0)
        /* Don't decrement ID, we want to keep underlying datatype */
        if(NULL == H5I_remove(tid_dst))
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "Can't decrement temporary datatype ID")
    if(tid_mem > 0)
        /* Decrement the memory datatype ID, it's transient */
        if(H5I_dec_ref(tid_mem) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "Can't decrement temporary datatype ID")
    if(buf)
        buf = H5FL_BLK_FREE(attr_buf, buf);
    if(reclaim_buf)
        reclaim_buf = H5FL_BLK_FREE(attr_buf, reclaim_buf);
    if(bkg_buf)
        bkg_buf = H5FL_BLK_FREE(attr_buf, bkg_buf);

    /* Release destination attribute information on failure */
    if(!ret_value && attr_dst && H5A__close(attr_dst) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTFREE, NULL, "can't close attribute")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__attr_copy_file() */


/*-------------------------------------------------------------------------
 * Function:    H5A__attr_post_copy_file
 *
 * Purpose:     Finish copying a message from between files.
 *              We have to copy the values of a reference attribute in the
 *              post copy because H5O_post_copy_file() fails at the case that
 *              an object may have a reference attribute that points to the
 *              object itself.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Peter Cao
 *              March 6, 2005
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__attr_post_copy_file(const H5O_loc_t *src_oloc, const H5A_t *attr_src,
    H5O_loc_t *dst_oloc, const H5A_t *attr_dst, H5O_copy_t *cpy_info)
{
    H5F_t  *file_src, *file_dst;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(src_oloc);
    HDassert(dst_oloc);
    HDassert(attr_dst);
    HDassert(attr_src);

    file_src = src_oloc->file;
    file_dst = dst_oloc->file;

    HDassert(file_src);
    HDassert(file_dst);

    if(H5T_committed(attr_src->shared->dt)) {
        H5O_loc_t         *src_oloc_dt;           /* Pointer to source datatype's object location */
        H5O_loc_t         *dst_oloc_dt;           /* Pointer to dest. datatype's object location */

        /* Get group entries for source & destination */
        src_oloc_dt = H5T_oloc(attr_src->shared->dt);
        HDassert(src_oloc_dt);
        dst_oloc_dt = H5T_oloc(attr_dst->shared->dt);
        HDassert(dst_oloc_dt);

        /* Reset object location for new object */
        H5O_loc_reset(dst_oloc_dt);
        dst_oloc_dt->file = file_dst;

        /* Copy the shared object from source to destination */
        if(H5O_copy_header_map(src_oloc_dt, dst_oloc_dt, cpy_info, FALSE, NULL, NULL) < 0)
            HGOTO_ERROR(H5E_OHDR, H5E_CANTCOPY, FAIL, "unable to copy object")

        /* Update shared message info from named datatype info */
        H5T_update_shared(attr_dst->shared->dt);
    } /* end if */

    /* Try to share both the datatype and dataset.  This does nothing if the
     * datatype is committed or sharing is disabled.
     */
    if(H5SM_try_share(file_dst, NULL, H5SM_WAS_DEFERRED, H5O_DTYPE_ID, attr_dst->shared->dt, NULL) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "can't share attribute datatype")
    if(H5SM_try_share(file_dst, NULL, H5SM_WAS_DEFERRED, H5O_SDSPACE_ID, attr_dst->shared->ds, NULL) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "can't share attribute dataspace")

    /* Only need to fix reference attribute with real data being copied to
     *  another file.
     */
    if((NULL != attr_dst->shared->data) && (H5T_get_class(attr_dst->shared->dt, FALSE) == H5T_REFERENCE) ) {

        /* copy object pointed by reference. The current implementation does not
         *  deal with nested reference such as reference in a compound structure
         */

        /* Check for expanding references */
        if(cpy_info->expand_ref) {
            size_t ref_count;

            /* Determine # of reference elements to copy */
            ref_count = attr_dst->shared->data_size / H5T_get_size(attr_dst->shared->dt);

            /* Copy objects referenced in source buffer to destination file and set destination elements */
            if(H5O_copy_expand_ref(file_src, attr_dst->shared->data, file_dst, attr_dst->shared->data, ref_count, H5T_get_ref_type(attr_dst->shared->dt), cpy_info) < 0)
                HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, FAIL, "unable to copy reference attribute")
        } /* end if */
        else
            /* Reset value to zero */
            HDmemset(attr_dst->shared->data, 0, attr_dst->shared->data_size);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__attr_post_copy_file() */


/*-------------------------------------------------------------------------
 * Function:    H5A__dense_post_copy_file_cb
 *
 * Purpose:     Callback routine for copying a dense attribute from SRC to DST.
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Peter Cao
 *              xcao@hdfgroup.org
 *              July 20, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A__dense_post_copy_file_cb(const H5A_t *attr_src, void *_udata)
{
    H5A_dense_file_cp_ud_t *udata = (H5A_dense_file_cp_ud_t *)_udata;
    H5A_t *attr_dst = NULL;
    herr_t ret_value = H5_ITER_CONT;   /* Return value */

    FUNC_ENTER_STATIC

    /* check arguments */
    HDassert(attr_src);
    HDassert(udata);
    HDassert(udata->ainfo);
    HDassert(udata->file);
    HDassert(udata->cpy_info);

    if(NULL == (attr_dst = H5A__attr_copy_file(attr_src, udata->file, udata->recompute_size, udata->cpy_info)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, H5_ITER_ERROR, "can't copy attribute")

    if(H5A__attr_post_copy_file(udata->oloc_src, attr_src, udata->oloc_dst, attr_dst, udata->cpy_info) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, H5_ITER_ERROR, "can't copy attribute")

    /* Reset shared location information */
    if(H5O_msg_reset_share(H5O_ATTR_ID, attr_dst) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTINIT, FAIL, "unable to reset attribute sharing")

    /* Set COPIED tag for destination object's metadata */
    H5_BEGIN_TAG(H5AC__COPIED_TAG);

    /* Insert attribute into dense storage */
    if(H5A__dense_insert(udata->file, udata->ainfo, attr_dst) < 0)
        HGOTO_ERROR_TAG(H5E_OHDR, H5E_CANTINSERT, H5_ITER_ERROR, "unable to add to dense storage")

    /* Reset metadata tag */
    H5_END_TAG

done:
    if(attr_dst && H5A__close(attr_dst) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CLOSEERROR, FAIL, "can't close destination attribute")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__dense_post_copy_file_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5A__dense_post_copy_file_all
 *
 * Purpose:     Copy all dense attributes from SRC to DST.
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  Peter Cao
 *              xcao@hdfgroup.org
 *              July 20, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__dense_post_copy_file_all(const H5O_loc_t *src_oloc, const H5O_ainfo_t *ainfo_src,
    H5O_loc_t *dst_oloc, H5O_ainfo_t *ainfo_dst, H5O_copy_t *cpy_info)
{
    H5A_dense_file_cp_ud_t udata;       /* User data for iteration callback */
    H5A_attr_iter_op_t attr_op;         /* Attribute operator */
    hbool_t recompute_size = FALSE;     /* recompute the size */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* check arguments */
    HDassert(ainfo_src);
    HDassert(ainfo_dst);

    udata.ainfo = ainfo_dst;                  /* Destination dense information    */
    udata.file = dst_oloc->file;              /* Destination file                 */
    udata.recompute_size = &recompute_size;   /* Flag to indicate if size changed */
    udata.cpy_info = cpy_info;                /* Information on copying options   */
    udata.oloc_src = src_oloc;
    udata.oloc_dst = dst_oloc;

    attr_op.op_type = H5A_ATTR_OP_LIB;
    attr_op.u.lib_op = H5A__dense_post_copy_file_cb;

    if(H5A__dense_iterate(src_oloc->file, (hid_t)0, ainfo_src, H5_INDEX_NAME,
            H5_ITER_NATIVE, (hsize_t)0, NULL, &attr_op, &udata) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, FAIL, "error building attribute table")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5A__dense_post_copy_file_all */


/*-------------------------------------------------------------------------
 * Function:	H5A__rename_by_name
 *
 * Purpose:     Private version of H5Arename_by_name
 *
 * Return:	Success:             Non-negative
 *		Failure:             Negative
 *
 * Programmer:	Quincey Koziol
 *              February 20, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__rename_by_name(H5G_loc_t loc, const char *obj_name, const char *old_attr_name,
    const char *new_attr_name)
{
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    herr_t	ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_PACKAGE

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(&loc, obj_name, &obj_loc/*out*/) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "object not found")
    loc_found = TRUE;

    /* Call attribute rename routine */
    if(H5O__attr_rename(obj_loc.oloc, old_attr_name, new_attr_name) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTRENAME, FAIL, "can't rename attribute")

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__rename_by_name() */


/*-------------------------------------------------------------------------
 * Function:	H5A__iterate_common
 *
 * Purpose:     Internal common version of H5Aiterate
 *
 * Return:	Success:             Non-negative
 *		Failure:             Negative
 *
 * Programmer:	Quincey Koziol
 *              December 6, 2017
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5A__iterate_common(hid_t loc_id, H5_index_t idx_type, H5_iter_order_t order,
    hsize_t *idx, H5A_attr_iter_op_t *attr_op, void *op_data)
{
    hsize_t start_idx;          /* Index of attribute to start iterating at */
    hsize_t last_attr;          /* Index of last attribute examined */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC_NOERR

    /* Call attribute iteration routine */
    last_attr = start_idx = (idx ? *idx : 0);
    if((ret_value = H5O__attr_iterate(loc_id, idx_type, order, start_idx, &last_attr, attr_op, op_data)) < 0)
        HERROR(H5E_ATTR, H5E_BADITER, "error iterating over attributes");

    /* Set the last attribute information */
    if(idx)
        *idx = last_attr;

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__iterate_common() */


/*-------------------------------------------------------------------------
 * Function:	H5A__iterate
 *
 * Purpose:     Private version of H5Aiterate2
 *
 * Return:	Success:             Non-negative
 *		Failure:             Negative
 *
 * Programmer:	Quincey Koziol
 *              December 6, 2017
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__iterate(hid_t loc_id, H5_index_t idx_type, H5_iter_order_t order,
    hsize_t *idx, H5A_operator2_t op, void *op_data)
{
    H5A_attr_iter_op_t  attr_op;        /* Attribute operator */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Build attribute operator info */
    attr_op.op_type = H5A_ATTR_OP_APP2;
    attr_op.u.app_op2 = op;

    /* Call internal attribute iteration routine */
    if((ret_value = H5A__iterate_common(loc_id, idx_type, order, idx, &attr_op, op_data)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_BADITER, FAIL, "error iterating over attributes")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__iterate() */

#ifndef H5_NO_DEPRECATED_SYMBOLS

/*-------------------------------------------------------------------------
 * Function:	H5A__iterate_old
 *
 * Purpose:     Private version of H5Aiterate1
 *
 * Return:	Success:             Non-negative
 *		Failure:             Negative
 *
 * Programmer:	Quincey Koziol
 *              December 6, 2017
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__iterate_old(hid_t loc_id, unsigned *attr_num, H5A_operator1_t op,
    void *op_data)
{
    H5A_attr_iter_op_t  attr_op;        /* Attribute operator */
    hsize_t idx;                        /* Index of attribute to start iterating at */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Build attribute operator info */
    attr_op.op_type = H5A_ATTR_OP_APP;
    attr_op.u.app_op = op;

    /* Set up index */
    idx = (hsize_t)(attr_num ? *attr_num : 0);

    /* Call internal attribute iteration routine */
    if((ret_value = H5A__iterate_common(loc_id, H5_INDEX_CRT_ORDER, H5_ITER_INC, &idx, &attr_op, op_data)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_BADITER, FAIL, "error iterating over attributes")

    /* Translate hsize_t index value to legacy unsigned index value*/
    if(attr_num)
        *attr_num = (unsigned)idx;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__iterate_old() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


/*-------------------------------------------------------------------------
 * Function:	H5A__iterate_by_name
 *
 * Purpose:     Private version of H5Aiterate_by_name
 *
 * Return:	Success:             Non-negative
 *		Failure:             Negative
 *
 * Programmer:	Quincey Koziol
 *              December 6, 2017
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__iterate_by_name(const H5G_loc_t *loc, const char *obj_name, H5_index_t idx_type, H5_iter_order_t order, hsize_t *idx, H5A_operator2_t op,
    void *op_data)
{
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;               /* Opened object group hier. path */
    H5O_loc_t   obj_oloc;               /* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    hid_t       obj_loc_id = (-1);      /* ID for object located */
    H5A_attr_iter_op_t  attr_op;        /* Attribute operator */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(loc, obj_name, &obj_loc/*out*/) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "object not found")
    loc_found = TRUE;

    /* Open the object */
    if((obj_loc_id = H5O__open_by_loc(&obj_loc, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTOPENOBJ, FAIL, "unable to open object")

    /* Build attribute operator info */
    attr_op.op_type = H5A_ATTR_OP_APP2;
    attr_op.u.app_op2 = op;

    /* Call attribute iteration routine */
    if((ret_value = H5A__iterate_common(obj_loc_id, idx_type, order, idx, &attr_op, op_data)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_BADITER, FAIL, "error iterating over attributes")

done:
    /* Release resources */
    if(obj_loc_id > 0) {
        if(H5I_dec_app_ref(obj_loc_id) < 0)
            HDONE_ERROR(H5E_ATTR, H5E_CANTDEC, FAIL, "unable to close temporary object")
    } /* end if */
    else if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__iterate_by_name() */


/*-------------------------------------------------------------------------
 * Function:	H5A__delete_by_name
 *
 * Purpose:     Private version of H5Adelete_by_name
 *
 * Return:	Success:             Non-negative
 *		Failure:             Negative
 *
 * Programmer:	Quincey Koziol
 *              December 6, 2017
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__delete_by_name(const H5G_loc_t *loc, const char *obj_name, const char *attr_name)
{
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(loc, obj_name, &obj_loc/*out*/) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "object not found")
    loc_found = TRUE;

    /* Delete the attribute from the location */
    if(H5O__attr_remove(obj_loc.oloc, attr_name) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__delete_by_name() */


/*-------------------------------------------------------------------------
 * Function:	H5A__delete_by_idx
 *
 * Purpose:     Private version of H5Adelete_by_idx
 *
 * Return:	Success:             Non-negative
 *		Failure:             Negative
 *
 * Programmer:	Quincey Koziol
 *              December 6, 2017
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5A__delete_by_idx(const H5G_loc_t *loc, const char *obj_name, H5_index_t idx_type,
    H5_iter_order_t order, hsize_t n)
{
    H5G_loc_t   obj_loc;                /* Location used to open group */
    H5G_name_t  obj_path;            	/* Opened object group hier. path */
    H5O_loc_t   obj_oloc;            	/* Opened object object location */
    hbool_t     loc_found = FALSE;      /* Entry at 'obj_name' found */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_PACKAGE

    /* Set up opened group location to fill in */
    obj_loc.oloc = &obj_oloc;
    obj_loc.path = &obj_path;
    H5G_loc_reset(&obj_loc);

    /* Find the object's location */
    if(H5G_loc_find(loc, obj_name, &obj_loc/*out*/) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_NOTFOUND, FAIL, "object not found")
    loc_found = TRUE;

    /* Delete the attribute from the location */
    if(H5O__attr_remove_by_idx(obj_loc.oloc, idx_type, order, n) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDELETE, FAIL, "unable to delete attribute")

done:
    /* Release resources */
    if(loc_found && H5G_loc_free(&obj_loc) < 0)
        HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, FAIL, "can't free location")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5A__delete_by_idx() */


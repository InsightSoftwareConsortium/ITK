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

/****************/
/* Module Setup */
/****************/

#include "H5Smodule.h"          /* This source code file is part of the H5S module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5CXprivate.h"    /* API Contexts         */
#include "H5Fprivate.h"		/* Files				*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Oprivate.h"		/* Object headers		  	*/
#include "H5Spkg.h"		/* Dataspaces 				*/


/****************/
/* Local Macros */
/****************/

/* Version of dataspace encoding */
#define H5S_ENCODE_VERSION      0


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Local Prototypes */
/********************/
static htri_t H5S_is_simple(const H5S_t *sdim);


/*********************/
/* Package Variables */
/*********************/

/* Package initialization variable */
hbool_t H5_PKG_INIT_VAR = FALSE;

/* Format version bounds for dataspace */
const unsigned H5O_sdspace_ver_bounds[] = {
    H5O_SDSPACE_VERSION_1,      /* H5F_LIBVER_EARLIEST */
    H5O_SDSPACE_VERSION_2,      /* H5F_LIBVER_V18 */
    H5O_SDSPACE_VERSION_LATEST  /* H5F_LIBVER_LATEST */
};

/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5S_extent_t struct */
H5FL_DEFINE(H5S_extent_t);

/* Declare a free list to manage the H5S_t struct */
H5FL_DEFINE(H5S_t);

/* Declare a free list to manage the array's of hsize_t's */
H5FL_ARR_DEFINE(hsize_t, H5S_MAX_RANK);

/* Dataspace ID class */
static const H5I_class_t H5I_DATASPACE_CLS[1] = {{
    H5I_DATASPACE,		/* ID class value */
    0,				/* Class flags */
    2,				/* # of reserved IDs for class */
    (H5I_free_t)H5S_close	/* Callback routine for closing objects of this class */
}};


/* Flag indicating "top" of interface has been initialized */
static hbool_t H5S_top_package_initialize_s = FALSE;



/*--------------------------------------------------------------------------
NAME
   H5S__init_package -- Initialize interface-specific information
USAGE
    herr_t H5S__init_package()
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.
--------------------------------------------------------------------------*/
herr_t
H5S__init_package(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_PACKAGE

    /* Initialize the atom group for the file IDs */
    if(H5I_register_type(H5I_DATASPACE_CLS) < 0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to initialize interface")

    /* Mark "top" of interface as initialized, too */
    H5S_top_package_initialize_s = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S__init_package() */


/*--------------------------------------------------------------------------
 NAME
    H5S_top_term_package
 PURPOSE
    Terminate various H5S objects
 USAGE
    void H5S_top_term_package()
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Release IDs for the atom group, deferring full interface shutdown
    until later (in H5S_term_package).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
int
H5S_top_term_package(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5S_top_package_initialize_s) {
	if(H5I_nmembers(H5I_DATASPACE) > 0) {
	    (void)H5I_clear_type(H5I_DATASPACE, FALSE, FALSE);
            n++; /*H5I*/
	} /* end if */

	/* Mark "top" of interface as closed */
        if(0 == n)
            H5S_top_package_initialize_s = FALSE;
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5S_top_term_package() */


/*--------------------------------------------------------------------------
 NAME
    H5S_term_package
 PURPOSE
    Terminate various H5S objects
 USAGE
    void H5S_term_package()
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    Release the atom group and any other resources allocated.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
     Can't report errors...

     Finishes shutting down the interface, after H5S_top_term_package()
     is called
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
int
H5S_term_package(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_PKG_INIT_VAR) {
        /* Sanity checks */
        HDassert(0 == H5I_nmembers(H5I_DATASPACE));
        HDassert(FALSE == H5S_top_package_initialize_s);

        /* Destroy the dataspace object id group */
        n += (H5I_dec_type_ref(H5I_DATASPACE) > 0);

	/* Mark interface as closed */
        if(0 == n)
            H5_PKG_INIT_VAR = FALSE;
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5S_term_package() */



/*--------------------------------------------------------------------------
 NAME
    H5S_get_validiated_dataspace
 PURPOSE
    Get a pointer to a validated H5S_t pointer
 USAGE
   H5S_t *H5S_get_validated_space(dataspace_id, space)
    hid_t           space_id;       IN: The ID of the dataspace
    const H5S_t *   space;          OUT: A pointer to the dataspace
 RETURNS
    SUCCEED/FAIL
 DESCRIPTION
    Gets a pointer to a dataspace struct after validating it. The pointer
    can be NULL (if the ID is H5S_ALL, for example).
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
herr_t
H5S_get_validated_dataspace(hid_t space_id, const H5S_t **space)
{
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(space);

    if (space_id < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid space_id (ID cannot be a negative number)")
 
    if (H5S_ALL == space_id) {
        /* No special dataspace struct for H5S_ALL */
        *space = NULL;
    }
    else {
        /* Get the dataspace pointer */
        if (NULL == (*space = (const H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "space_id is not a dataspace ID")

        /* Check for valid selection */
        if (H5S_SELECT_VALID(*space) != TRUE)
            HGOTO_ERROR(H5E_DATASPACE, H5E_BADRANGE, FAIL, "selection + offset not within extent")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5S_get_validated_dataspace() */


/*--------------------------------------------------------------------------
 NAME
    H5S_create
 PURPOSE
    Create empty, typed dataspace
 USAGE
   H5S_t *H5S_create(type)
    H5S_type_t  type;           IN: Dataspace type to create
 RETURNS
    Pointer to dataspace on success, NULL on failure
 DESCRIPTION
    Creates a new dataspace of a given type.  The extent is undefined and the
    selection is set to the "all" selection.
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
H5S_t *
H5S_create(H5S_class_t type)
{
    H5S_t *new_ds = NULL;    /* New dataspace created */
    H5S_t *ret_value = NULL; /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Create a new dataspace */
    if(NULL == (new_ds = H5FL_CALLOC(H5S_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Initialize default dataspace state */
    new_ds->extent.type = type;
    if(type == H5S_NULL)
        new_ds->extent.version = H5O_SDSPACE_VERSION_2;
    else
        new_ds->extent.version = H5O_SDSPACE_VERSION_1;
    new_ds->extent.rank = 0;
    new_ds->extent.size = new_ds->extent.max = NULL;

    switch(type) {
        case H5S_SCALAR:
            new_ds->extent.nelem = 1;
            break;

        case H5S_SIMPLE:
        case H5S_NULL:
            new_ds->extent.nelem = 0;
            break;

        case H5S_NO_CLASS:
        default:
            HDassert("unknown dataspace (extent) type" && 0);
            break;
    } /* end switch */

    /* Start with "all" selection */
    if(H5S_select_all(new_ds, FALSE) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, NULL, "unable to set all selection")

    /* Reset common selection info pointer */
    new_ds->select.sel_info.hslab = NULL;

    /* Reset "shared" info on extent */
    if(H5O_msg_reset_share(H5O_SDSPACE_ID, &(new_ds->extent.sh_loc)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTRESET, NULL, "unable to reset shared component info")

    /* Set return value */
    ret_value = new_ds;

done:
    if(ret_value == NULL) {
        if(new_ds && H5S_close(new_ds) < 0)
            HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, NULL, "unable to release dataspace")
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_create() */


/*--------------------------------------------------------------------------
 NAME
    H5Screate
 PURPOSE
    Create empty, typed dataspace
 USAGE
   hid_t  H5Screate(type)
    H5S_type_t  type;           IN: Dataspace type to create
 RETURNS
    Valid dataspace ID on success, negative on failure
 DESCRIPTION
    Creates a new dataspace of a given type.  The extent & selection are
    undefined
 GLOBAL VARIABLES
 COMMENTS, BUGS, ASSUMPTIONS
 EXAMPLES
 REVISION LOG
--------------------------------------------------------------------------*/
hid_t
H5Screate(H5S_class_t type)
{
    H5S_t *new_ds=NULL;         /* New dataspace structure */
    hid_t ret_value;            /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "Sc", type);

    /* Check args */
    if(type <= H5S_NO_CLASS || type > H5S_NULL)  /* don't allow complex dataspace yet */
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid dataspace type")

    if(NULL == (new_ds = H5S_create(type)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "unable to create dataspace")

    /* Atomize */
    if((ret_value = H5I_register (H5I_DATASPACE, new_ds, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom")

done:
    if(ret_value < 0) {
        if(new_ds && H5S_close(new_ds) < 0)
            HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace")
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Screate() */


/*-------------------------------------------------------------------------
 * Function:	H5S_extent_release
 *
 * Purpose:	Releases all memory associated with a dataspace extent.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_extent_release(H5S_extent_t *extent)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(extent);

    /* Release extent */
    if(extent->type == H5S_SIMPLE) {
        if(extent->size)
            extent->size = H5FL_ARR_FREE(hsize_t, extent->size);
        if(extent->max)
            extent->max = H5FL_ARR_FREE(hsize_t, extent->max);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
}   /* end H5S_extent_release() */


/*-------------------------------------------------------------------------
 * Function:	H5S_close
 *
 * Purpose:	Releases all memory associated with a dataspace.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_close(H5S_t *ds)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(ds);

    /* Release selection (this should come before the extent release) */
    if(H5S_SELECT_RELEASE(ds) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace selection")

    /* Release extent */
    if(H5S_extent_release(&ds->extent) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace extent")

    /* Release the main structure */
    ds = H5FL_FREE(H5S_t, ds);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_close() */


/*-------------------------------------------------------------------------
 * Function:	H5Sclose
 *
 * Purpose:	Release access to a dataspace object.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Errors:
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Sclose(hid_t space_id)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", space_id);

    /* Check args */
    if (NULL == H5I_object_verify(space_id,H5I_DATASPACE))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    /* When the reference count reaches zero the resources are freed */
    if(H5I_dec_app_ref(space_id) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDEC, FAIL, "problem freeing id")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Sclose() */


/*-------------------------------------------------------------------------
 * Function:	H5Scopy
 *
 * Purpose:	Copies a dataspace.
 *
 * Return:	Success:	ID of the new dataspace
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Friday, January 30, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Scopy(hid_t space_id)
{
    H5S_t	*src;
    H5S_t	*dst = NULL;
    hid_t	ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", space_id);

    /* Check args */
    if(NULL == (src = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    /* Copy */
    if(NULL == (dst = H5S_copy(src, FALSE, TRUE)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to copy dataspace")

    /* Atomize */
    if((ret_value = H5I_register (H5I_DATASPACE, dst, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace atom")

done:
    if(ret_value < 0) {
        if(dst && H5S_close(dst) < 0)
            HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace")
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Scopy() */


/*-------------------------------------------------------------------------
 * Function:	H5Sextent_copy
 *
 * Purpose:	Copies a dataspace extent.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Sextent_copy(hid_t dst_id,hid_t src_id)
{
    H5S_t	*src;
    H5S_t	*dst;
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "ii", dst_id, src_id);

    /* Check args */
    if(NULL == (src = (H5S_t *)H5I_object_verify(src_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")
    if(NULL == (dst = (H5S_t *)H5I_object_verify(dst_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    /* Copy */
    if(H5S_extent_copy(dst, src) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "can't copy extent")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Sextent_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5S_extent_copy
 *
 * Purpose:     Copies a dataspace extent
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Neil Fortner
 *              Monday, February 23, 2015
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_extent_copy(H5S_t *dst, const H5S_t *src)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(dst);
    HDassert(src);

    /* Copy extent */
    if(H5S_extent_copy_real(&(dst->extent), &(src->extent), TRUE) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "can't copy extent")

    /* If the selection is 'all', update the number of elements selected in the
     * destination space */
    if(H5S_SEL_ALL == H5S_GET_SELECT_TYPE(dst))
        if(H5S_select_all(dst, FALSE) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_extent_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5S_extent_copy_real
 *
 * Purpose:	Copies a dataspace extent
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Wednesday, June  3, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_extent_copy_real(H5S_extent_t *dst, const H5S_extent_t *src, hbool_t copy_max)
{
    unsigned u;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Release destination extent before we copy over it */
    if(H5S_extent_release(dst) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace extent")

    /* Copy the regular fields */
    dst->type = src->type;
    dst->version = src->version;
    dst->nelem = src->nelem;
    dst->rank = src->rank;

    switch (src->type) {
        case H5S_NULL:
        case H5S_SCALAR:
            dst->size = NULL;
            dst->max = NULL;
            break;

        case H5S_SIMPLE:
            if(src->size) {
                dst->size = (hsize_t *)H5FL_ARR_MALLOC(hsize_t, (size_t)src->rank);
                for(u = 0; u < src->rank; u++)
                    dst->size[u] = src->size[u];
            } /* end if */
            else
                dst->size = NULL;
            if(copy_max && src->max) {
                dst->max = (hsize_t *)H5FL_ARR_MALLOC(hsize_t, (size_t)src->rank);
                for(u = 0; u < src->rank; u++)
                    dst->max[u] = src->max[u];
            } /* end if */
            else
                dst->max = NULL;
            break;

        case H5S_NO_CLASS:
        default:
            HDassert("unknown dataspace type" && 0);
            break;
    } /* end switch */

    /* Copy the shared object info */
    if(H5O_set_shared(&(dst->sh_loc), &(src->sh_loc)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, FAIL, "can't copy shared information")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_extent_copy_real() */


/*-------------------------------------------------------------------------
 * Function:	H5S_copy
 *
 * Purpose:	Copies a dataspace, by copying the extent and selection through
 *          H5S_extent_copy and H5S_select_copy.  If the SHARE_SELECTION flag
 *          is set, then the selection can be shared between the source and
 *          destination dataspaces.  (This should only occur in situations
 *          where the destination dataspace will immediately change to a new
 *          selection)
 *
 * Return:	Success:	A pointer to a new copy of SRC
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Thursday, December  4, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5S_t *
H5S_copy(const H5S_t *src, hbool_t share_selection, hbool_t copy_max)
{
    H5S_t	*dst = NULL;
    H5S_t       *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    if(NULL == (dst = H5FL_CALLOC(H5S_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Copy the source dataspace's extent */
    if(H5S_extent_copy_real(&(dst->extent), &(src->extent), copy_max) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, NULL, "can't copy extent")

    /* Copy the source dataspace's selection */
    if(H5S_select_copy(dst, src, share_selection) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, NULL, "can't copy select")

    /* Set the return value */
    ret_value = dst;

done:
    if(NULL == ret_value)
        if(dst)
            dst = H5FL_FREE(H5S_t, dst);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_copy() */


/*-------------------------------------------------------------------------
 * Function:	H5S_get_simple_extent_npoints
 *
 * Purpose:	Determines how many data points a dataset extent has.
 *
 * Return:	Success:	Number of data points in the dataset extent.
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Note:        This routine participates in the "Inlining C function pointers"
 *              pattern, don't call it directly, use the appropriate macro
 *              defined in H5Sprivate.h.
 *
 * Modifications:
 *  Changed Name - QAK 7/7/98
 *
 *-------------------------------------------------------------------------
 */
hssize_t
H5S_get_simple_extent_npoints(const H5S_t *ds)
{
    hssize_t    ret_value = -1;         /* Return value */

    FUNC_ENTER_NOAPI(-1)

    /* check args */
    HDassert(ds);

    /* Get the number of elements in extent */
    ret_value = (hssize_t)ds->extent.nelem;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_get_simple_extent_npoints() */


/*-------------------------------------------------------------------------
 * Function:	H5Sget_simple_extent_npoints
 *
 * Purpose:	Determines how many data points a dataset extent has.
 *
 * Return:	Success:	Number of data points in the dataset.
 *
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *  Changed Name - QAK 7/7/98
 *
 *-------------------------------------------------------------------------
 */
hssize_t
H5Sget_simple_extent_npoints(hid_t space_id)
{
    H5S_t		   *ds;
    hssize_t		    ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("Hs", "i", space_id);

    /* Check args */
    if(NULL == (ds = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    ret_value = (hssize_t)H5S_GET_EXTENT_NPOINTS(ds);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Sget_simple_extent_npoints() */


/*-------------------------------------------------------------------------
 * Function:	H5S_get_npoints_max
 *
 * Purpose:	Determines the maximum number of data points a dataspace may
 *		have.  If the `max' array is null then the maximum number of
 *		data points is the same as the current number of data points
 *		without regard to the hyperslab.  If any element of the `max'
 *		array is zero then the maximum possible size is returned.
 *
 * Return:	Success:	Maximum number of data points the dataspace
 *				may have.
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5S_get_npoints_max(const H5S_t *ds)
{
    unsigned	    u;
    hsize_t	    ret_value = 0;      /* Return value */

    FUNC_ENTER_NOAPI(0)

    /* check args */
    HDassert(ds);

    switch (H5S_GET_EXTENT_TYPE(ds)) {
        case H5S_NULL:
            ret_value = 0;
            break;

        case H5S_SCALAR:
            ret_value = 1;
            break;

        case H5S_SIMPLE:
            if (ds->extent.max) {
                for (ret_value=1, u=0; u<ds->extent.rank; u++) {
                    if (H5S_UNLIMITED==ds->extent.max[u]) {
                        ret_value = HSIZET_MAX;
                        break;
                    }
                    else
                        ret_value *= ds->extent.max[u];
                }
            }
            else {
                for (ret_value=1, u=0; u<ds->extent.rank; u++)
                    ret_value *= ds->extent.size[u];
            }
            break;

        case H5S_NO_CLASS:
        default:
            HDassert("unknown dataspace class" && 0);
            HGOTO_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, 0, "internal error (unknown dataspace class)")
    }

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Sget_simple_extent_ndims
 *
 * Purpose:	Determines the dimensionality of a dataspace.
 *
 * Return:	Success:	The number of dimensions in a dataspace.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5Sget_simple_extent_ndims(hid_t space_id)
{
    H5S_t		   *ds;
    int		   ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("Is", "i", space_id);

    /* Check args */
    if(NULL == (ds = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    ret_value = (int)H5S_GET_EXTENT_NDIMS(ds);

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Sget_simple_extent_ndims() */


/*-------------------------------------------------------------------------
 * Function:	H5S_get_simple_extent_ndims
 *
 * Purpose:	Returns the number of dimensions in a dataspace.
 *
 * Return:	Success:	Non-negative number of dimensions.  Zero
 *				implies a scalar.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Thursday, December 11, 1997
 *
 * Note:        This routine participates in the "Inlining C function pointers"
 *              pattern, don't call it directly, use the appropriate macro
 *              defined in H5Sprivate.h.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5S_get_simple_extent_ndims(const H5S_t *ds)
{
    int	ret_value = -1;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check args */
    HDassert(ds);

    switch(H5S_GET_EXTENT_TYPE(ds)) {
        case H5S_NULL:
        case H5S_SCALAR:
        case H5S_SIMPLE:
            ret_value = (int)ds->extent.rank;
            break;

        case H5S_NO_CLASS:
        default:
            HDassert("unknown dataspace class" && 0);
            HGOTO_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL, "internal error (unknown dataspace class)")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_get_simple_extent_ndims() */


/*-------------------------------------------------------------------------
 * Function:	H5Sget_simple_extent_dims
 *
 * Purpose:	Returns the size and maximum sizes in each dimension of
 *		a dataspace DS through	the DIMS and MAXDIMS arguments.
 *
 * Return:	Success:	Number of dimensions, the same value as
 *				returned by H5Sget_simple_extent_ndims().
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Thursday, December 11, 1997
 *
 * Modifications:
 *		June 18, 1998	Albert Cheng
 *		Added maxdims argument.  Removed dims argument check
 *		since it can still return ndims even if both dims and
 *		maxdims are NULLs.
 *
 *-------------------------------------------------------------------------
 */
int
H5Sget_simple_extent_dims(hid_t space_id, hsize_t dims[]/*out*/,
			  hsize_t maxdims[]/*out*/)
{
    H5S_t		   *ds;
    int		   ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("Is", "ixx", space_id, dims, maxdims);

    /* Check args */
    if (NULL == (ds = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    ret_value = H5S_get_simple_extent_dims(ds, dims, maxdims);

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5S_extent_get_dims
 *
 * Purpose:	Returns the size in each dimension of a dataspace.  This
 *		function may not be meaningful for all types of dataspaces.
 *
 * Return:	Success:	Number of dimensions.  Zero implies scalar.
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, June 30, 2009
 *
 *-------------------------------------------------------------------------
 */
int
H5S_extent_get_dims(const H5S_extent_t *ext, hsize_t dims[], hsize_t max_dims[])
{
    int	i;                      /* Local index variable */
    int	ret_value = -1;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check args */
    HDassert(ext);

    switch(ext->type) {
        case H5S_NULL:
        case H5S_SCALAR:
            ret_value = 0;
            break;

        case H5S_SIMPLE:
            ret_value = (int)ext->rank;
            for(i = 0; i < ret_value; i++) {
                if(dims)
                    dims[i] = ext->size[i];
                if(max_dims) {
                    if(ext->max)
                        max_dims[i] = ext->max[i];
                    else
                        max_dims[i] = ext->size[i];
                } /* end if */
            } /* end for */
            break;

        case H5S_NO_CLASS:
        default:
            HDassert("unknown dataspace class" && 0);
            HGOTO_ERROR(H5E_DATASPACE, H5E_UNSUPPORTED, FAIL, "internal error (unknown dataspace class)")
    } /* end switch */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_extent_get_dims() */


/*-------------------------------------------------------------------------
 * Function:	H5S_get_simple_extent_dims
 *
 * Purpose:	Returns the size in each dimension of a dataspace.  This
 *		function may not be meaningful for all types of dataspaces.
 *
 * Return:	Success:	Number of dimensions.  Zero implies scalar.
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *		Thursday, December 11, 1997
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int
H5S_get_simple_extent_dims(const H5S_t *ds, hsize_t dims[], hsize_t max_dims[])
{
    int	ret_value = -1;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* check args */
    HDassert(ds);

    /* Get dims for extent */
    if((ret_value = H5S_extent_get_dims(&ds->extent, dims, max_dims)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTGET, FAIL, "can't retrieve dataspace extent dims")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_get_simple_extent_dims() */


/*-------------------------------------------------------------------------
 * Function:	H5S_write
 *
 * Purpose:	Updates a dataspace by writing a message to an object
 *		header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_write(H5F_t *f, H5O_t *oh, unsigned update_flags, H5S_t *ds)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(f);
    HDassert(oh);
    HDassert(ds);
    HDassert(H5S_GET_EXTENT_TYPE(ds) >= 0);

    /* Write the current dataspace extent to the dataspace message */
    if(H5O_msg_write_oh(f, oh, H5O_SDSPACE_ID, 0, update_flags, &(ds->extent)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "can't update simple dataspace message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_write() */


/*-------------------------------------------------------------------------
 * Function:	H5S_append
 *
 * Purpose:	Updates a dataspace by adding a message to an object
 *		header.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, December 31, 2002
 *
 * Modifications:
 *
 *              John Mainzer, 6/6/05
 *              Updated function to use the new dirtied parameter of
 *              H5AC_unprotect() instead of manipulating the is_dirty
 *              field of the cache info.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_append(H5F_t *f, H5O_t *oh, H5S_t *ds)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert(f);
    HDassert(oh);
    HDassert(ds);
    HDassert(H5S_GET_EXTENT_TYPE(ds) >= 0);

    /* Add the dataspace message to the object header */
    if(H5O_msg_append_oh(f, oh, H5O_SDSPACE_ID, 0, 0, &(ds->extent)) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "can't add simple dataspace message")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_append() */


/*-------------------------------------------------------------------------
 * Function:	H5S_read
 *
 * Purpose:	Reads the dataspace from an object header.
 *
 * Return:	Success:	Pointer to a new dataspace.
 *
 *		Failure:	NULL
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 *-------------------------------------------------------------------------
 */
H5S_t *
H5S_read(const H5O_loc_t *loc)
{
    H5S_t	*ds = NULL;             /* Dataspace to return */
    H5S_t       *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* check args */
    HDassert(loc);

    if(NULL == (ds = H5FL_CALLOC(H5S_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    if(NULL == H5O_msg_read(loc, H5O_SDSPACE_ID, &(ds->extent)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, NULL, "unable to load dataspace info from dataset header")

    /* Default to entire dataspace being selected */
    if(H5S_select_all(ds, FALSE) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, NULL, "unable to set all selection")

    /* Set the value for successful return */
    ret_value = ds;

done:
    if(ret_value == NULL) {
        if(ds != NULL)
            ds = H5FL_FREE(H5S_t, ds);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_read() */


/*--------------------------------------------------------------------------
 NAME
    H5S_is_simple
 PURPOSE
    Check if a dataspace is simple (internal)
 USAGE
    htri_t H5S_is_simple(sdim)
	H5S_t *sdim;		IN: Pointer to dataspace object to query
 RETURNS
    TRUE/FALSE/FAIL
 DESCRIPTION
	This function determines the if a dataspace is "simple". ie. if it
    has orthogonal, evenly spaced dimensions.
--------------------------------------------------------------------------*/
static htri_t
H5S_is_simple(const H5S_t *sdim)
{
    htri_t ret_value = FAIL;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check args and all the boring stuff. */
    HDassert(sdim);

    /* H5S_NULL shouldn't be simple dataspace */
    ret_value = (H5S_GET_EXTENT_TYPE(sdim) == H5S_SIMPLE ||
	  H5S_GET_EXTENT_TYPE(sdim) == H5S_SCALAR) ? TRUE : FALSE;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_is_simple() */


/*--------------------------------------------------------------------------
 NAME
    H5Sis_simple
 PURPOSE
    Check if a dataspace is simple
 USAGE
    htri_t H5Sis_simple(space_id)
	hid_t space_id;	      IN: ID of dataspace object to query
 RETURNS
    TRUE/FALSE/FAIL
 DESCRIPTION
	This function determines the if a dataspace is "simple". ie. if it
    has orthogonal, evenly spaced dimensions.
--------------------------------------------------------------------------*/
htri_t
H5Sis_simple(hid_t space_id)
{
    H5S_t		   *space;	/* dataspace to modify */
    htri_t		    ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("t", "i", space_id);

    /* Check args and all the boring stuff. */
    if ((space = (H5S_t *)H5I_object_verify(space_id,H5I_DATASPACE)) == NULL)
	HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "not a dataspace")

    ret_value = H5S_is_simple(space);

  done:
    FUNC_LEAVE_API(ret_value)
}


/*--------------------------------------------------------------------------
 NAME
    H5Sset_extent_simple
 PURPOSE
    Sets the size of a simple dataspace
 USAGE
    herr_t H5Sset_extent_simple(space_id, rank, dims, max)
        hid_t space_id;	      IN: Dataspace object to query
        int rank;	      IN: # of dimensions for the dataspace
        const size_t *dims;   IN: Size of each dimension for the dataspace
 	const size_t *max;    IN: Maximum size of each dimension for the
 				  dataspace
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
    This function sets the number and size of each dimension in the
    dataspace. Setting RANK to a value of zero converts the dataspace to a
    scalar dataspace.  Dimensions are specified from slowest to fastest
    changing in the DIMS array (i.e. 'C' order).  Setting the size of a
    dimension in the MAX array to zero indicates that the dimension is of
    unlimited size and should be allowed to expand.  If MAX is NULL, the
    dimensions in the DIMS array are used as the maximum dimensions.
    Currently, only the first dimension in the array (the slowest) may be
    unlimited in size.

 MODIFICATION
    A null dataspace cannot be created from simple space with this function.

    Christian Chilan 01/17/2007
    Verifies that each element of DIMS is not equal to H5S_UNLIMITED.

    Raymond Lu 03/30/2011
    We allow 0 dimension size for non-unlimited dimension starting from 1.8.7
    release.
--------------------------------------------------------------------------*/
herr_t
H5Sset_extent_simple(hid_t space_id, int rank, const hsize_t dims[/*rank*/],
		      const hsize_t max[/*rank*/])
{
    H5S_t	*space;	/* dataspace to modify */
    int	        u;	/* local counting variable */
    herr_t      ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("e", "iIs*[a1]h*[a1]h", space_id, rank, dims, max);

    /* Check args */
    if ((space = (H5S_t *)H5I_object_verify(space_id,H5I_DATASPACE)) == NULL)
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "not a dataspace")
    if (rank > 0 && dims == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no dimensions specified")
    if (rank<0 || rank>H5S_MAX_RANK)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid rank")
    if (dims) {
        for (u=0; u<rank; u++) {
            if (H5S_UNLIMITED==dims[u])
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "current dimension must have a specific size, not H5S_UNLIMITED")
        }
    }
    if (max!=NULL) {
        if(dims==NULL)
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "maximum dimension specified, but no current dimensions specified")
        for (u=0; u<rank; u++) {
            if (max[u]!=H5S_UNLIMITED && max[u]<dims[u])
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid maximum dimension size")
        }
    }

    /* Do it */
    if (H5S_set_extent_simple(space, (unsigned)rank, dims, max)<0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "unable to set simple extent")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5S_set_extent_simple
 *
 * Purpose:	This is where the real work happens for
 *		H5Sset_extent_simple().
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke (copied from H5Sset_extent_simple)
 *              Wednesday, July  8, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_set_extent_simple(H5S_t *space, unsigned rank, const hsize_t *dims,
		       const hsize_t *max)
{
    unsigned u;                 /* Local index variable */
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check args */
    HDassert(rank <= H5S_MAX_RANK);
    HDassert(0 == rank || dims);

    /* shift out of the previous state to a "simple" dataspace.  */
    if(H5S_extent_release(&space->extent) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTFREE, FAIL, "failed to release previous dataspace extent")

    if(rank == 0) {		/* scalar variable */
        space->extent.type = H5S_SCALAR;
        space->extent.nelem = 1;
        space->extent.rank = 0;	/* set to scalar rank */
    } /* end if */
    else {
        hsize_t nelem;  /* Number of elements in extent */

        space->extent.type = H5S_SIMPLE;

        /* Set the rank and allocate space for the dims */
        space->extent.rank = rank;
        space->extent.size = (hsize_t *)H5FL_ARR_MALLOC(hsize_t, (size_t)rank);

        /* Copy the dimensions & compute the number of elements in the extent */
        for(u = 0, nelem = 1; u < space->extent.rank; u++) {
            space->extent.size[u] = dims[u];
            nelem *= dims[u];
        } /* end for */
        space->extent.nelem = nelem;

        /* Copy the maximum dimensions if specified. Otherwise, the maximal dimensions are the 
         * same as the dimension */
        space->extent.max = (hsize_t *)H5FL_ARR_MALLOC(hsize_t, (size_t)rank);
        if(max != NULL) {
            HDmemcpy(space->extent.max, max, sizeof(hsize_t) * rank);
        } else {
            for(u = 0; u < space->extent.rank; u++)
                space->extent.max[u] = dims[u];
        }
    } /* end else */

    /* Selection related cleanup */

    /* Set offset to zeros */
    HDmemset(space->select.offset, 0, sizeof(hsize_t) * space->extent.rank);
    space->select.offset_changed = FALSE;

    /* If the selection is 'all', update the number of elements selected */
    if(H5S_GET_SELECT_TYPE(space) == H5S_SEL_ALL)
        if(H5S_select_all(space, FALSE) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5S_set_extent_simple() */


/*-------------------------------------------------------------------------
 * Function:	H5Screate_simple
 *
 * Purpose:	Creates a new simple dataspace object and opens it for
 *		access. The DIMS argument is the size of the simple dataset
 *		and the MAXDIMS argument is the upper limit on the size of
 *		the dataset.  MAXDIMS may be the null pointer in which case
 *		the upper limit is the same as DIMS.  If an element of
 *		MAXDIMS is H5S_UNLIMITED then the corresponding dimension is
 *		unlimited, otherwise no element of MAXDIMS should be smaller
 *		than the corresponding element of DIMS.
 *
 * Return:	Success:	The ID for the new simple dataspace object.
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		Tuesday, January  27, 1998
 *
 * Modification:
 *              Raymond Lu 03/30/2011
 *              We allow 0-dimension for non-unlimited dimension starting 
 *              from 1.8.7 release.
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Screate_simple(int rank, const hsize_t dims[/*rank*/],
		  const hsize_t maxdims[/*rank*/])
{
    H5S_t	*space = NULL;
    int		i;
    hid_t	ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("i", "Is*[a0]h*[a0]h", rank, dims, maxdims);

    /* Check arguments */
    if(rank < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "dimensionality cannot be negative")
    if(rank > H5S_MAX_RANK)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "dimensionality is too large")

    /* We allow users to use this function to create scalar or null dataspace.
     * Check DIMS isn't set when the RANK is 0.
     */
    if(!dims && rank != 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid dataspace information")

    /* Check whether the current dimensions are valid */
    for(i = 0; i < rank; i++) {
        if(H5S_UNLIMITED == dims[i])
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "current dimension must have a specific size, not H5S_UNLIMITED")
        if(maxdims && H5S_UNLIMITED != maxdims[i] && maxdims[i]<dims[i])
            HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "maxdims is smaller than dims")
    } /* end for */

    /* Create the space and set the extent */
    if(NULL == (space = H5S_create_simple((unsigned)rank,dims,maxdims)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, FAIL, "can't create simple dataspace")

    /* Atomize */
    if((ret_value = H5I_register (H5I_DATASPACE, space, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to register dataspace ID")

done:
    if(ret_value < 0) {
        if(space && H5S_close(space) < 0)
            HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release dataspace")
    } /* end if */

    FUNC_LEAVE_API(ret_value)
} /* end H5Screate_simple() */


/*-------------------------------------------------------------------------
 * Function:	H5S_create_simple
 *
 * Purpose:	Internal function to create simple dataspace
 *
 * Return:	Success:	The ID for the new simple dataspace object.
 *		Failure:	Negative
 *
 * Errors:
 *
 * Programmer:	Quincey Koziol
 *		Thursday, April  3, 2003
 *
 * Modifications:
 *              Extracted from H5Screate_simple
 *              Quincey Koziol, Thursday, April  3, 2003
 *
 *-------------------------------------------------------------------------
 */
H5S_t *
H5S_create_simple(unsigned rank, const hsize_t dims[/*rank*/],
		  const hsize_t maxdims[/*rank*/])
{
    H5S_t       *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Check arguments */
    HDassert(rank <=H5S_MAX_RANK);

    /* Create the space and set the extent */
    if(NULL==(ret_value=H5S_create(H5S_SIMPLE)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCREATE, NULL, "can't create simple dataspace")
    if(H5S_set_extent_simple(ret_value,rank,dims,maxdims)<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, NULL, "can't set dimensions")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_create_simple() */


/*-------------------------------------------------------------------------
 * Function:	H5Sencode
 *
 * Purpose:	Given a dataspace ID, converts the object description
 *              (including selection) into binary in a buffer.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Raymond Lu
 *              slu@ncsa.uiuc.edu
 *              July 14, 2004
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Sencode(hid_t obj_id, void *buf, size_t *nalloc)
{
    H5S_t       *dspace;
    hid_t       temp_fapl_id = H5P_DEFAULT;
    herr_t      ret_value=SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*x*z", obj_id, buf, nalloc);

    /* Check argument and retrieve object */
    if (NULL == (dspace = (H5S_t *)H5I_object_verify(obj_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    /* Verify access property list and set up collective metadata if appropriate */
    if(H5CX_set_apl(&temp_fapl_id, H5P_CLS_FACC, H5I_INVALID_HID, TRUE) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTSET, H5I_INVALID_HID, "can't set access property list info")

    if(H5S_encode(dspace, (unsigned char **)&buf, nalloc)<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "can't encode dataspace")

done:
    FUNC_LEAVE_API(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5S_encode
 *
 * Purpose:	Private function for H5Sencode.  Converts an object
 *              description for dataspace and its selection into binary
 *              in a buffer.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Raymond Lu
 *              slu@ncsa.uiuc.edu
 *              July 14, 2004
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_encode(H5S_t *obj, unsigned char **p, size_t *nalloc)
{
    H5F_t       *f = NULL;      /* Fake file structure*/
    size_t      extent_size;    /* Size of serialized dataspace extent */
    hssize_t    sselect_size;   /* Signed size of serialized dataspace selection */
    size_t      select_size;    /* Size of serialized dataspace selection */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Allocate "fake" file structure */
    if(NULL == (f = H5F_fake_alloc((uint8_t)0)))
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTALLOC, FAIL, "can't allocate fake file struct")

    /* Find out the size of buffer needed for extent */
    if((extent_size = H5O_msg_raw_size(f, H5O_SDSPACE_ID, TRUE, obj)) == 0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_BADSIZE, FAIL, "can't find dataspace size")

    /* Find out the size of buffer needed for selection */
    if((sselect_size = H5S_SELECT_SERIAL_SIZE(obj)) < 0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_BADSIZE, FAIL, "can't find dataspace selection size")
    H5_CHECKED_ASSIGN(select_size, size_t, sselect_size, hssize_t);

    /* Verify the size of buffer.  If it's not big enough, simply return the
     * right size without filling the buffer. */
    if(!*p || *nalloc < (extent_size + select_size + 1 + 1 + 1 + 4))
        *nalloc = extent_size + select_size + 1 + 1 + 1 + 4;
    else {
        unsigned char *pp = (*p);   /* Local pointer for decoding */

        /* Encode the type of the information */
        *pp++ = H5O_SDSPACE_ID;

        /* Encode the version of the dataspace information */
        *pp++ = H5S_ENCODE_VERSION;

        /* Encode the "size of size" information */
        *pp++ = (unsigned char)H5F_SIZEOF_SIZE(f);

        /* Encode size of extent information. Pointer is actually moved in this macro. */
        UINT32ENCODE(pp, extent_size);

        /* Encode the extent part of dataspace */
        if(H5O_msg_encode(f, H5O_SDSPACE_ID, TRUE, pp, obj) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "can't encode extent space")
        pp += extent_size;

        /* Encode the selection part of dataspace.  */
        *p = pp;
        if(H5S_SELECT_SERIALIZE(obj, p) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTENCODE, FAIL, "can't encode select space")
    } /* end else */

done:
    /* Release fake file structure */
    if(f && H5F_fake_free(f) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, FAIL, "unable to release fake file struct")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_encode() */


/*-------------------------------------------------------------------------
 * Function:	H5Sdecode
 *
 * Purpose:	Decode a binary object description of dataspace and
 *              return a new object handle.
 *
 * Return:	Success:	dataspace ID(non-negative)
 *
 *		Failure:	negative
 *
 * Programmer:	Raymond Lu
 *              slu@ncsa.uiuc.edu
 *              July 14, 2004
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Sdecode(const void *buf)
{
    H5S_t       *ds;
    hid_t       ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "*x", buf);

    if(buf == NULL)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "empty buffer")

    if((ds = H5S_decode((const unsigned char **)&buf)) == NULL)
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, FAIL, "can't decode object")

    /* Register the type and return the ID */
    if((ret_value = H5I_register(H5I_DATASPACE, ds, TRUE)) < 0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTREGISTER, FAIL, "unable to register dataspace")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Sdecode() */


/*-------------------------------------------------------------------------
 * Function:	H5S_decode
 *
 * Purpose:	Private function for H5Sdecode.  Reconstructs a binary
 *              description of dataspace and returns a new object handle.
 *
 * Return:	Success:	dataspace ID(non-negative)
 *
 *		Failure:	negative
 *
 * Programmer:	Raymond Lu
 *              slu@ncsa.uiuc.edu
 *              July 14, 2004
 *
 *-------------------------------------------------------------------------
 */
H5S_t*
H5S_decode(const unsigned char **p)
{
    H5F_t       *f = NULL;              /* Fake file structure*/
    H5S_t       *ds;                    /* Decoded dataspace */
    H5S_extent_t *extent;               /* Entent of decoded dataspace */
    const unsigned char *pp = (*p);     /* Local pointer for decoding */
    size_t      extent_size;            /* size of the extent message*/
    uint8_t     sizeof_size;            /* 'Size of sizes' for file */
    H5S_t       *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Decode the type of the information */
    if(*pp++ != H5O_SDSPACE_ID)
	HGOTO_ERROR(H5E_DATASPACE, H5E_BADMESG, NULL, "not an encoded dataspace")

    /* Decode the version of the dataspace information */
    if(*pp++ != H5S_ENCODE_VERSION)
	HGOTO_ERROR(H5E_DATASPACE, H5E_VERSION, NULL, "unknown version of encoded dataspace")

    /* Decode the "size of size" information */
    sizeof_size = *pp++;

    /* Allocate "fake" file structure */
    if(NULL == (f = H5F_fake_alloc(sizeof_size)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTALLOC, NULL, "can't allocate fake file struct")

    /* Decode size of extent information */
    UINT32DECODE(pp, extent_size);

    /* Decode the extent part of dataspace */
    /* (pass mostly bogus file pointer and bogus DXPL) */
    if(NULL == (extent = (H5S_extent_t *)H5O_msg_decode(f, NULL, H5O_SDSPACE_ID, extent_size, pp)))
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, NULL, "can't decode object")
    pp += extent_size;

    /* Copy the extent into dataspace structure */
    if((ds = H5FL_CALLOC(H5S_t))==NULL)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for dataspace conversion path table")
    if(H5O_msg_copy(H5O_SDSPACE_ID, extent, &(ds->extent)) == NULL)
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOPY, NULL, "can't copy object")
    if(H5S_extent_release(extent) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTDELETE, NULL, "can't release previous dataspace")
    extent = H5FL_FREE(H5S_extent_t, extent);

    /* Initialize to "all" selection. Deserialization relies on valid existing selection. */
    if(H5S_select_all(ds, FALSE) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, NULL, "unable to set all selection")

    /* Decode the select part of dataspace.  I believe this part always exists. */
    *p = pp;
    if(H5S_SELECT_DESERIALIZE(&ds, p) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDECODE, NULL, "can't decode space selection")

    /* Set return value */
    ret_value = ds;

done:
    /* Release fake file structure */
    if(f && H5F_fake_free(f) < 0)
        HDONE_ERROR(H5E_DATASPACE, H5E_CANTRELEASE, NULL, "unable to release fake file struct")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_decode() */


/*-------------------------------------------------------------------------
 * Function:	H5S_get_simple_extent_type
 *
 * Purpose:	Internal function for retrieving the type of extent for a dataspace object
 *
 * Return:	Success:	The class of the dataspace object
 *
 *		Failure:	N5S_NO_CLASS
 *
 * Errors:
 *
 * Programmer:	Quincey Koziol
 *		Thursday, September 28, 2000
 *
 * Note:        This routine participates in the "Inlining C function pointers"
 *              pattern, don't call it directly, use the appropriate macro
 *              defined in H5Sprivate.h.
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5S_class_t
H5S_get_simple_extent_type(const H5S_t *space)
{
    H5S_class_t	ret_value = H5S_NO_CLASS;       /* Return value */

    FUNC_ENTER_NOAPI(H5S_NO_CLASS)

    HDassert(space);

    ret_value=H5S_GET_EXTENT_TYPE(space);

done:
    FUNC_LEAVE_NOAPI(ret_value)
}


/*-------------------------------------------------------------------------
 * Function:	H5Sget_simple_extent_type
 *
 * Purpose:	Retrieves the type of extent for a dataspace object
 *
 * Return:	Success:	The class of the dataspace object
 *
 *		Failure:	N5S_NO_CLASS
 *
 * Errors:
 *
 * Programmer:	Quincey Koziol
 *		Thursday, July 23, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
H5S_class_t
H5Sget_simple_extent_type(hid_t sid)
{
    H5S_t	*space;
    H5S_class_t	ret_value;

    FUNC_ENTER_API(H5S_NO_CLASS)
    H5TRACE1("Sc", "i", sid);

    /* Check arguments */
    if (NULL == (space = (H5S_t *)H5I_object_verify(sid, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, H5S_NO_CLASS, "not a dataspace")

    ret_value=H5S_GET_EXTENT_TYPE(space);

done:
    FUNC_LEAVE_API(ret_value)
}


/*--------------------------------------------------------------------------
 NAME
    H5Sset_extent_none
 PURPOSE
    Resets the extent of a dataspace back to "none"
 USAGE
    herr_t H5Sset_extent_none(space_id)
        hid_t space_id;	      IN: Dataspace object to reset
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
	This function resets the type of a dataspace back to "none" with no
    extent information stored for the dataspace.
--------------------------------------------------------------------------*/
herr_t
H5Sset_extent_none(hid_t space_id)
{
    H5S_t		   *space;	/* dataspace to modify */
    herr_t                  ret_value=SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", space_id);

    /* Check args */
    if (NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "not a dataspace")

    /* Clear the previous extent from the dataspace */
    if(H5S_extent_release(&space->extent)<0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTDELETE, FAIL, "can't release previous dataspace")

    space->extent.type=H5S_NO_CLASS;

done:
    FUNC_LEAVE_API(ret_value)
}   /* end H5Sset_extent_none() */


/*--------------------------------------------------------------------------
 NAME
    H5Soffset_simple
 PURPOSE
    Changes the offset of a selection within a simple dataspace extent
 USAGE
    herr_t H5Soffset_simple(space_id, offset)
        hid_t space_id;	        IN: Dataspace object to reset
        const hssize_t *offset; IN: Offset to position the selection at
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
	This function creates an offset for the selection within an extent, allowing
    the same shaped selection to be moved to different locations within a
    dataspace without requiring it to be re-defined.
--------------------------------------------------------------------------*/
herr_t
H5Soffset_simple(hid_t space_id, const hssize_t *offset)
{
    H5S_t		   *space;	/* dataspace to modify */
    herr_t                  ret_value=SUCCEED;  /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Hs", space_id, offset);

    /* Check args */
    if (NULL == (space = (H5S_t *)H5I_object_verify(space_id, H5I_DATASPACE)))
        HGOTO_ERROR(H5E_ATOM, H5E_BADATOM, FAIL, "not a dataspace")
    if (space->extent.rank==0 || (H5S_GET_EXTENT_TYPE(space)==H5S_SCALAR
            || H5S_GET_EXTENT_TYPE(space)==H5S_NULL))
        HGOTO_ERROR(H5E_ATOM, H5E_UNSUPPORTED, FAIL, "can't set offset on scalar or null dataspace")
    if (offset == NULL)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no offset specified")

    /* Set the selection offset */
    if(H5S_select_offset(space,offset)<0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTINIT, FAIL, "can't set offset")

done:
    FUNC_LEAVE_API(ret_value)
}   /* end H5Soffset_simple() */


/*-------------------------------------------------------------------------
 * Function:    H5S_set_extent
 *
 * Purpose:     Modify the dimensions of a dataspace. Based on H5S_extend
 *
 * Return:      Success: Non-negative
 *              Failure: Negative
 *
 * Programmer:  Pedro Vicente, pvn@ncsa.uiuc.edu
 *              March 13, 2002
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5S_set_extent(H5S_t *space, const hsize_t *size)
{
    unsigned u;                 /* Local index variable */
    htri_t ret_value = FALSE;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check args */
    HDassert(space && H5S_SIMPLE == H5S_GET_EXTENT_TYPE(space));
    HDassert(size);

    /* Verify that the dimensions being changed are allowed to change */
    for(u = 0; u < space->extent.rank; u++) {
        if(space->extent.size[u] != size[u]) {
            /* Check for invalid dimension size modification */
            if(space->extent.max && H5S_UNLIMITED != space->extent.max[u] &&
                     space->extent.max[u] < size[u])
                 HGOTO_ERROR(H5E_DATASPACE, H5E_BADVALUE, FAIL, "dimension cannot exceed the existing maximal size (new: %llu max: %llu)", (unsigned long long)size[u], (unsigned long long)space->extent.max[u])

            /* Indicate that dimension size can be modified */
            ret_value = TRUE;
        } /* end if */
    } /* end for */

    /* Update dimension size(s) */
    if(ret_value)
        if(H5S_set_extent_real(space, size) < 0)
             HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, FAIL, "failed to change dimension size(s)")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_set_extent() */


/*-------------------------------------------------------------------------
 * Function: H5S_has_extent
 *
 * Purpose: Determines if a simple dataspace's extent has been set (e.g.,
 *          by H5Sset_extent_simple() ).  Helps avoid write errors.
 *
 * Return: TRUE if dataspace has extent set
 *         FALSE if dataspace's extent is uninitialized
 *
 * Programmer: James Laird
 *
 * Date: July 23, 2004
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5S_has_extent(const H5S_t *ds)
{
    hbool_t ret_value = FALSE;          /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(ds);

    if(0 == ds->extent.rank && 0 == ds->extent.nelem && H5S_NULL != ds->extent.type)
        ret_value = FALSE;
    else
        ret_value = TRUE;

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_has_extent() */


/*-------------------------------------------------------------------------
 * Function:    H5S_set_extent_real
 *
 * Purpose:     Modify the dimensions of a dataspace. Based on H5S_extend
 *
 * Return:      Success: Non-negative
 *              Failure: Negative
 *
 * Programmer:  Pedro Vicente, pvn@ncsa.uiuc.edu
 *              March 13, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_set_extent_real(H5S_t *space, const hsize_t *size)
{
    hsize_t nelem;      /* Number of elements in extent */
    unsigned u;         /* Local index variable */
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check args */
    HDassert(space && H5S_SIMPLE == H5S_GET_EXTENT_TYPE(space));
    HDassert(size);

    /* Change the dataspace size & re-compute the number of elements in the extent */
    for(u = 0, nelem = 1; u < space->extent.rank; u++ ) {
        space->extent.size[u] = size[u];
        nelem *= size[u];
    } /* end for */
    space->extent.nelem = nelem;

    /* If the selection is 'all', update the number of elements selected */
    if(H5S_SEL_ALL == H5S_GET_SELECT_TYPE(space))
        if(H5S_select_all(space, FALSE) < 0)
            HGOTO_ERROR(H5E_DATASPACE, H5E_CANTDELETE, FAIL, "can't change selection")

    /* Mark the dataspace as no longer shared if it was before */
    if(H5O_msg_reset_share(H5O_SDSPACE_ID, space) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTRESET, FAIL, "can't stop sharing dataspace")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_set_extent_real() */


/*-------------------------------------------------------------------------
 * Function:	H5Sextent_equal
 *
 * Purpose:	Determines if two dataspace extents are equal.
 *
 * Return:	Success:	TRUE if equal, FALSE if unequal
 *
 *		Failure:	Negative
 *
 * Programmer:	Quincey Koziol
 *		Monday, October 24, 2005
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5Sextent_equal(hid_t space1_id, hid_t space2_id)
{
    const H5S_t	*ds1, *ds2;     /* Dataspaces to compare */
    htri_t	ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("t", "ii", space1_id, space2_id);

    /* check args */
    if(NULL == (ds1 = (const H5S_t *)H5I_object_verify(space1_id, H5I_DATASPACE)) ||
            NULL == (ds2 = (const H5S_t *)H5I_object_verify(space2_id, H5I_DATASPACE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a dataspace")

    /* Check dataspaces for extent's equality */
    if((ret_value = H5S_extent_equal(ds1, ds2)) < 0)
	HGOTO_ERROR(H5E_DATASPACE, H5E_CANTCOMPARE, FAIL, "dataspace comparison failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Sextent_equal() */


/*--------------------------------------------------------------------------
 NAME
    H5S_extent_equal
 PURPOSE
    Check if two dataspaces have equal extents
 USAGE
    htri_t H5S_extent_equal(ds1, ds2)
        H5S_t *ds1, *ds2;	        IN: Dataspace objects to compare
 RETURNS
     TRUE if equal, FALSE if unequal on succeess/Negative on failure
 DESCRIPTION
	Compare two dataspaces if their extents are identical.
--------------------------------------------------------------------------*/
htri_t
H5S_extent_equal(const H5S_t *ds1, const H5S_t *ds2)
{
    unsigned u;                 /* Local index variable */
    htri_t ret_value = TRUE;    /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check args */
    HDassert(ds1);
    HDassert(ds2);

    /* Make certain the dataspaces are the same type */
    if(ds1->extent.type != ds2->extent.type)
        HGOTO_DONE(FALSE)

    /* Make certain the dataspaces are the same rank */
    if(ds1->extent.rank != ds2->extent.rank)
        HGOTO_DONE(FALSE)

    /* Make certain the dataspaces' current dimensions are the same size */
    if(ds1->extent.rank > 0) {
        HDassert(ds1->extent.size);
        HDassert(ds2->extent.size);
        for(u = 0; u < ds1->extent.rank; u++)
            if(ds1->extent.size[u] != ds2->extent.size[u])
                HGOTO_DONE(FALSE)
    } /* end if */

    /* Make certain the dataspaces' maximum dimensions are the same size */
    if(ds1->extent.rank > 0) {
        /* Check for no maximum dimensions on dataspaces */
        if(ds1->extent.max != NULL && ds2->extent.max != NULL) {
            for(u = 0; u < ds1->extent.rank; u++)
                if(ds1->extent.max[u] != ds2->extent.max[u])
                    HGOTO_DONE(FALSE)
        } /* end if */
        else
            if((ds1->extent.max == NULL && ds2->extent.max != NULL) ||
                    (ds1->extent.max != NULL && ds2->extent.max == NULL))
                HGOTO_DONE(FALSE)
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5S_extent_equal() */


/*-------------------------------------------------------------------------
 * Function:	H5S_extent_nelem
 *
 * Purpose:	Determines how many elements a dataset extent describes.
 *
 * Return:	Success:	Number of data points in the dataset extent.
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *		Thursday, November 30, 2006
 *
 *-------------------------------------------------------------------------
 */
hsize_t
H5S_extent_nelem(const H5S_extent_t *ext)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check args */
    HDassert(ext);

    /* Return the number of elements in extent */
    FUNC_LEAVE_NOAPI(ext->nelem)
} /* end H5S_extent_nelem() */


/*-------------------------------------------------------------------------
 * Function:    H5S_set_version
 *
 * Purpose:     Set the version to encode a dataspace with.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi; December 2017
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5S_set_version(H5F_t *f, H5S_t *ds)
{
    unsigned version;           /* Message version */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(f);
    HDassert(ds);

    /* Upgrade to the version indicated by the file's low bound if higher */
    version = MAX(ds->extent.version, H5O_sdspace_ver_bounds[H5F_LOW_BOUND(f)]);

    /* Version bounds check */
    if(version > H5O_sdspace_ver_bounds[H5F_HIGH_BOUND(f)])
        HGOTO_ERROR(H5E_DATASET, H5E_BADRANGE, FAIL, "Dataspace version out of bounds")

    /* Set the message version */
    ds->extent.version = version;

done:
    FUNC_LEAVE_NOAPI(ret_value)

} /* end H5S_set_version() */


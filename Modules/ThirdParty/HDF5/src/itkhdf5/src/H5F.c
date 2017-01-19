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

/****************/
/* Module Setup */
/****************/

#define H5F_PACKAGE		/*suppress error about including H5Fpkg	  */

/* Interface initialization */
#define H5_INTERFACE_INIT_FUNC	H5F__init_pub_interface


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Aprivate.h"		/* Attributes				*/
#include "H5ACprivate.h"        /* Metadata cache                       */
#include "H5Dprivate.h"		/* Datasets				*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access				*/
#include "H5FDprivate.h"	/* File drivers				*/
#include "H5Gprivate.h"		/* Groups				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Pprivate.h"		/* Property lists			*/
#include "H5SMprivate.h"	/* Shared Object Header Messages	*/
#include "H5Tprivate.h"		/* Datatypes				*/


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


/* File ID class */
static const H5I_class_t H5I_FILE_CLS[1] = {{
    H5I_FILE,			/* ID class value */
    H5I_CLASS_REUSE_IDS,	/* Class flags */
    0,				/* # of reserved IDs for class */
    (H5I_free_t)H5F_close	/* Callback routine for closing objects of this class */
}};


/*--------------------------------------------------------------------------
NAME
   H5F__init_pub_interface -- Initialize interface-specific information
USAGE
    herr_t H5F__init_pub_interface()
RETURNS
    Non-negative on success/Negative on failure
DESCRIPTION
    Initializes any interface-specific data or routines.  (Just calls
    H5F_init() currently).

--------------------------------------------------------------------------*/
static herr_t
H5F__init_pub_interface(void)
{
    herr_t          ret_value                = SUCCEED;   /* Return value */
    FUNC_ENTER_STATIC

    /*
     * Initialize the atom group for the file IDs.
     */
    if(H5I_register_type(H5I_FILE_CLS) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to initialize interface")

    ret_value = H5F_init();
done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5F__init_pub_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5F_init
 *
 * Purpose:	Initialize the interface from some other layer.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Robb Matzke
 *              Wednesday, December 16, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5F_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5F_init() */


/*-------------------------------------------------------------------------
 * Function:	H5F_term_interface
 *
 * Purpose:	Terminate this interface: free all memory and reset global
 *		variables to their initial values.  Release all ID groups
 *		associated with this interface.
 *
 * Return:	Success:	Positive if anything was done that might
 *				have affected other interfaces; zero
 *				otherwise.
 *
 *		Failure:        Never fails.
 *
 * Programmer:	Robb Matzke
 *              Friday, February 19, 1999
 *
 *-------------------------------------------------------------------------
 */
int
H5F_term_interface(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_interface_initialize_g) {
	if(H5I_nmembers(H5I_FILE) > 0) {
            (void)H5I_clear_type(H5I_FILE, FALSE, FALSE);
            n++; /*H5I*/
	} /* end if */
        else {
            /* Make certain we've cleaned up all the shared file objects */
            H5F_sfile_assert_num(0);

            /* Destroy the file object id group */
	    (void)H5I_dec_type_ref(H5I_FILE);
            n++; /*H5I*/

	    /* Mark closed */
	    H5_interface_initialize_g = 0;
	} /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5F_term_interface() */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_create_plist
 *
 * Purpose:	Get an atom for a copy of the file-creation property list for
 *		this file. This function returns an atom with a copy of the
 *		properties used to create a file.
 *
 * Return:	Success:	template ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fget_create_plist(hid_t file_id)
{
    H5F_t *file;                /* File info */
    H5P_genplist_t *plist;      /* Property list */
    hid_t ret_value;            /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", file_id);

    /* check args */
    if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")
    if(NULL == (plist = (H5P_genplist_t *)H5I_object(file->shared->fcpl_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a property list")

    /* Create the property list object to return */
    if((ret_value = H5P_copy_plist(plist, TRUE)) < 0)
	HGOTO_ERROR(H5E_INTERNAL, H5E_CANTINIT, FAIL, "unable to copy file creation properties")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_create_plist() */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_access_plist
 *
 * Purpose:	Returns a copy of the file access property list of the
 *		specified file.
 *
 *              NOTE: Make sure that, if you are going to overwrite
 *              information in the copied property list that was
 *              previously opened and assigned to the property list, then
 *              you must close it before overwriting the values.
 *
 * Return:	Success:	Object ID for a copy of the file access
 *				property list.
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Wednesday, February 18, 1998
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fget_access_plist(hid_t file_id)
{
    H5F_t *f;           /* File info */
    hid_t ret_value;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", file_id);

    /* Check args */
    if(NULL == (f = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* Retrieve the file's access property list */
    if((ret_value = H5F_get_access_plist(f, TRUE)) < 0)
        HGOTO_ERROR(H5E_PLIST, H5E_CANTGET, FAIL, "can't get file access property list")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_access_plist() */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_obj_count
 *
 * Purpose:	Public function returning the number of opened object IDs
 *		(files, datasets, groups and datatypes) in the same file.
 *
 * Return:	Non-negative on success; negative on failure.
 *
 * Programmer:	Raymond Lu
 *		Wednesday, Dec 5, 2001
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_obj_count(hid_t file_id, unsigned types)
{
    H5F_t    *f = NULL;         /* File to query */
    size_t  obj_count = 0;      /* Number of opened objects */
    ssize_t  ret_value;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("Zs", "iIu", file_id, types);

    /* Check arguments */
    if(file_id != (hid_t)H5F_OBJ_ALL && (NULL == (f = (H5F_t *)H5I_object_verify(file_id, H5I_FILE))))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file id")
    if(0 == (types & H5F_OBJ_ALL))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not an object type")

    /* Perform the query */
    if(H5F_get_obj_count(f, types, TRUE, &obj_count) < 0)
	HGOTO_ERROR(H5E_INTERNAL, H5E_BADITER, FAIL, "H5F_get_obj_count failed")

    /* Set the return value */
    ret_value = (ssize_t)obj_count;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_obj_count() */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_object_ids
 *
 * Purpose:	Public function to return a list of opened object IDs.
 *
 * Return:	Non-negative on success; negative on failure.
 *
 * Programmer:  Raymond Lu
 *              Wednesday, Dec 5, 2001
 *
 * Modification:
 *              Raymond Lu
 *              24 September 2008
 *              Changed the return value to ssize_t and MAX_OBJTS to size_t to
 *              accommadate potential large number of objects.
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_obj_ids(hid_t file_id, unsigned types, size_t max_objs, hid_t *oid_list)
{
    H5F_t    *f = NULL;         /* File to query */
    size_t    obj_id_count = 0; /* Number of open objects */
    ssize_t   ret_value;        /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("Zs", "iIuz*i", file_id, types, max_objs, oid_list);

    /* Check arguments */
    if(file_id != (hid_t)H5F_OBJ_ALL && (NULL == (f = (H5F_t *)H5I_object_verify(file_id, H5I_FILE))))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file id")
    if(0 == (types & H5F_OBJ_ALL))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not an object type")
    if(!oid_list)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "object ID list is NULL")

    /* Perform the query */
    if(H5F_get_obj_ids(f, types, max_objs, oid_list, TRUE, &obj_id_count) < 0)
        HGOTO_ERROR(H5E_INTERNAL, H5E_BADITER, FAIL, "H5F_get_obj_ids failed")

    /* Set the return value */
    ret_value = (ssize_t)obj_id_count;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_obj_ids() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_vfd_handle
 *
 * Purpose:     Returns a pointer to the file handle of the low-level file
 *              driver.
 *
 * Return:      Success:        non-negative value.
 *              Failure:        negative.
 *
 * Programmer:  Raymond Lu
 *              Sep. 16, 2002
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_vfd_handle(hid_t file_id, hid_t fapl, void **file_handle)
{
    H5F_t               *file;          /* File to query */
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "ii**x", file_id, fapl, file_handle);

    /* Check args */
    if(!file_handle)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file handle pointer")

    /* Get the file */
    if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file id")

    /* Retrieve the VFD handle for the file */
    if(H5F_get_vfd_handle(file, fapl, file_handle) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "can't retrieve VFD handle")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_vfd_handle() */


/*-------------------------------------------------------------------------
 * Function:	H5Fis_hdf5
 *
 * Purpose:	Check the file signature to detect an HDF5 file.
 *
 * Bugs:	This function is not robust: it only uses the default file
 *		driver when attempting to open the file when in fact it
 *		should use all known file drivers.
 *
 * Return:	Success:	TRUE/FALSE
 *
 *		Failure:	Negative
 *
 * Programmer:	Unknown
 *
 * Modifications:
 *		Robb Matzke, 1999-08-02
 *		Rewritten to use the virtual file layer.
 *-------------------------------------------------------------------------
 */
htri_t
H5Fis_hdf5(const char *name)
{
    htri_t      ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("t", "*s", name);

    /* Check args and all the boring stuff. */
    if(!name || !*name)
        HGOTO_ERROR(H5E_ARGS, H5E_BADRANGE, FAIL, "no file name specified")

    /* call the private is_HDF5 function */
    if((ret_value = H5F_is_hdf5(name)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_NOTHDF5, FAIL, "unable open file")

done:

    FUNC_LEAVE_API(ret_value)
} /* end H5Fis_hdf5() */


/*-------------------------------------------------------------------------
 * Function:	H5Fcreate
 *
 * Purpose:	This is the primary function for creating HDF5 files . The
 *		flags parameter determines whether an existing file will be
 *		overwritten or not.  All newly created files are opened for
 *		both reading and writing.  All flags may be combined with the
 *		bit-wise OR operator (`|') to change the behavior of the file
 *		create call.
 *
 *		The more complex behaviors of a file's creation and access
 *		are controlled through the file-creation and file-access
 *		property lists.  The value of H5P_DEFAULT for a template
 *		value indicates that the library should use the default
 *		values for the appropriate template.
 *
 * See also:	H5Fpublic.h for the list of supported flags. H5Ppublic.h for
 * 		the list of file creation and file access properties.
 *
 * Return:	Success:	A file ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Fcreate(const char *filename, unsigned flags, hid_t fcpl_id, hid_t fapl_id)
{
    H5F_t	*new_file = NULL;	/*file struct for new file	*/
    hid_t	ret_value;	        /*return value			*/

    FUNC_ENTER_API(FAIL)
    H5TRACE4("i", "*sIuii", filename, flags, fcpl_id, fapl_id);

    /* Check/fix arguments */
    if(!filename || !*filename)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file name")
    /* In this routine, we only accept the following flags:
     *          H5F_ACC_EXCL and H5F_ACC_TRUNC
     */
    if(flags & ~(H5F_ACC_EXCL | H5F_ACC_TRUNC))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid flags")
    /* The H5F_ACC_EXCL and H5F_ACC_TRUNC flags are mutually exclusive */
    if((flags & H5F_ACC_EXCL) && (flags & H5F_ACC_TRUNC))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "mutually exclusive flags for file creation")

    /* Check file creation property list */
    if(H5P_DEFAULT == fcpl_id)
        fcpl_id = H5P_FILE_CREATE_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fcpl_id, H5P_FILE_CREATE))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file create property list")

    /* Check the file access property list */
    if(H5P_DEFAULT == fapl_id)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file access property list")

    /*
     * Adjust bit flags by turning on the creation bit and making sure that
     * the EXCL or TRUNC bit is set.  All newly-created files are opened for
     * reading and writing.
     */
    if (0==(flags & (H5F_ACC_EXCL|H5F_ACC_TRUNC)))
	flags |= H5F_ACC_EXCL;	 /*default*/
    flags |= H5F_ACC_RDWR | H5F_ACC_CREAT;

    /*
     * Create a new file or truncate an existing file.
     */
    if(NULL == (new_file = H5F_open(filename, flags, fcpl_id, fapl_id, H5AC_dxpl_id)))
	HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to create file")

    /* Get an atom for the file */
    if((ret_value = H5I_register(H5I_FILE, new_file, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file")

    /* Keep this ID in file object structure */
    new_file->file_id = ret_value;

done:
    if(ret_value < 0 && new_file)
        if(H5F_close(new_file) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "problems closing file")

    FUNC_LEAVE_API(ret_value)
} /* end H5Fcreate() */


/*-------------------------------------------------------------------------
 * Function:	H5Fopen
 *
 * Purpose:	This is the primary function for accessing existing HDF5
 *		files.  The FLAGS argument determines whether writing to an
 *		existing file will be allowed or not.  All flags may be
 *		combined with the bit-wise OR operator (`|') to change the
 *		behavior of the file open call.  The more complex behaviors
 *		of a file's access are controlled through the file-access
 *		property list.
 *
 * See Also:	H5Fpublic.h for a list of possible values for FLAGS.
 *
 * Return:	Success:	A file ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Unknown
 *
 * Modifications:
 *	  	Robb Matzke, 1997-07-18
 *		File struct creation and destruction is through H5F_new() and
 *		H5F_dest(). Reading the root symbol table entry is done with
 *		H5G_decode().
 *
 *  		Robb Matzke, 1997-09-23
 *		Most of the work is now done by H5F_open() since H5Fcreate()
 *		and H5Fopen() originally contained almost identical code.
 *
 *	 	Robb Matzke, 1998-02-18
 *		Added better error checking for the flags and the file access
 *		property list.  It used to be possible to make the library
 *		dump core by passing an object ID that was not a file access
 *		property list.
 *
 * 		Robb Matzke, 1999-08-02
 *		The file access property list is passed to the H5F_open() as
 *		object IDs.
 *-------------------------------------------------------------------------
 */
hid_t
H5Fopen(const char *filename, unsigned flags, hid_t fapl_id)
{
    H5F_t	*new_file = NULL;	/*file struct for new file	*/
    hid_t	ret_value;	        /*return value			*/

    FUNC_ENTER_API(FAIL)
    H5TRACE3("i", "*sIui", filename, flags, fapl_id);

    /* Check/fix arguments. */
    if(!filename || !*filename)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file name")
    /* Reject undefined flags (~H5F_ACC_PUBLIC_FLAGS) and the H5F_ACC_TRUNC & H5F_ACC_EXCL flags */
    if((flags & ~H5F_ACC_PUBLIC_FLAGS) ||
            (flags & H5F_ACC_TRUNC) || (flags & H5F_ACC_EXCL))
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid file open flags")
    if(H5P_DEFAULT == fapl_id)
        fapl_id = H5P_FILE_ACCESS_DEFAULT;
    else
        if(TRUE != H5P_isa_class(fapl_id, H5P_FILE_ACCESS))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not file access property list")

    /* Open the file */
    if(NULL == (new_file = H5F_open(filename, flags, H5P_FILE_CREATE_DEFAULT, fapl_id, H5AC_dxpl_id)))
	HGOTO_ERROR(H5E_FILE, H5E_CANTOPENFILE, FAIL, "unable to open file")

    /* Get an atom for the file */
    if((ret_value = H5I_register(H5I_FILE, new_file, TRUE)) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

    /* Keep this ID in file object structure */
    new_file->file_id = ret_value;

done:
    if(ret_value < 0 && new_file && H5F_try_close(new_file) < 0)
        HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "problems closing file")

    FUNC_LEAVE_API(ret_value)
} /* end H5Fopen() */


/*-------------------------------------------------------------------------
 * Function:	H5Fflush
 *
 * Purpose:	Flushes all outstanding buffers of a file to disk but does
 *		not remove them from the cache.  The OBJECT_ID can be a file,
 *		dataset, group, attribute, or named data type.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Thursday, August  6, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fflush(hid_t object_id, H5F_scope_t scope)
{
    H5F_t	*f = NULL;              /* File to flush */
    H5O_loc_t	*oloc = NULL;           /* Object location for ID */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "iFs", object_id, scope);

    switch(H5I_get_type(object_id)) {
        case H5I_FILE:
            if(NULL == (f = (H5F_t *)H5I_object(object_id)))
                HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
            break;

        case H5I_GROUP:
            {
                H5G_t	*grp;

                if(NULL == (grp = (H5G_t *)H5I_object(object_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid group identifier")
                oloc = H5G_oloc(grp);
            }
            break;

        case H5I_DATATYPE:
            {
                H5T_t	*type;

                if(NULL == (type = (H5T_t *)H5I_object(object_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid type identifier")
                oloc = H5T_oloc(type);
            }
            break;

        case H5I_DATASET:
            {
                H5D_t	*dset;

                if(NULL == (dset = (H5D_t *)H5I_object(object_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid dataset identifier")
                oloc = H5D_oloc(dset);
            }
            break;

        case H5I_ATTR:
            {
                H5A_t	*attr;

                if(NULL == (attr = (H5A_t *)H5I_object(object_id)))
                    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid attribute identifier")
                oloc = H5A_oloc(attr);
            }
            break;

        case H5I_UNINIT:
        case H5I_BADID:
        case H5I_DATASPACE:
        case H5I_REFERENCE:
        case H5I_VFL:
        case H5I_GENPROP_CLS:
        case H5I_GENPROP_LST:
        case H5I_ERROR_CLASS:
        case H5I_ERROR_MSG:
        case H5I_ERROR_STACK:
        case H5I_NTYPES:
        default:
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file or file object")
    } /* end switch */

    if(!f) {
	if(!oloc)
	    HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "object is not assocated with a file")
	f = oloc->file;
    } /* end if */
    if(!f)
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "object is not associated with a file")

    /* Flush the file */
    /*
     * Nothing to do if the file is read only.	This determination is
     * made at the shared open(2) flags level, implying that opening a
     * file twice, once for read-only and once for read-write, and then
     * calling H5Fflush() with the read-only handle, still causes data
     * to be flushed.
     */
    if(H5F_ACC_RDWR & H5F_INTENT(f)) {
        /* Flush other files, depending on scope */
        if(H5F_SCOPE_GLOBAL == scope) {
            /* Call the flush routine for mounted file hierarchies */
            if(H5F_flush_mounts(f, H5AC_dxpl_id) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush mounted file hierarchy")
        } /* end if */
        else {
            /* Call the flush routine, for this file */
            if(H5F_flush(f, H5AC_dxpl_id, FALSE) < 0)
                HGOTO_ERROR(H5E_FILE, H5E_CANTFLUSH, FAIL, "unable to flush file's cached information")
        } /* end else */
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fflush() */


/*-------------------------------------------------------------------------
 * Function:	H5Fclose
 *
 * Purpose:	This function closes the file specified by FILE_ID by
 *		flushing all data to storage, and terminating access to the
 *		file through FILE_ID.  If objects (e.g., datasets, groups,
 *		etc.) are open in the file then the underlying storage is not
 *		closed until those objects are closed; however, all data for
 *		the file and the open objects is flushed.
 *
 * Return:	Success:	Non-negative
 *
 *		Failure:	Negative
 *
 * Programmer:	Robb Matzke
 *              Saturday, February 20, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fclose(hid_t file_id)
{
    H5F_t       *f = NULL;
    int         nref;
    herr_t	ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* Check/fix arguments. */
    if(H5I_FILE != H5I_get_type(file_id))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file ID")

    /* Flush file if this is the last reference to this id and we have write
     * intent, unless it will be flushed by the "shared" file being closed.
     * This is only necessary to replicate previous behaviour, and could be
     * disabled by an option/property to improve performance. */
    if(NULL == (f = (H5F_t *)H5I_object(file_id)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "invalid file identifier")
    if((f->shared->nrefs > 1) && (H5F_INTENT(f) & H5F_ACC_RDWR)) {
        if((nref = H5I_get_ref(file_id, FALSE)) < 0)
            HGOTO_ERROR(H5E_ATOM, H5E_CANTGET, FAIL, "can't get ID ref count")
        if(nref == 1)
            if(H5F_flush(f, H5AC_dxpl_id, FALSE) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "unable to flush cache")
    } /* end if */

    /*
     * Decrement reference count on atom.  When it reaches zero the file will
     * be closed.
     */
    if(H5I_dec_app_ref(file_id) < 0)
	HGOTO_ERROR(H5E_ATOM, H5E_CANTCLOSEFILE, FAIL, "decrementing file ID failed")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fclose() */


/*-------------------------------------------------------------------------
 * Function:	H5Freopen
 *
 * Purpose:	Reopen a file.  The new file handle which is returned points
 *		to the same file as the specified file handle.  Both handles
 *		share caches and other information.  The only difference
 *		between the handles is that the new handle is not mounted
 *		anywhere and no files are mounted on it.
 *
 * Return:	Success:	New file ID
 *
 *		Failure:	FAIL
 *
 * Programmer:	Robb Matzke
 *              Friday, October 16, 1998
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Freopen(hid_t file_id)
{
    H5F_t	*old_file = NULL;
    H5F_t	*new_file = NULL;
    hid_t	ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE1("i", "i", file_id);

    /* Check arguments */
    if(NULL == (old_file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

    /* Get a new "top level" file struct, sharing the same "low level" file struct */
    if(NULL == (new_file = H5F_new(old_file->shared, 0, H5P_FILE_CREATE_DEFAULT, H5P_FILE_ACCESS_DEFAULT, NULL)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTINIT, FAIL, "unable to reopen file")

    /* Duplicate old file's names */
    new_file->open_name = H5MM_xstrdup(old_file->open_name);
    new_file->actual_name = H5MM_xstrdup(old_file->actual_name);
    new_file->extpath = H5MM_xstrdup(old_file->extpath);

    if((ret_value = H5I_register(H5I_FILE, new_file, TRUE)) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTREGISTER, FAIL, "unable to atomize file handle")

    /* Keep this ID in file object structure */
    new_file->file_id = ret_value;

done:
    if(ret_value < 0 && new_file)
        if(H5F_dest(new_file, H5AC_dxpl_id, FALSE) < 0)
            HDONE_ERROR(H5E_FILE, H5E_CANTCLOSEFILE, FAIL, "can't close file")

    FUNC_LEAVE_API(ret_value)
} /* end H5Freopen() */


/*-------------------------------------------------------------------------
 * Function:	H5Fget_intent
 *
 * Purpose:	Public API to retrieve the file's 'intent' flags passed
 *              during H5Fopen()
 *
 * Return:	Non-negative on success/negative on failure
 *
 * Programmer:	James Laird
 *		August 23, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_intent(hid_t file_id, unsigned *intent_flags)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Iu", file_id, intent_flags);

    /* If no intent flags were passed in, exit quietly */
    if(intent_flags) {
        H5F_t * file;           /* Pointer to file structure */

        /* Get the internal file structure */
        if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")

        /* HDF5 uses some flags internally that users don't know about.
         * Simplify things for them so that they only get either H5F_ACC_RDWR
         * or H5F_ACC_RDONLY.
         */
        if(H5F_INTENT(file) & H5F_ACC_RDWR)
            *intent_flags = H5F_ACC_RDWR;
        else
            *intent_flags = H5F_ACC_RDONLY;
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_intent() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_freespace
 *
 * Purpose:     Retrieves the amount of free space in the file.
 *
 * Return:      Success:        Amount of free space for type
 *              Failure:        Negative
 *
 * Programmer:  Quincey Koziol
 *              koziol@ncsa.uiuc.edu
 *              Oct  6, 2003
 *
 *-------------------------------------------------------------------------
 */
hssize_t
H5Fget_freespace(hid_t file_id)
{
    H5F_t      *file;           /* File object for file ID */
    hsize_t	tot_space;	/* Amount of free space in the file */
    hssize_t    ret_value;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("Hs", "i", file_id);

    /* Check args */
    if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Go get the actual amount of free space in the file */
    if(H5MF_get_freespace(file, H5AC_ind_dxpl_id, &tot_space, NULL) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to check free space for file")

    ret_value = (hssize_t)tot_space;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_freespace() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_filesize
 *
 * Purpose:     Retrieves the file size of the HDF5 file. This function
 *              is called after an existing file is opened in order
 *		to learn the true size of the underlying file.
 *
 * Return:      Success:        Non-negative
 *              Failure:        Negative
 *
 * Programmer:  David Pitt
 *              david.pitt@bigpond.com
 *              Apr 27, 2004
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_filesize(hid_t file_id, hsize_t *size)
{
    H5F_t       *file;                  /* File object for file ID */
    haddr_t     eof;                    /* End of file address */
    haddr_t     base_addr;              /* Base address for the file */
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*h", file_id, size);

    /* Check args */
    if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Go get the actual file size */
    if(HADDR_UNDEF == (eof = H5FD_get_eof(file->shared->lf)))
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to get file size")
    base_addr = H5FD_get_base_addr(file->shared->lf);

    if(size)
        *size = (hsize_t)(eof + base_addr);     /* Convert relative base address for file to absolute address */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_filesize() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_file_image
 *
 * Purpose:     If a buffer is provided (via the buf_ptr argument) and is 
 *		big enough (size in buf_len argument), load *buf_ptr with
 *		an image of the open file whose ID is provided in the 
 *		file_id parameter, and return the number of bytes copied
 *		to the buffer.
 *
 *		If the buffer exists, but is too small to contain an image
 *		of the indicated file, return a negative number.
 *
 *		Finally, if no buffer is provided, return the size of the 
 *		buffer needed.  This value is simply the eoa of the target 
 *		file.
 *
 *		Note that any user block is skipped.
 *
 *		Also note that the function may not be used on files 
 *		opened with either the split/multi file driver or the
 *		family file driver.
 *
 *		In the former case, the sparse address space makes the 
 *		get file image operation impractical, due to the size of
 *		the image typically required.
 *
 *		In the case of the family file driver, the problem is
 *		the driver message in the super block, which will prevent
 *		the image being opened with any driver other than the
 *		family file driver -- which negates the purpose of the 
 *		operation.  This can be fixed, but no resources for 
 *		this now.
 *
 * Return:      Success:        Bytes copied / number of bytes needed.
 *              Failure:        negative value
 *
 * Programmer:  John Mainzer
 *              11/15/11
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_file_image(hid_t file_id, void *buf_ptr, size_t buf_len)
{
    H5F_t      *file;                   /* File object for file ID */
    ssize_t     ret_value;              /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("Zs", "i*xz", file_id, buf_ptr, buf_len);

    /* Check args */
    if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* call private get_file_image function */
    if((ret_value = H5F_get_file_image(file, buf_ptr, buf_len)) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "unable to get file image")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_file_image() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_config
 *
 * Purpose:     Retrieves the current automatic cache resize configuration
 *		from the metadata cache, and return it in *config_ptr.
 *
 *		Note that the version field of *config_Ptr must be correctly
 *		filled in by the caller.  This allows us to adapt for
 *		obsolete versions of the structure.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_mdc_config(hid_t file_id, H5AC_cache_config_t *config_ptr)
{
    H5F_t      *file;                   /* File object for file ID */
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", file_id, config_ptr);

    /* Check args */
    if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")
    if((NULL == config_ptr) || (config_ptr->version != H5AC__CURR_CACHE_CONFIG_VERSION))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "Bad config_ptr")

    /* Go get the resize configuration */
    if(H5AC_get_cache_auto_resize_config(file->shared->cache, config_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_auto_resize_config() failed.")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_mdc_config() */


/*-------------------------------------------------------------------------
 * Function:    H5Fset_mdc_config
 *
 * Purpose:     Sets the current metadata cache automatic resize
 *		configuration, using the contents of the instance of
 *		H5AC_cache_config_t pointed to by config_ptr.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fset_mdc_config(hid_t file_id, H5AC_cache_config_t *config_ptr)
{
    H5F_t      *file;                   /* File object for file ID */
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", file_id, config_ptr);

    /* Check args */
    if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
         HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* set the resize configuration  */
    if(H5AC_set_cache_auto_resize_config(file->shared->cache, config_ptr) < 0)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "H5AC_set_cache_auto_resize_config() failed.")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fset_mdc_config() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_hit_rate
 *
 * Purpose:     Retrieves the current hit rate from the metadata cache.
 *		This rate is the overall hit rate since the last time
 *		the hit rate statistics were reset either manually or
 *		automatically.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_mdc_hit_rate(hid_t file_id, double *hit_rate_ptr)
{
    H5F_t      *file;                   /* File object for file ID */
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*d", file_id, hit_rate_ptr);

    /* Check args */
    if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    if(NULL == hit_rate_ptr)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "NULL hit rate pointer")

    /* Go get the current hit rate */
    if(H5AC_get_cache_hit_rate(file->shared->cache, hit_rate_ptr) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_hit_rate() failed.")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_mdc_hit_rate() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_mdc_size
 *
 * Purpose:     Retrieves the maximum size, minimum clean size, current
 *		size, and current number of entries from the metadata
 *		cache associated with the specified file.  If any of
 *		the ptr parameters are NULL, the associated datum is
 *		not returned.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_mdc_size(hid_t file_id, size_t *max_size_ptr, size_t *min_clean_size_ptr,
    size_t *cur_size_ptr, int *cur_num_entries_ptr)
{
    H5F_t      *file;                   /* File object for file ID */
    int32_t    cur_num_entries;
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE5("e", "i*z*z*z*Is", file_id, max_size_ptr, min_clean_size_ptr,
             cur_size_ptr, cur_num_entries_ptr);

    /* Check args */
    if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
         HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Go get the size data */
    if(H5AC_get_cache_size(file->shared->cache, max_size_ptr,
            min_clean_size_ptr, cur_size_ptr, &cur_num_entries) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "H5AC_get_cache_size() failed.")

    if(cur_num_entries_ptr != NULL)
	*cur_num_entries_ptr = (int)cur_num_entries;

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Fget_mdc_size() */


/*-------------------------------------------------------------------------
 * Function:    H5Freset_mdc_hit_rate_stats
 *
 * Purpose:     Reset the hit rate statistic whose current value can
 *		be obtained via the H5Fget_mdc_hit_rate() call.  Note
 *		that this statistic will also be reset once per epoch
 *		by the automatic cache resize code if it is enabled.
 *
 *		It is probably a bad idea to call this function unless
 *		you are controlling cache size from your program instead
 *		of using our cache size control code.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              3/24/05
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Freset_mdc_hit_rate_stats(hid_t file_id)
{
    H5F_t      *file;                   /* File object for file ID */
    herr_t     ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* Check args */
    if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
         HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Reset the hit rate statistic */
    if(H5AC_reset_cache_hit_rate_stats(file->shared->cache) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "can't reset cache hit rate")

done:
    FUNC_LEAVE_API(ret_value)
} /* H5Freset_mdc_hit_rate_stats() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_name
 *
 * Purpose:     Gets the name of the file to which object OBJ_ID belongs.
 *              If `name' is non-NULL then write up to `size' bytes into that
 *              buffer and always return the length of the entry name.
 *              Otherwise `size' is ignored and the function does not store the name,
 *              just returning the number of characters required to store the name.
 *              If an error occurs then the buffer pointed to by `name' (NULL or non-NULL)
 *              is unchanged and the function returns a negative value.
 *
 * Note:	This routine returns the name that was used to open the file,
 *		not the actual name after resolving symlinks, etc.
 *
 * Return:      Success:        The length of the file name
 *              Failure:        Negative
 *
 * Programmer:  Raymond Lu
 *              June 29, 2004
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Fget_name(hid_t obj_id, char *name/*out*/, size_t size)
{
    H5F_t         *f;           /* Top file in mount hierarchy */
    size_t        len;
    ssize_t       ret_value;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("Zs", "ixz", obj_id, name, size);

    /* For file IDs, get the file object directly */
    /* (This prevents the H5G_loc() call from returning the file pointer for
     * the top file in a mount hierarchy)
     */
    if(H5I_get_type(obj_id) == H5I_FILE ) {
        if(NULL == (f = (H5F_t *)H5I_object(obj_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")
    } /* end if */
    else {
        H5G_loc_t     loc;        /* Object location */

        /* Get symbol table entry */
        if(H5G_loc(obj_id, &loc) < 0)
             HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid object ID")
        f = loc.oloc->file;
    } /* end else */

    len = HDstrlen(H5F_OPEN_NAME(f));

    if(name) {
        HDstrncpy(name, H5F_OPEN_NAME(f), MIN(len + 1,size));
        if(len >= size)
            name[size-1]='\0';
    } /* end if */

    /* Set return value */
    ret_value = (ssize_t)len;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_name() */


/*-------------------------------------------------------------------------
 * Function:    H5Fget_info
 *		1. Get storage size for superblock extension if there is one
 *              2. Get the amount of btree and heap storage for entries
 *                 in the SOHM table if there is one.
 *		Consider success when there is no superblock extension and/or SOHM table
 *
 * Return:      Success:        non-negative on success
 *              Failure:        Negative
 *
 * Programmer:  Vailin Choi
 *              July 11, 2007
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fget_info(hid_t obj_id, H5F_info_t *finfo)
{
    H5F_t *f;                           /* Top file in mount hierarchy */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*x", obj_id, finfo);

    /* Check args */
    if(!finfo)
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "no info struct")

    /* For file IDs, get the file object directly */
    /* (This prevents the H5G_loc() call from returning the file pointer for
     * the top file in a mount hierarchy)
     */
    if(H5I_get_type(obj_id) == H5I_FILE ) {
        if(NULL == (f = (H5F_t *)H5I_object(obj_id)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a file")
    } /* end if */
    else {
        H5G_loc_t     loc;        /* Object location */

        /* Get symbol table entry */
        if(H5G_loc(obj_id, &loc) < 0)
             HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid object ID")
        f = loc.oloc->file;
    } /* end else */
    HDassert(f->shared);

    /* Reset file info struct */
    HDmemset(finfo, 0, sizeof(H5F_info_t));

    /* Check for superblock extension info */
    if(H5F_super_size(f, H5AC_ind_dxpl_id, NULL, &finfo->super_ext_size) < 0)
        HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "Unable to retrieve superblock extension size")

    /* Check for SOHM info */
    if(H5F_addr_defined(f->shared->sohm_addr))
        if(H5SM_ih_size(f, H5AC_ind_dxpl_id, finfo) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTGET, FAIL, "Unable to retrieve SOHM btree & heap storage info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fget_info() */


/*-------------------------------------------------------------------------
 * Function:    H5Fclear_elink_file_cache
 *
 * Purpose:     Releases the external file cache associated with the
 *              provided file, potentially closing any cached files
 *              unless they are held open from somewhere\ else.
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 * Programmer:  Neil Fortner; December 30, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Fclear_elink_file_cache(hid_t file_id)
{
    H5F_t         *file;        /* File */
    herr_t        ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", file_id);

    /* Check args */
    if(NULL == (file = (H5F_t *)H5I_object_verify(file_id, H5I_FILE)))
        HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a file ID")

    /* Release the EFC */
    if(file->shared->efc)
        if(H5F_efc_release(file->shared->efc) < 0)
            HGOTO_ERROR(H5E_FILE, H5E_CANTRELEASE, FAIL, "can't release external file cache")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Fclear_elink_file_cache() */

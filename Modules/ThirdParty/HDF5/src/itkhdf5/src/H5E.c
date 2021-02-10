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
 * Purpose:	Provides error handling in the form of a stack.  The
 *		FUNC_ENTER() macro clears the error stack whenever an API
 *		function is entered.  When an error is detected, an entry is
 *		pushed onto the stack.  As the functions unwind additional
 *		entries are pushed onto the stack. The API function will
 *		return some indication that an error occurred and the
 *		application can print the error stack.
 *
 *		Certain API functions in the H5E package (such as H5Eprint2())
 *		do not clear the error stack.  Otherwise, any function which
 *		doesn't have an underscore immediately after the package name
 *		will clear the error stack.  For instance, H5Fopen() clears
 *		the error stack while H5F_open() does not.
 *
 *		An error stack has a fixed maximum size.  If this size is
 *		exceeded then the stack will be truncated and only the
 *		inner-most functions will have entries on the stack. This is
 *		expected to be a rare condition.
 *
 *		Each thread has its own error stack, but since
 *		multi-threading has not been added to the library yet, this
 *		package maintains a single error stack. The error stack is
 *		statically allocated to reduce the complexity of handling
 *		errors within the H5E package.
 *
 */

/****************/
/* Module Setup */
/****************/

#include "H5Emodule.h"          /* This source code file is part of the H5E module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Iprivate.h"		/* IDs                                  */
#include "H5Epkg.h"		/* Error handling		  	*/
#include "H5FLprivate.h"	/* Free lists                           */
#include "H5MMprivate.h"	/* Memory management			*/

/****************/
/* Local Macros */
/****************/

/* HDF5 error class */
#define H5E_CLS_NAME         "HDF5"
#define H5E_CLS_LIB_NAME     "HDF5"


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/
/* Static function declarations */
static herr_t H5E_set_default_auto(H5E_t *stk);
static H5E_cls_t *H5E_register_class(const char *cls_name, const char *lib_name,
                                const char *version);
static herr_t  H5E_unregister_class(H5E_cls_t *cls);
static ssize_t H5E_get_class_name(const H5E_cls_t *cls, char *name, size_t size);
static int H5E_close_msg_cb(void *obj_ptr, hid_t obj_id, void *udata);
static herr_t  H5E_close_msg(H5E_msg_t *err);
static H5E_msg_t *H5E_create_msg(H5E_cls_t *cls, H5E_type_t msg_type, const char *msg);
static H5E_t  *H5E_get_current_stack(void);
static herr_t  H5E_set_current_stack(H5E_t *estack);
static herr_t  H5E_close_stack(H5E_t *err_stack);
static ssize_t H5E_get_num(const H5E_t *err_stack);


/*********************/
/* Package Variables */
/*********************/

/* Package initialization variable */
hbool_t H5_PKG_INIT_VAR = FALSE;


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/

/* Declare a free list to manage the H5E_t struct */
H5FL_DEFINE_STATIC(H5E_t);

/* Declare a free list to manage the H5E_cls_t struct */
H5FL_DEFINE_STATIC(H5E_cls_t);

/* Declare a free list to manage the H5E_msg_t struct */
H5FL_DEFINE_STATIC(H5E_msg_t);

/* Error class ID class */
static const H5I_class_t H5I_ERRCLS_CLS[1] = {{
    H5I_ERROR_CLASS,		/* ID class value */
    0,				/* Class flags */
    0,				/* # of reserved IDs for class */
    (H5I_free_t)H5E_unregister_class /* Callback routine for closing objects of this class */
}};

/* Error message ID class */
static const H5I_class_t H5I_ERRMSG_CLS[1] = {{
    H5I_ERROR_MSG,		/* ID class value */
    0,				/* Class flags */
    0,				/* # of reserved IDs for class */
    (H5I_free_t)H5E_close_msg   /* Callback routine for closing objects of this class */
}};

/* Error stack ID class */
static const H5I_class_t H5I_ERRSTK_CLS[1] = {{
    H5I_ERROR_STACK,		/* ID class value */
    0,				/* Class flags */
    0,				/* # of reserved IDs for class */
    (H5I_free_t)H5E_close_stack /* Callback routine for closing objects of this class */
}};



/*-------------------------------------------------------------------------
 * Function:	H5E_init
 *
 * Purpose:	Initialize the interface from some other layer.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 29, 2004
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5E_init(void)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)
    /* FUNC_ENTER() does all the work */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5E_init() */


/*--------------------------------------------------------------------------
 * Function:    H5E__init_package
 *
 * Purpose:     Initialize interface-specific information
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Raymond Lu
 *              Friday, July 11, 2003
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5E__init_package(void)
{
    H5E_cls_t   *cls;           /* Pointer to error class */
    H5E_msg_t   *msg;           /* Pointer to new error message */
    char lib_vers[128];         /* Buffer to constructu library version within */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* Initialize the atom group for the error class IDs */
    if(H5I_register_type(H5I_ERRCLS_CLS) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize ID group")

    /* Initialize the atom group for the major error IDs */
    if(H5I_register_type(H5I_ERRMSG_CLS) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize ID group")

    /* Initialize the atom group for the error stacks */
    if(H5I_register_type(H5I_ERRSTK_CLS) < 0)
        HGOTO_ERROR(H5E_ATOM, H5E_CANTINIT, FAIL, "unable to initialize ID group")

#ifndef H5_HAVE_THREADSAFE
    H5E_stack_g[0].nused = 0;
    H5E_set_default_auto(H5E_stack_g);
#endif /* H5_HAVE_THREADSAFE */

    /* Allocate the HDF5 error class */
    HDassert(H5E_ERR_CLS_g == (-1));
    HDsnprintf(lib_vers, sizeof(lib_vers), "%u.%u.%u%s", H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE, (HDstrlen(H5_VERS_SUBRELEASE) > 0 ? "-"H5_VERS_SUBRELEASE : ""));
    if(NULL == (cls = H5E_register_class(H5E_CLS_NAME, H5E_CLS_LIB_NAME, lib_vers)))
        HGOTO_ERROR(H5E_ERROR, H5E_CANTINIT, FAIL, "class initialization failed")
    if((H5E_ERR_CLS_g = H5I_register(H5I_ERROR_CLASS, cls, FALSE)) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error class")

    /* Include the automatically generated error code initialization */
    #include "H5Einit.h"

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5E__init_package() */


/*-------------------------------------------------------------------------
 * Function:	H5E_term_package
 *
 * Purpose:	Terminates the H5E interface
 *
 * Return:	Success:	Positive if anything is done that might
 *				affect other interfaces; zero otherwise.
 *
 * 		Failure:	Negative.
 *
 * Programmer:	Raymond Lu
 *	        Tuesday, July 22, 2003
 *
 *-------------------------------------------------------------------------
 */
int
H5E_term_package(void)
{
    int	n = 0;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    if(H5_PKG_INIT_VAR) {
        int64_t ncls, nmsg, nstk;

        /* Check if there are any open error stacks, classes or messages */
        ncls = H5I_nmembers(H5I_ERROR_CLASS);
        nmsg = H5I_nmembers(H5I_ERROR_MSG);
        nstk = H5I_nmembers(H5I_ERROR_STACK);

        if((ncls + nmsg + nstk) > 0) {
            /* Clear any outstanding error stacks */
            if(nstk > 0)
	        (void)H5I_clear_type(H5I_ERROR_STACK, FALSE, FALSE);

            /* Clear all the error classes */
	    if(ncls > 0) {
	        (void)H5I_clear_type(H5I_ERROR_CLASS, FALSE, FALSE);

                /* Reset the HDF5 error class, if its been closed */
                if(H5I_nmembers(H5I_ERROR_CLASS) == 0)
                    H5E_ERR_CLS_g = -1;
            } /* end if */

            /* Clear all the error messages */
	    if(nmsg > 0) {
	        (void)H5I_clear_type(H5I_ERROR_MSG, FALSE, FALSE);

                /* Reset the HDF5 error messages, if they've been closed */
                if(H5I_nmembers(H5I_ERROR_MSG) == 0) {
                    /* Include the automatically generated error code termination */
                    #include "H5Eterm.h"
                } /* end if */
            } /* end if */

            n++; /*H5I*/
	} /* end if */
        else {
	    /* Destroy the error class, message, and stack id groups */
	    n += (H5I_dec_type_ref(H5I_ERROR_STACK) > 0);
	    n += (H5I_dec_type_ref(H5I_ERROR_CLASS) > 0);
	    n += (H5I_dec_type_ref(H5I_ERROR_MSG) > 0);

	    /* Mark closed */
            if(0 == n)
                H5_PKG_INIT_VAR = FALSE;
	} /* end else */
    } /* end if */

    FUNC_LEAVE_NOAPI(n)
} /* end H5E_term_package() */


/*--------------------------------------------------------------------------
 * Function:    H5E_set_default_auto
 *
 * Purpose:     Initialize "automatic" error stack reporting info to library
 *              default
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Thursday, November 1, 2007
 *
 *--------------------------------------------------------------------------
 */
static herr_t
H5E_set_default_auto(H5E_t *stk)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

#ifndef H5_NO_DEPRECATED_SYMBOLS
#ifdef H5_USE_16_API_DEFAULT
    stk->auto_op.vers = 1;
#else /* H5_USE_16_API */
    stk->auto_op.vers = 2;
#endif /* H5_USE_16_API_DEFAULT */

    stk->auto_op.func1 = stk->auto_op.func1_default = (H5E_auto1_t)H5Eprint1;
    stk->auto_op.func2 = stk->auto_op.func2_default = (H5E_auto2_t)H5Eprint2;
    stk->auto_op.is_default = TRUE;
#else /* H5_NO_DEPRECATED_SYMBOLS */
    stk->auto_op.func2 = (H5E_auto2_t)H5Eprint2;
#endif /* H5_NO_DEPRECATED_SYMBOLS */

    stk->auto_data = NULL;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5E_set_default_auto() */


#ifdef H5_HAVE_THREADSAFE
/*-------------------------------------------------------------------------
 * Function:	H5E_get_stack
 *
 * Purpose:	Support function for H5E_get_my_stack() to initialize and
 *              acquire per-thread error stack.
 *
 * Return:	Success:	error stack (H5E_t *)
 *
 *		Failure:	NULL
 *
 * Programmer:	Chee Wai LEE
 *              April 24, 2000
 *
 *-------------------------------------------------------------------------
 */
H5E_t *
H5E_get_stack(void)
{
    H5E_t *estack = NULL;

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    estack = (H5E_t *)H5TS_get_thread_local_value(H5TS_errstk_key_g);

    if(!estack) {
        /* No associated value with current thread - create one */
#ifdef H5_HAVE_WIN_THREADS
        /* Win32 has to use LocalAlloc to match the LocalFree in DllMain */
        estack = (H5E_t *)LocalAlloc(LPTR, sizeof(H5E_t)); 
#else
        /* Use HDmalloc here since this has to match the HDfree in the
         * destructor and we want to avoid the codestack there.
         */
        estack = (H5E_t *)HDmalloc(sizeof(H5E_t));
#endif /* H5_HAVE_WIN_THREADS */
        HDassert(estack);

        /* Set the thread-specific info */
        estack->nused = 0;
        H5E_set_default_auto(estack);

        /* (It's not necessary to release this in this API, it is
         *      released by the "key destructor" set up in the H5TS
         *      routines.  See calls to pthread_key_create() in H5TS.c -QAK)
         */
        H5TS_set_thread_local_value(H5TS_errstk_key_g, (void *)estack);
    } /* end if */

    /* Set return value */
    FUNC_LEAVE_NOAPI(estack)
} /* end H5E_get_stack() */
#endif  /* H5_HAVE_THREADSAFE */


/*-------------------------------------------------------------------------
 * Function:	H5E_free_class
 *
 * Purpose:	Private function to free an error class.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Friday, January 22, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_free_class(H5E_cls_t *cls)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check arguments */
    HDassert(cls);

    /* Free error class structure */
    cls->cls_name = (char *)H5MM_xfree((void*)cls->cls_name);
    cls->lib_name = (char *)H5MM_xfree((void*)cls->lib_name);
    cls->lib_vers = (char *)H5MM_xfree((void*)cls->lib_vers);
    cls = H5FL_FREE(H5E_cls_t, cls);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5E_free_class() */


/*-------------------------------------------------------------------------
 * Function:	H5Eregister_class
 *
 * Purpose:	Registers an error class.
 *
 * Return:	Non-negative value as class ID on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Eregister_class(const char *cls_name, const char *lib_name, const char *version)
{
    H5E_cls_t   *cls;        /* Pointer to error class */
    hid_t       ret_value;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("i", "*s*s*s", cls_name, lib_name, version);

    /* Check arguments */
    if(cls_name == NULL || lib_name == NULL || version == NULL)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "invalid string")

    /* Create the new error class object */
    if(NULL == (cls = H5E_register_class(cls_name, lib_name, version)))
	HGOTO_ERROR(H5E_ERROR, H5E_CANTCREATE, FAIL, "can't create error class")

    /* Register the new error class to get an ID for it */
    if((ret_value = H5I_register(H5I_ERROR_CLASS, cls, TRUE)) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error class")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eregister_class() */


/*-------------------------------------------------------------------------
 * Function:	H5E_register_class
 *
 * Purpose:	Private function to register an error class.
 *
 * Return:	Non-negative value as class ID on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 *-------------------------------------------------------------------------
 */
static H5E_cls_t *
H5E_register_class(const char *cls_name, const char *lib_name, const char *version)
{
    H5E_cls_t   *cls = NULL;            /* Pointer to error class */
    H5E_cls_t   *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments */
    HDassert(cls_name);
    HDassert(lib_name);
    HDassert(version);

    /* Allocate space for new error class */
    if(NULL == (cls = H5FL_CALLOC(H5E_cls_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Duplicate string information */
    if(NULL == (cls->cls_name = H5MM_xstrdup(cls_name)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    if(NULL == (cls->lib_name = H5MM_xstrdup(lib_name)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    if(NULL == (cls->lib_vers = H5MM_xstrdup(version)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set the return value */
    ret_value = cls;

done:
    if(!ret_value)
        if(cls && H5E_free_class(cls) < 0)
            HDONE_ERROR(H5E_ERROR, H5E_CANTRELEASE, NULL, "unable to free error class")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5E_register_class() */


/*-------------------------------------------------------------------------
 * Function:	H5Eunregister_class
 *
 * Purpose:	Closes an error class.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eunregister_class(hid_t class_id)
{
    herr_t       ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", class_id);

    /* Check arguments */
    if(H5I_ERROR_CLASS != H5I_get_type(class_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an error class")

    /*
     * Decrement the counter on the dataset.  It will be freed if the count
     * reaches zero.
     */
    if(H5I_dec_app_ref(class_id) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error class")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eunregister_class() */


/*-------------------------------------------------------------------------
 * Function:	H5E_unregister_class
 *
 * Purpose:	Private function to close an error class.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_unregister_class(H5E_cls_t *cls)
{
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments */
    HDassert(cls);

    /* Iterate over all the messages and delete those in this error class */
    if(H5I_iterate(H5I_ERROR_MSG, H5E_close_msg_cb, cls, FALSE) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_BADITER, FAIL, "unable to free all messages in this error class")

    /* Free error class structure */
    if(H5E_free_class(cls) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTRELEASE, FAIL, "unable to free error class")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5E_unregister_class() */


/*-------------------------------------------------------------------------
 * Function:	H5Eget_class_name
 *
 * Purpose:	Retrieves error class name.
 *
 * Return:      Non-negative for name length if succeeds(zero means no name);
 *              otherwise returns negative value.
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Eget_class_name(hid_t class_id, char *name, size_t size)
{
    H5E_cls_t   *cls;        /* Pointer to error class */
    ssize_t     ret_value;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("Zs", "i*sz", class_id, name, size);

    /* Get the error class */
    if(NULL == (cls = (H5E_cls_t *)H5I_object_verify(class_id, H5I_ERROR_CLASS)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error class ID")

    /* Retrieve the class name */
    if((ret_value = H5E_get_class_name(cls, name, size)) < 0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get error class name")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eget_class_name() */


/*-------------------------------------------------------------------------
 * Function:	H5E_get_class_name
 *
 * Purpose:	Private function to retrieve error class name.
 *
 * Return:      Non-negative for name length if succeeds(zero means no name);
 *              otherwise returns negative value.
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5E_get_class_name(const H5E_cls_t *cls, char *name, size_t size)
{
    ssize_t       len = 0;          /* Length of error class's name */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check arguments */
    HDassert(cls);

    /* Get the class's name */
    len = (ssize_t)HDstrlen(cls->cls_name);

    /* Set the user's buffer, if provided */
    if(name) {
       HDstrncpy(name, cls->cls_name, MIN((size_t)(len + 1), size));
       if((size_t)len >= size)
          name[size - 1] = '\0';
    } /* end if */

    /* Return the full length */
    FUNC_LEAVE_NOAPI(len)
} /* end H5E_get_class_name() */


/*-------------------------------------------------------------------------
 * Function:    H5E_close_msg_cb
 *
 * Purpose:     H5I_iterate callback function to close error messages in the
 *              error class.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:  Raymond Lu
 *              July 14, 2003
 *
 *-------------------------------------------------------------------------
 */
static int
H5E_close_msg_cb(void *obj_ptr, hid_t obj_id, void *udata)
{
    H5E_msg_t   *err_msg = (H5E_msg_t*)obj_ptr;
    H5E_cls_t   *cls = (H5E_cls_t*)udata;
    herr_t      ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments */
    HDassert(err_msg);

    /* Close the message if it is in the class being closed */
    if(err_msg->cls == cls) {
        if(H5E_close_msg(err_msg) < 0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTCLOSEOBJ, FAIL, "unable to close error message")
        if(NULL == H5I_remove(obj_id))
            HGOTO_ERROR(H5E_ERROR, H5E_CANTREMOVE, FAIL, "unable to remove error message")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5E_close_msg_cb() */


/*-------------------------------------------------------------------------
 * Function:	H5Eclose_msg
 *
 * Purpose:	Closes a major or minor error.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eclose_msg(hid_t err_id)
{
    herr_t       ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", err_id);

    /* Check arguments */
    if(H5I_ERROR_MSG != H5I_get_type(err_id))
        HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not an error class")

    /* Decrement the counter.  It will be freed if the count reaches zero. */
    if(H5I_dec_app_ref(err_id) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error message")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eclose_msg() */


/*-------------------------------------------------------------------------
 * Function:	H5E_close_msg
 *
 * Purpose:	Private function to close an error messge.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_close_msg(H5E_msg_t *err)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check arguments */
    HDassert(err);

    /* Release message */
    err->msg = (char *)H5MM_xfree((void *)err->msg);
    /* Don't free err->cls here */
    err = H5FL_FREE(H5E_msg_t, err);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5E_close_msg() */


/*-------------------------------------------------------------------------
 * Function:	H5Ecreate_msg
 *
 * Purpose:	Creates a major or minor error, returns an ID.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Ecreate_msg(hid_t class_id, H5E_type_t msg_type, const char *msg_str)
{
    H5E_cls_t   *cls;           /* Pointer to error class */
    H5E_msg_t   *msg;           /* Pointer to new error message */
    hid_t       ret_value;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("i", "iEt*s", class_id, msg_type, msg_str);

    /* Check arguments */
    if(msg_type != H5E_MAJOR && msg_type != H5E_MINOR)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "not a valid message type")
    if(msg_str == NULL)
	HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, FAIL, "message is NULL")

    /* Get the error class */
    if(NULL == (cls = (H5E_cls_t *)H5I_object_verify(class_id, H5I_ERROR_CLASS)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error class ID")

    /* Create the new error message object */
    if(NULL == (msg = H5E_create_msg(cls, msg_type, msg_str)))
	HGOTO_ERROR(H5E_ERROR, H5E_CANTCREATE, FAIL, "can't create error message")

    /* Register the new error class to get an ID for it */
    if((ret_value = H5I_register(H5I_ERROR_MSG, msg, TRUE)) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't register error message")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Ecreate_msg() */

/*-------------------------------------------------------------------------
 * Function:	H5E_create_msg
 *
 * Purpose:	Private function to create a major or minor error.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 *-------------------------------------------------------------------------
 */
static H5E_msg_t *
H5E_create_msg(H5E_cls_t *cls, H5E_type_t msg_type, const char *msg_str)
{
    H5E_msg_t   *msg = NULL;            /* Pointer to new error message */
    H5E_msg_t   *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments */
    HDassert(cls);
    HDassert(msg_type == H5E_MAJOR || msg_type == H5E_MINOR);
    HDassert(msg_str);

    /* Allocate new message object */
    if(NULL == (msg = H5FL_MALLOC(H5E_msg_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Fill new message object */
    msg->cls = cls;
    msg->type = msg_type;
    if(NULL == (msg->msg = H5MM_xstrdup(msg_str)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set return value */
    ret_value = msg;

done:
    if(!ret_value)
        if(msg && H5E_close_msg(msg) < 0)
            HDONE_ERROR(H5E_ERROR, H5E_CANTCLOSEOBJ, NULL, "unable to close error message")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5E_create_msg() */


/*-------------------------------------------------------------------------
 * Function:	H5Eget_msg
 *
 * Purpose:	Retrieves an error message.
 *
 * Return:      Non-negative for message length if succeeds(zero means no message);
 *              otherwise returns negative value.
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Eget_msg(hid_t msg_id, H5E_type_t *type, char *msg_str, size_t size)
{
    H5E_msg_t   *msg;           /* Pointer to error message */
    ssize_t      ret_value;     /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE4("Zs", "i*Et*sz", msg_id, type, msg_str, size);

    /* Get the message object */
    if(NULL == (msg = (H5E_msg_t *)H5I_object_verify(msg_id, H5I_ERROR_MSG)))
	HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error message ID")

    /* Get the message's text */
    if((ret_value = H5E_get_msg(msg, type, msg_str, size)) < 0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get error message text")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eget_msg() */


/*-------------------------------------------------------------------------
 * Function:	H5Ecreate_stack
 *
 * Purpose:	Creates a new, empty, error stack.
 *
 * Return:	Non-negative value as stack ID on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Thursday, November 1, 2007
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Ecreate_stack(void)
{
    H5E_t	*stk;           /* Error stack */
    hid_t       ret_value;      /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE0("i","");

    /* Allocate a new error stack */
    if(NULL == (stk = H5FL_CALLOC(H5E_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* Set the "automatic" error reporting info to the library default */
    H5E_set_default_auto(stk);

    /* Register the stack */
    if((ret_value = H5I_register(H5I_ERROR_STACK, stk, TRUE)) < 0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't create error stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Ecreate_stack() */


/*-------------------------------------------------------------------------
 * Function:	H5Eget_current_stack
 *
 * Purpose:	Registers current error stack, returns object handle for it,
 *              clears it.
 *
 * Return:	Non-negative value as stack ID on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 *-------------------------------------------------------------------------
 */
hid_t
H5Eget_current_stack(void)
{
    H5E_t	*stk;           /* Error stack */
    hid_t       ret_value;   /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(FAIL)
    H5TRACE0("i","");

    /* Get the current stack */
    if(NULL == (stk = H5E_get_current_stack()))
	HGOTO_ERROR(H5E_ERROR, H5E_CANTCREATE, FAIL, "can't create error stack")

    /* Register the stack */
    if((ret_value = H5I_register(H5I_ERROR_STACK, stk, TRUE)) < 0)
	HGOTO_ERROR(H5E_ERROR, H5E_CANTREGISTER, FAIL, "can't create error stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eget_current_stack() */


/*-------------------------------------------------------------------------
 * Function:	H5E_get_current_stack
 *
 * Purpose:	Private function to register an error stack.
 *
 * Return:	Non-negative value as class ID on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 11, 2003
 *
 *-------------------------------------------------------------------------
 */
static H5E_t *
H5E_get_current_stack(void)
{
    H5E_t	*current_stack;         /* Pointer to the current error stack */
    H5E_t	*estack_copy = NULL;    /* Pointer to new error stack to return */
    unsigned    u;                      /* Local index variable */
    H5E_t      *ret_value = NULL;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Get a pointer to the current error stack */
    if(NULL == (current_stack = H5E_get_my_stack())) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, NULL, "can't get current error stack")

    /* Allocate a new error stack */
    if(NULL == (estack_copy = H5FL_CALLOC(H5E_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Make a copy of current error stack */
    estack_copy->nused = current_stack->nused;
    for(u = 0; u < current_stack->nused; u++) {
        H5E_error2_t *current_error, *new_error; /* Pointers to errors on each stack */

        /* Get pointers into the current error stack location */
        current_error = &(current_stack->slot[u]);
        new_error = &(estack_copy->slot[u]);

        /* Increment the IDs to indicate that they are used in this stack */
        if(H5I_inc_ref(current_error->cls_id, FALSE) < 0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, NULL, "unable to increment ref count on error class")
        new_error->cls_id = current_error->cls_id;
        if(H5I_inc_ref(current_error->maj_num, FALSE) < 0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, NULL, "unable to increment ref count on error message")
        new_error->maj_num = current_error->maj_num;
        if(H5I_inc_ref(current_error->min_num, FALSE) < 0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, NULL, "unable to increment ref count on error message")
        new_error->min_num = current_error->min_num;
        if(NULL == (new_error->func_name = H5MM_xstrdup(current_error->func_name)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        if(NULL == (new_error->file_name = H5MM_xstrdup(current_error->file_name)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        new_error->line = current_error->line;
        if(NULL == (new_error->desc = H5MM_xstrdup(current_error->desc)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    } /* end for */

    /* Copy the "automatic" error reporting information */
    estack_copy->auto_op = current_stack->auto_op;
    estack_copy->auto_data = current_stack->auto_data;

    /* Empty current error stack */
    H5E_clear_stack(current_stack);

    /* Set the return value */
    ret_value = estack_copy;

done:
    if(ret_value == NULL)
        if(estack_copy)
            estack_copy = H5FL_FREE(H5E_t, estack_copy);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5E_get_current_stack() */


/*-------------------------------------------------------------------------
 * Function:	H5Eset_current_stack
 *
 * Purpose:     Replaces current stack with specified stack.  This closes the
 *		stack ID also.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 15, 2003
 *
 * Modification:
 *              Raymond Lu
 *              7 September 2010
 *              Also closes the stack to avoid potential problem (bug 1799)
 * 
 *-------------------------------------------------------------------------
 */
herr_t
H5Eset_current_stack(hid_t err_stack)
{
    H5E_t *estack;
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", err_stack);

    if(err_stack != H5E_DEFAULT) {
        if(NULL == (estack = (H5E_t *)H5I_object_verify(err_stack, H5I_ERROR_STACK)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")

        /* Set the current error stack */
        if(H5E_set_current_stack(estack) < 0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "unable to set error stack")

        /*
         * Decrement the counter on the error stack.  It will be freed if the count
         * reaches zero.
         */
        if(H5I_dec_app_ref(err_stack) < 0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error stack")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eset_current_stack() */


/*-------------------------------------------------------------------------
 * Function:	H5E_set_current_stack
 *
 * Purpose:	Private function to replace an error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 15, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_set_current_stack(H5E_t *estack)
{
    H5E_t	*current_stack;         /* Default error stack */
    unsigned     u;                     /* Local index variable */
    herr_t       ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity check */
    HDassert(estack);

    /* Get a pointer to the current error stack */
    if(NULL == (current_stack = H5E_get_my_stack ())) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
	HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")

    /* Empty current error stack */
    H5E_clear_stack(current_stack);

    /* Copy new stack to current error stack */
    current_stack->nused = estack->nused;
    for(u = 0; u < current_stack->nused; u++) {
        H5E_error2_t *current_error, *new_error; /* Pointers to errors on each stack */

        /* Get pointers into the current error stack location */
        current_error = &(current_stack->slot[u]);
        new_error = &(estack->slot[u]);

        /* Increment the IDs to indicate that they are used in this stack */
        if(H5I_inc_ref(new_error->cls_id, FALSE) < 0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, FAIL, "unable to increment ref count on error class")
        current_error->cls_id = new_error->cls_id;
        if(H5I_inc_ref(new_error->maj_num, FALSE) < 0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, FAIL, "unable to increment ref count on error class")
        current_error->maj_num = new_error->maj_num;
        if(H5I_inc_ref(new_error->min_num, FALSE) < 0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTINC, FAIL, "unable to increment ref count on error class")
        current_error->min_num = new_error->min_num;
        if(NULL == (current_error->func_name = H5MM_xstrdup(new_error->func_name)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
        if(NULL == (current_error->file_name = H5MM_xstrdup(new_error->file_name)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
        current_error->line = new_error->line;
        if(NULL == (current_error->desc = H5MM_xstrdup(new_error->desc)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    } /* end for */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5E_set_current_stack() */


/*-------------------------------------------------------------------------
 * Function:	H5Eclose_stack
 *
 * Purpose:	Closes an error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eclose_stack(hid_t stack_id)
{
    herr_t       ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE1("e", "i", stack_id);

    if(H5E_DEFAULT != stack_id) {
        /* Check arguments */
        if(H5I_ERROR_STACK != H5I_get_type(stack_id))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")

        /*
         * Decrement the counter on the error stack.  It will be freed if the count
         * reaches zero.
         */
        if(H5I_dec_app_ref(stack_id) < 0)
            HGOTO_ERROR(H5E_ERROR, H5E_CANTDEC, FAIL, "unable to decrement ref count on error stack")
    } /* end if */

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eclose_stack() */


/*-------------------------------------------------------------------------
 * Function:	H5E_close_stack
 *
 * Purpose:	Private function to close an error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 14, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5E_close_stack(H5E_t *estack)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(estack);

    /* Release the stack's error information */
    H5E_clear_stack(estack);

    /* Free the stack structure */
    estack = H5FL_FREE(H5E_t, estack);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5E_close_stack() */


/*-------------------------------------------------------------------------
 * Function:	H5Eget_num
 *
 * Purpose:	Retrieves the number of error message.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 15, 2003
 *
 *-------------------------------------------------------------------------
 */
ssize_t
H5Eget_num(hid_t error_stack_id)
{
    H5E_t *estack;         /* Error stack to operate on */
    ssize_t ret_value;      /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(FAIL)
    H5TRACE1("Zs", "i", error_stack_id);

    /* Need to check for errors */
    if(error_stack_id == H5E_DEFAULT) {
    	if(NULL == (estack = H5E_get_my_stack())) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear_stack(NULL);

        /* Get the error stack to operate on */
        if(NULL == (estack = (H5E_t *)H5I_object_verify(error_stack_id, H5I_ERROR_STACK)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */

    /* Get the number of errors on stack */
    if((ret_value = H5E_get_num(estack)) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get number of errors")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eget_num() */


/*-------------------------------------------------------------------------
 * Function:	H5E_get_num
 *
 * Purpose:	Private function to retrieve number of errors in error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 15, 2003
 *
 *-------------------------------------------------------------------------
 */
static ssize_t
H5E_get_num(const H5E_t *estack)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    HDassert(estack);

    FUNC_LEAVE_NOAPI((ssize_t)estack->nused)
} /* end H5E_get_num() */


/*-------------------------------------------------------------------------
 * Function:	H5Epop
 *
 * Purpose:	Deletes some error messages from the top of error stack.
 *
 * Return:	Non-negative value on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Friday, July 16, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Epop(hid_t err_stack, size_t count)
{
    H5E_t *estack;
    herr_t ret_value = SUCCEED;   /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(FAIL)
    H5TRACE2("e", "iz", err_stack, count);

    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT) {
    	if(NULL == (estack = H5E_get_my_stack())) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear_stack(NULL);

        /* Get the error stack to operate on */
        if(NULL == (estack = (H5E_t *)H5I_object_verify(err_stack, H5I_ERROR_STACK)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */

    /* Range limit the number of errors to pop off stack */
    if(count > estack->nused)
        count = estack->nused;

    /* Pop the errors off the stack */
    if(H5E_pop(estack, count) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTRELEASE, FAIL, "can't pop errors from stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Epop() */


/*-------------------------------------------------------------------------
 * Function:	H5Epush2
 *
 * Purpose:	Pushes a new error record onto error stack for the current
 *		thread.  The error has major and minor IDs MAJ_ID and
 *		MIN_ID, the name of a function where the error was detected,
 *		the name of the file where the error was detected, the
 *		line within that file, and an error description string.  The
 *		function name, file name, and error description strings must
 *		be statically allocated.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Monday, October 18, 1999
 *
 * Notes: 	Basically a new public API wrapper around the H5E__push_stack
 *              function.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Epush2(hid_t err_stack, const char *file, const char *func, unsigned line,
        hid_t cls_id, hid_t maj_id, hid_t min_id, const char *fmt, ...)
{
    va_list     ap;             /* Varargs info */
    H5E_t       *estack;        /* Pointer to error stack to modify */
#ifndef H5_HAVE_VASPRINTF
    int         tmp_len;        /* Current size of description buffer */
    int         desc_len;       /* Actual length of description when formatted */
#endif /* H5_HAVE_VASPRINTF */
    char        *tmp = NULL;      /* Buffer to place formatted description in */
    hbool_t     va_started = FALSE; /* Whether the variable argument list is open */
    herr_t	ret_value=SUCCEED;      /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(FAIL)
    H5TRACE8("e", "i*s*sIuiii*s", err_stack, file, func, line, cls_id, maj_id,
             min_id, fmt);

    if(err_stack == H5E_DEFAULT)
    	estack = NULL;
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear_stack(NULL);

        /* Get the error stack to operate on */
        if(NULL == (estack = (H5E_t *)H5I_object_verify(err_stack, H5I_ERROR_STACK)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */

/* Note that the variable-argument parsing for the format is identical in
 *      the H5E_printf_stack() routine - correct errors and make changes in both
 *      places. -QAK
 */

    /* Format the description */
    va_start(ap, fmt);
    va_started = TRUE;

#ifdef H5_HAVE_VASPRINTF
    /* Use the vasprintf() routine, since it does what we're trying to do below */
    if(HDvasprintf(&tmp, fmt, ap) < 0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
#else /* H5_HAVE_VASPRINTF */
    /* Allocate space for the formatted description buffer */
    tmp_len = 128;
    if(NULL == (tmp = H5MM_malloc((size_t)tmp_len)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")

    /* If the description doesn't fit into the initial buffer size, allocate more space and try again */
    while((desc_len = HDvsnprintf(tmp, (size_t)tmp_len, fmt, ap)) > (tmp_len - 1)) {
        /* shutdown & restart the va_list */
        va_end(ap);
        va_start(ap, fmt);

        /* Release the previous description, it's too small */
        H5MM_xfree(tmp);

        /* Allocate a description of the appropriate length */
        tmp_len = desc_len + 1;
        if(NULL == (tmp = H5MM_malloc((size_t)tmp_len)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed")
    } /* end while */
#endif /* H5_HAVE_VASPRINTF */

    /* Push the error on the stack */
    if(H5E__push_stack(estack, file, func, line, cls_id, maj_id, min_id, tmp) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't push error on stack")

done:
    if(va_started)
        va_end(ap);
#ifdef H5_HAVE_VASPRINTF
    /* Memory was allocated with HDvasprintf so it needs to be freed
     * with HDfree
     */
    if(tmp)
        HDfree(tmp);
#else /* H5_HAVE_VASPRINTF */
    if(tmp)
        H5MM_xfree(tmp);
#endif /* H5_HAVE_VASPRINTF */

    FUNC_LEAVE_API(ret_value)
} /* end H5Epush2() */


/*-------------------------------------------------------------------------
 * Function:	H5Eclear2
 *
 * Purpose:	Clears the error stack for the specified error stack.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Raymond Lu
 *              Wednesday, July 16, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eclear2(hid_t err_stack)
{
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t ret_value = SUCCEED;   /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(FAIL)
    H5TRACE1("e", "i", err_stack);

    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT)
    	estack = NULL;
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear_stack(NULL);

        if(NULL == (estack = (H5E_t *)H5I_object_verify(err_stack, H5I_ERROR_STACK)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */

    /* Clear the error stack */
    if(H5E_clear_stack(estack) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't clear error stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eclear2() */


/*-------------------------------------------------------------------------
 * Function:	H5Eprint2
 *
 * Purpose:	Prints the error stack in some default way.  This is just a
 *		convenience function for H5Ewalk() with a function that
 *		prints error messages.  Users are encouraged to write their
 *		own more specific error handlers.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eprint2(hid_t err_stack, FILE *stream)
{
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t ret_value = SUCCEED;   /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(FAIL)
    /*NO TRACE*/

    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT) {
    	if(NULL == (estack = H5E_get_my_stack())) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear_stack(NULL);

        if(NULL == (estack = (H5E_t *)H5I_object_verify(err_stack, H5I_ERROR_STACK)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */

    /* Print error stack */
    if(H5E_print(estack, stream, FALSE) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTLIST, FAIL, "can't display error stack")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eprint2() */


/*-------------------------------------------------------------------------
 * Function:	H5Ewalk2
 *
 * Purpose:	Walks the error stack for the current thread and calls some
 *		function for each error along the way.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Ewalk2(hid_t err_stack, H5E_direction_t direction, H5E_walk2_t stack_func, void *client_data)
{
    H5E_t *estack;                      /* Error stack to operate on */
    H5E_walk_op_t op;                   /* Operator for walking error stack */
    herr_t ret_value = SUCCEED;         /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(FAIL)
    /*NO TRACE*/

    /* Need to check for errors */
    if(err_stack == H5E_DEFAULT) {
    	if(NULL == (estack = H5E_get_my_stack())) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else {
        /* Only clear the error stack if it's not the default stack */
        H5E_clear_stack(NULL);

        if(NULL == (estack = (H5E_t *)H5I_object_verify(err_stack, H5I_ERROR_STACK)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")
    } /* end else */

    /* Walk the error stack */
    op.vers = 2;
    op.u.func2 = stack_func;
    if((ret_value = H5E_walk(estack, direction, &op, client_data)) < 0)
        HERROR(H5E_ERROR, H5E_CANTLIST, "can't walk error stack");

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Ewalk2() */


/*-------------------------------------------------------------------------
 * Function:	H5Eget_auto2
 *
 * Purpose:	Returns the current settings for the automatic error stack
 *		traversal function and its data for specific error stack.
 *		Either (or both) arguments may be null in which case the
 *		value is not returned.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Saturday, February 28, 1998
 *
 * Modification:Raymond Lu
 *              4 October 2010
 *              If the printing function isn't the default H5Eprint1 or 2, 
 *              and H5Eset_auto1 has been called to set the old style 
 *              printing function, a call to H5Eget_auto2 should fail.
 *-------------------------------------------------------------------------
 */
herr_t
H5Eget_auto2(hid_t estack_id, H5E_auto2_t *func, void **client_data)
{
    H5E_t   *estack;            /* Error stack to operate on */
    H5E_auto_op_t op;           /* Error stack function */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "i*x**x", estack_id, func, client_data);

    if(estack_id == H5E_DEFAULT) {
    	if(NULL == (estack = H5E_get_my_stack())) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else
        if(NULL == (estack = (H5E_t *)H5I_object_verify(estack_id, H5I_ERROR_STACK)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")

    /* Get the automatic error reporting information */
    if(H5E_get_auto(estack, &op, client_data) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get automatic error info")

#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Fail if the printing function isn't the default(user-set) and set through H5Eset_auto1 */
    if(!op.is_default && op.vers == 1)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "wrong API function, H5Eset_auto1 has been called")
#endif /* H5_NO_DEPRECATED_SYMBOLS */

    if(func)
        *func = op.func2;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eget_auto2() */


/*-------------------------------------------------------------------------
 * Function:	H5Eset_auto2
 *
 * Purpose:	Turns on or off automatic printing of errors for certain
 *              error stack.  When turned on (non-null FUNC pointer) any
 *              API function which returns an error indication will first
 *              call FUNC passing it CLIENT_DATA as an argument.
 *
 *		The default values before this function is called are
 *		H5Eprint2() with client data being the standard error stream,
 *		stderr.
 *
 *		Automatic stack traversal is always in the H5E_WALK_DOWNWARD
 *		direction.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Robb Matzke
 *              Friday, February 27, 1998
 *
 * Modification:Raymond Lu
 *              4 October 2010
 *              If the FUNC is H5Eprint2, put the IS_DEFAULT flag on.
 *-------------------------------------------------------------------------
 */
herr_t
H5Eset_auto2(hid_t estack_id, H5E_auto2_t func, void *client_data)
{
    H5E_t   *estack;            /* Error stack to operate on */
    H5E_auto_op_t op;           /* Error stack operator */
    herr_t ret_value = SUCCEED; /* Return value */

    /* Don't clear the error stack! :-) */
    FUNC_ENTER_API_NOCLEAR(FAIL)
    H5TRACE3("e", "ix*x", estack_id, func, client_data);

    if(estack_id == H5E_DEFAULT) {
    	if(NULL == (estack = H5E_get_my_stack())) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else
        if(NULL == (estack = (H5E_t *)H5I_object_verify(estack_id, H5I_ERROR_STACK)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")

#ifndef H5_NO_DEPRECATED_SYMBOLS
    /* Get the automatic error reporting information */
    if(H5E_get_auto(estack, &op, NULL) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get automatic error info")

    /* Set the automatic error reporting information */
    if(func != op.func2_default)
        op.is_default = FALSE;
    else
        op.is_default = TRUE;

    op.vers = 2;
#endif /* H5_NO_DEPRECATED_SYMBOLS */

    /* Set the automatic error reporting function */
    op.func2 = func;

    if(H5E_set_auto(estack, &op, client_data) < 0)
        HGOTO_ERROR(H5E_ERROR, H5E_CANTSET, FAIL, "can't set automatic error info")

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eset_auto2() */


/*-------------------------------------------------------------------------
 * Function:	H5Eauto_is_v2
 *
 * Purpose:	Determines if the error auto reporting function for an
 *              error stack conforms to the H5E_auto_stack_t typedef
 *              or the H5E_auto_t typedef.  The IS_STACK parameter is set
 *              to 1 for the first case and 0 for the latter case.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, September  8, 2004
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5Eauto_is_v2(hid_t estack_id, unsigned *is_stack)
{
    H5E_t   *estack;            /* Error stack to operate on */
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_API(FAIL)
    H5TRACE2("e", "i*Iu", estack_id, is_stack);

    if(estack_id == H5E_DEFAULT) {
    	if(NULL == (estack = H5E_get_my_stack())) /*lint !e506 !e774 Make lint 'constant value Boolean' in non-threaded case */
            HGOTO_ERROR(H5E_ERROR, H5E_CANTGET, FAIL, "can't get current error stack")
    } /* end if */
    else
        if(NULL == (estack = (H5E_t *)H5I_object_verify(estack_id, H5I_ERROR_STACK)))
            HGOTO_ERROR(H5E_ARGS, H5E_BADTYPE, FAIL, "not a error stack ID")

    /* Check if the error stack reporting function is the "newer" stack type */
    if(is_stack)
#ifndef H5_NO_DEPRECATED_SYMBOLS
        *is_stack = estack->auto_op.vers > 1;
#else
        *is_stack = 1;
#endif

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5Eauto_is_v2() */


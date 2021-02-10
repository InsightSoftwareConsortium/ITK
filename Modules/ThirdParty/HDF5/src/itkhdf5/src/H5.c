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


/***********/
/* Headers */
/***********/
#include "H5private.h"          /* Generic Functions                        */
#include "H5ACprivate.h"        /* Metadata cache                           */
#include "H5CXprivate.h"        /* API Contexts                             */
#include "H5Dprivate.h"         /* Datasets                                 */
#include "H5Eprivate.h"         /* Error handling                           */
#include "H5FLprivate.h"        /* Free lists                               */
#include "H5FSprivate.h"        /* File free space                          */
#include "H5Lprivate.h"         /* Links                                    */
#include "H5MMprivate.h"        /* Memory management                        */
#include "H5Pprivate.h"         /* Property lists                           */
#include "H5SLprivate.h"        /* Skip lists                               */
#include "H5Tprivate.h"         /* Datatypes                                */

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
static void H5_debug_mask(const char*);
#ifdef H5_HAVE_PARALLEL
static int H5_mpi_delete_cb(MPI_Comm comm, int keyval, void *attr_val, int *flag);
#endif /*H5_HAVE_PARALLEL*/

/*********************/
/* Package Variables */
/*********************/


/*****************************/
/* Library Private Variables */
/*****************************/

/* HDF5 API Entered variable */
/* (move to H5.c when new FUNC_ENTER macros in actual use -QAK) */
hbool_t H5_api_entered_g = FALSE;

/* statically initialize block for pthread_once call used in initializing */
/* the first global mutex                                                 */
#ifdef H5_HAVE_THREADSAFE
H5_api_t H5_g;
#else
hbool_t H5_libinit_g = FALSE;   /* Library hasn't been initialized */
hbool_t H5_libterm_g = FALSE;   /* Library isn't being shutdown */
#endif

#ifdef H5_HAVE_MPE
hbool_t H5_MPEinit_g = FALSE;	/* MPE Library hasn't been initialized */
#endif

char                    H5_lib_vers_info_g[] = H5_VERS_INFO;
static hbool_t          H5_dont_atexit_g = FALSE;
H5_debug_t              H5_debug_g; /* debugging info */


/*******************/
/* Local Variables */
/*******************/


/*--------------------------------------------------------------------------
 * NAME
 *   H5_init_library -- Initialize library-global information
 * USAGE
 *    herr_t H5_init_library()
 *
 * RETURNS
 *    Non-negative on success/Negative on failure
 *
 * DESCRIPTION
 *    Initializes any library-global data or routines.
 *
 *--------------------------------------------------------------------------
 */
herr_t
H5_init_library(void)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

#ifdef H5_HAVE_PARALLEL
    {
	int mpi_initialized;
	int mpi_finalized;
        int mpi_code;

	MPI_Initialized(&mpi_initialized);
	MPI_Finalized(&mpi_finalized);

#ifdef H5_HAVE_MPE
        /* Initialize MPE instrumentation library. */
        if (!H5_MPEinit_g) {
            int mpe_code;
            if (mpi_initialized && !mpi_finalized) {
                mpe_code = MPE_Init_log();
                HDassert(mpe_code >=0);
                H5_MPEinit_g = TRUE;
            }
        }
#endif /*H5_HAVE_MPE*/

        /* add an attribute on MPI_COMM_SELF to call H5_term_library
           when it is destroyed, i.e. on MPI_Finalize */
        if (mpi_initialized && !mpi_finalized) {
            int key_val;

            if(MPI_SUCCESS != (mpi_code = MPI_Comm_create_keyval(MPI_COMM_NULL_COPY_FN,
                                                                 (MPI_Comm_delete_attr_function *)H5_mpi_delete_cb, 
                                                                 &key_val, NULL)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Comm_create_keyval failed", mpi_code)

            if(MPI_SUCCESS != (mpi_code = MPI_Comm_set_attr(MPI_COMM_SELF, key_val, NULL)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Comm_set_attr failed", mpi_code)

            if(MPI_SUCCESS != (mpi_code = MPI_Comm_free_keyval(&key_val)))
                HMPI_GOTO_ERROR(FAIL, "MPI_Comm_free_keyval failed", mpi_code)
        }
    }
#endif /*H5_HAVE_PARALLEL*/

    /*
     * Make sure the package information is updated.
     */
    HDmemset(&H5_debug_g, 0, sizeof H5_debug_g);
    H5_debug_g.pkg[H5_PKG_A].name = "a";
    H5_debug_g.pkg[H5_PKG_AC].name = "ac";
    H5_debug_g.pkg[H5_PKG_B].name = "b";
    H5_debug_g.pkg[H5_PKG_D].name = "d";
    H5_debug_g.pkg[H5_PKG_E].name = "e";
    H5_debug_g.pkg[H5_PKG_F].name = "f";
    H5_debug_g.pkg[H5_PKG_G].name = "g";
    H5_debug_g.pkg[H5_PKG_HG].name = "hg";
    H5_debug_g.pkg[H5_PKG_HL].name = "hl";
    H5_debug_g.pkg[H5_PKG_I].name = "i";
    H5_debug_g.pkg[H5_PKG_MF].name = "mf";
    H5_debug_g.pkg[H5_PKG_MM].name = "mm";
    H5_debug_g.pkg[H5_PKG_O].name = "o";
    H5_debug_g.pkg[H5_PKG_P].name = "p";
    H5_debug_g.pkg[H5_PKG_S].name = "s";
    H5_debug_g.pkg[H5_PKG_T].name = "t";
    H5_debug_g.pkg[H5_PKG_V].name = "v";
    H5_debug_g.pkg[H5_PKG_Z].name = "z";

    /*
     * Install atexit() library cleanup routines unless the H5dont_atexit()
     * has been called.  Once we add something to the atexit() list it stays
     * there permanently, so we set H5_dont_atexit_g after we add it to prevent
     * adding it again later if the library is cosed and reopened.
     */
    if (!H5_dont_atexit_g) {

#if defined(H5_HAVE_THREADSAFE) && defined(H5_HAVE_WIN_THREADS)
        /* Clean up Win32 thread resources. Pthreads automatically cleans up.
         * This must be entered before the library cleanup code so it's
         * executed in LIFO order (i.e., last).
         */
	    (void)HDatexit(H5TS_win32_process_exit);
#endif /* H5_HAVE_THREADSAFE && H5_HAVE_WIN_THREADS */

        /* Normal library termination code */
        (void)HDatexit(H5_term_library);

        H5_dont_atexit_g = TRUE;
    } /* end if */

    /*
     * Initialize interfaces that might not be able to initialize themselves
     * soon enough.  The file & dataset interfaces must be initialized because
     * calling H5P_create() might require the file/dataset property classes to be
     * initialized.  The property interface must be initialized before the file
     * & dataset interfaces though, in order to provide them with the proper
     * property classes.
     * The link interface needs to be initialized so that link property lists
     * have their properties registered.
     * The FS module needs to be initialized as a result of the fix for HDFFV-10160:
     *   It might not be initialized during normal file open. 
     *   When the application does not close the file, routines in the module might
     *   be called via H5_term_library() when shutting down the file.
     */
    if(H5E_init() < 0)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL, "unable to initialize error interface")
    if(H5P_init() < 0)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL, "unable to initialize property list interface")
    if(H5T_init() < 0)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL, "unable to initialize datatype interface")
    if(H5D_init() < 0)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL, "unable to initialize dataset interface")
    if(H5AC_init() < 0)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL, "unable to initialize metadata caching interface")
    if(H5L_init() < 0)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL, "unable to initialize link interface")
    if(H5FS_init() < 0)
        HGOTO_ERROR(H5E_FUNC, H5E_CANTINIT, FAIL, "unable to initialize FS interface")

    /* Debugging? */
    H5_debug_mask("-all");
    H5_debug_mask(HDgetenv("HDF5_DEBUG"));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5_init_library() */


/*-------------------------------------------------------------------------
 * Function:	H5_term_library
 *
 * Purpose:	Terminate interfaces in a well-defined order due to
 *		dependencies among the interfaces, then terminate
 *		library-specific data.
 *
 * Return:	void
 *
 *-------------------------------------------------------------------------
 */
void
H5_term_library(void)
{
    int	pending, ntries = 0, n;
    size_t	at = 0;
    char	loop[1024];
    H5E_auto2_t func;

#ifdef H5_HAVE_THREADSAFE
    /* explicit locking of the API */
    H5_FIRST_THREAD_INIT
    H5_API_LOCK
#endif

    /* Don't do anything if the library is already closed */
    if(!(H5_INIT_GLOBAL))
        goto done;

    /* Indicate that the library is being shut down */
    H5_TERM_GLOBAL = TRUE;

    /* Push the API context without checking for errors */
    H5CX_push_special();

    /* Check if we should display error output */
    (void)H5Eget_auto2(H5E_DEFAULT, &func, NULL);

    /*
     * Terminate each interface. The termination functions return a positive
     * value if they do something that might affect some other interface in a
     * way that would necessitate some cleanup work in the other interface.
     */
#define DOWN(F)								      \
    (((n = H5##F##_term_package()) && (at + 8) < sizeof loop)?	      \
     (HDsprintf(loop + at, "%s%s", (at ? "," : ""), #F),			      \
      at += HDstrlen(loop + at),					      \
      n):                                                                     \
     ((n > 0 && (at + 5) < sizeof loop) ?				      \
     (HDsprintf(loop + at, "..."),					      \
      at += HDstrlen(loop + at),					      \
     n) : n))

    do {
        pending = 0;

        /* Try to organize these so the "higher" level components get shut
         * down before "lower" level components that they might rely on. -QAK
         */
        pending += DOWN(L);

        /* Close the "top" of various interfaces (IDs, etc) but don't shut
         *  down the whole interface yet, so that the object header messages
         *  get serialized correctly for entries in the metadata cache and the
         *  symbol table entry in the superblock gets serialized correctly, etc.
         *  all of which is performed in the 'F' shutdown.
         */
        pending += DOWN(A_top);
        pending += DOWN(D_top);
        pending += DOWN(G_top);
        pending += DOWN(R_top);
        pending += DOWN(S_top);
        pending += DOWN(T_top);

        /* Don't shut down the file code until objects in files are shut down */
        if(pending == 0)
            pending += DOWN(F);

        /* Don't shut down the property list code until all objects that might
         * use property lists are shut down */
        if(pending == 0)
            pending += DOWN(P);

        /* Wait to shut down the "bottom" of various interfaces until the
         *      files are closed, so pieces of the file can be serialized
         *      correctly.
         */
        if(pending == 0) {
            /* Shut down the "bottom" of the attribute, dataset, group,
             *  reference, dataspace, and datatype interfaces, fully closing
             *  out the interfaces now.
             */
            pending += DOWN(A);
            pending += DOWN(D);
            pending += DOWN(G);
            pending += DOWN(R);
            pending += DOWN(S);
            pending += DOWN(T);
        } /* end if */

        /* Don't shut down "low-level" components until "high-level" components
         * have successfully shut down.  This prevents property lists and IDs
         * from being closed "out from underneath" of the high-level objects
         * that depend on them. -QAK
         */
        if(pending == 0) {
            pending += DOWN(AC);
            pending += DOWN(Z);
            pending += DOWN(FD);
            pending += DOWN(PL);
            /* Don't shut down the error code until other APIs which use it are shut down */
            if(pending == 0)
                pending += DOWN(E);
            /* Don't shut down the ID code until other APIs which use them are shut down */
            if(pending == 0)
                pending += DOWN(I);
            /* Don't shut down the skip list code until everything that uses it is down */
            if(pending == 0)
                pending += DOWN(SL);
            /* Don't shut down the free list code until everything that uses it is down */
            if(pending == 0)
                pending += DOWN(FL);
            /* Don't shut down the API context code until _everything_ else is down */
            if(pending == 0)
                pending += DOWN(CX);
        } /* end if */
    } while(pending && ntries++ < 100);

    if(pending) {
        /* Only display the error message if the user is interested in them. */
        if(func) {
            HDfprintf(stderr, "HDF5: infinite loop closing library\n");
            HDfprintf(stderr, "      %s\n", loop);
#ifndef NDEBUG
            HDabort();
#endif /* NDEBUG */
        } /* end if */
    } /* end if */

#ifdef H5_HAVE_MPE
    /* Close MPE instrumentation library.  May need to move this
     * down if any of the below code involves using the instrumentation code.
     */
    if(H5_MPEinit_g) {
        int mpi_initialized;
        int mpi_finalized;
        int mpe_code;

        MPI_Initialized(&mpi_initialized);
        MPI_Finalized(&mpi_finalized);

        if (mpi_initialized && !mpi_finalized) {
            mpe_code = MPE_Finish_log("h5log");
            HDassert(mpe_code >=0);
        } /* end if */
        H5_MPEinit_g = FALSE;	/* turn it off no matter what */
    } /* end if */
#endif

    /* Free open debugging streams */
    while(H5_debug_g.open_stream) {
        H5_debug_open_stream_t  *tmp_open_stream;

        tmp_open_stream = H5_debug_g.open_stream;
        (void)HDfclose(H5_debug_g.open_stream->stream);
        H5_debug_g.open_stream = H5_debug_g.open_stream->next;
        (void)H5MM_free(tmp_open_stream);
    } /* end while */

#if defined H5_MEMORY_ALLOC_SANITY_CHECK
    /* Sanity check memory allocations */
    H5MM_final_sanity_check();
#endif /* H5_MEMORY_ALLOC_SANITY_CHECK */

    /* Reset flag indicating that the library is being shut down */
    H5_TERM_GLOBAL = FALSE;

    /* Mark library as closed */
    H5_INIT_GLOBAL = FALSE;

    /* Don't pop the API context (i.e. H5CX_pop), since it's been shut down already */

done:
#ifdef H5_HAVE_THREADSAFE
    H5_API_UNLOCK
#endif /* H5_HAVE_THREADSAFE */

    return;
} /* end H5_term_library() */


/*-------------------------------------------------------------------------
 * Function:	H5dont_atexit
 *
 * Purpose:	Indicates that the library is not to clean up after itself
 *		when the application exits by calling exit() or returning
 *		from main().  This function must be called before any other
 *		HDF5 function or constant is used or it will have no effect.
 *
 *		If this function is used then certain memory buffers will not
 *		be de-allocated nor will open files be flushed automatically.
 *		The application may still call H5close() explicitly to
 *		accomplish these things.
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative if this function is called more than
 *				once or if it is called too late.
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5dont_atexit(void)
{
    herr_t ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_API_NOINIT_NOERR_NOFS
    H5TRACE0("e","");

    if(H5_dont_atexit_g)
        ret_value = FAIL;
    else
        H5_dont_atexit_g = TRUE;

    FUNC_LEAVE_API_NOFS(ret_value)
} /* end H5dont_atexit() */


/*-------------------------------------------------------------------------
 * Function:	H5garbage_collect
 *
 * Purpose:	Walks through all the garbage collection routines for the
 *		library, which are supposed to free any unused memory they have
 *		allocated.
 *
 *      These should probably be registered dynamically in a linked list of
 *          functions to call, but there aren't that many right now, so we
 *          hard-wire them...
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5garbage_collect(void)
{
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE0("e","");

    /* Call the garbage collection routines in the library */
    if(H5FL_garbage_coll()<0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGC, FAIL, "can't garbage collect objects")

done:
    FUNC_LEAVE_API(ret_value)
}   /* end H5garbage_collect() */


/*-------------------------------------------------------------------------
 * Function:	H5set_free_list_limits
 *
 * Purpose:	Sets limits on the different kinds of free lists.  Setting a value
 *      of -1 for a limit means no limit of that type.  These limits are global
 *      for the entire library.  Each "global" limit only applies to free lists
 *      of that type, so if an application sets a limit of 1 MB on each of the
 *      global lists, up to 3 MB of total storage might be allocated (1MB on
 *      each of regular, array and block type lists).
 *
 *      The settings for block free lists are duplicated to factory free lists.
 *      Factory free list limits cannot be set independently currently.
 *
 * Parameters:
 *  int reg_global_lim;  IN: The limit on all "regular" free list memory used
 *  int reg_list_lim;    IN: The limit on memory used in each "regular" free list
 *  int arr_global_lim;  IN: The limit on all "array" free list memory used
 *  int arr_list_lim;    IN: The limit on memory used in each "array" free list
 *  int blk_global_lim;  IN: The limit on all "block" free list memory used
 *  int blk_list_lim;    IN: The limit on memory used in each "block" free list
 *
 * Return:	Success:	non-negative
 *
 *		Failure:	negative
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5set_free_list_limits(int reg_global_lim, int reg_list_lim, int arr_global_lim,
    int arr_list_lim, int blk_global_lim, int blk_list_lim)
{
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE6("e", "IsIsIsIsIsIs", reg_global_lim, reg_list_lim, arr_global_lim,
             arr_list_lim, blk_global_lim, blk_list_lim);

    /* Call the free list function to actually set the limits */
    if(H5FL_set_free_list_limits(reg_global_lim, reg_list_lim, arr_global_lim, arr_list_lim,
            blk_global_lim, blk_list_lim, blk_global_lim, blk_list_lim)<0)
        HGOTO_ERROR(H5E_RESOURCE, H5E_CANTSET, FAIL, "can't set garbage collection limits")

done:
    FUNC_LEAVE_API(ret_value)
}   /* end H5set_free_list_limits() */


/*-------------------------------------------------------------------------
 * Function:    H5_debug_mask
 *
 * Purpose:     Set runtime debugging flags according to the string S.  The
 *              string should contain file numbers and package names
 *              separated by other characters. A file number applies to all
 *              following package names up to the next file number. The
 *              initial file number is `2' (the standard error stream). Each
 *              package name can be preceded by a `+' or `-' to add or remove
 *              the package from the debugging list (`+' is the default). The
 *              special name `all' means all packages.
 *
 *              The name `trace' indicates that API tracing is to be turned
 *              on or off.
 *
 *              The name 'ttop' indicates that only top-level API calls
 *              should be shown. This also turns on tracing as if the
 *              'trace' word was shown.
 *
 * Return:      void
 *
 *-------------------------------------------------------------------------
 */
static void
H5_debug_mask(const char *s)
{
    FILE	*stream = stderr;
    char	pkg_name[32], *rest;
    size_t	i;
    hbool_t	clear;

    while (s && *s) {

        if (HDisalpha(*s) || '-'==*s || '+'==*s) {

            /* Enable or Disable debugging? */
            if ('-'==*s) {
                clear = TRUE;
                s++;
            } else if ('+'==*s) {
                clear = FALSE;
                s++;
            } else {
                clear = FALSE;
            } /* end if */

            /* Get the name */
            for (i=0; HDisalpha(*s); i++, s++)
                if (i<sizeof pkg_name)
                    pkg_name[i] = *s;
            pkg_name[MIN(sizeof(pkg_name)-1, i)] = '\0';

            /* Trace, all, or one? */
            if (!HDstrcmp(pkg_name, "trace")) {
                H5_debug_g.trace = clear ? NULL : stream;
            } else if (!HDstrcmp(pkg_name, "ttop")) {
                H5_debug_g.trace = stream;
                H5_debug_g.ttop = (hbool_t)!clear;
            } else if (!HDstrcmp(pkg_name, "ttimes")) {
                H5_debug_g.trace = stream;
                H5_debug_g.ttimes = (hbool_t)!clear;
            } else if (!HDstrcmp(pkg_name, "all")) {
                for (i=0; i<(size_t)H5_NPKGS; i++)
                    H5_debug_g.pkg[i].stream = clear ? NULL : stream;
            } else {
                for (i=0; i<(size_t)H5_NPKGS; i++) {
                    if (!HDstrcmp(H5_debug_g.pkg[i].name, pkg_name)) {
                        H5_debug_g.pkg[i].stream = clear ? NULL : stream;
                        break;
		            } /* end if */
                } /* end for */
                if (i>=(size_t)H5_NPKGS)
                    HDfprintf(stderr, "HDF5_DEBUG: ignored %s\n", pkg_name);
            } /* end if-else */

        } else if (HDisdigit(*s)) {
            int fd = (int)HDstrtol(s, &rest, 0);
            H5_debug_open_stream_t *open_stream;

            if((stream = HDfdopen(fd, "w")) != NULL) {
                (void)HDsetvbuf(stream, NULL, _IOLBF, (size_t)0);

                if(NULL == (open_stream = (H5_debug_open_stream_t *)H5MM_malloc(sizeof(H5_debug_open_stream_t)))) {
                    (void)HDfclose(stream);
                    return;
                } /* end if */

                open_stream->stream = stream;
                open_stream->next = H5_debug_g.open_stream;
                H5_debug_g.open_stream = open_stream;
            } /* end if */

            s = rest;
        } else {
            s++;
        } /* end if-else */
    } /* end while */

    return;

} /* end H5_debug_mask() */

#ifdef H5_HAVE_PARALLEL

/*-------------------------------------------------------------------------
 * Function:	H5_mpi_delete_cb
 *
 * Purpose:	Callback attribute on MPI_COMM_SELF to terminate the HDF5 
 *              library when the communicator is destroyed, i.e. on MPI_Finalize.
 *
 * Return:	MPI_SUCCESS
 *
 *-------------------------------------------------------------------------
 */
static int H5_mpi_delete_cb(MPI_Comm H5_ATTR_UNUSED comm, int H5_ATTR_UNUSED keyval, void H5_ATTR_UNUSED *attr_val, int H5_ATTR_UNUSED *flag)
{
    H5_term_library();
    return MPI_SUCCESS;
}
#endif /*H5_HAVE_PARALLEL*/


/*-------------------------------------------------------------------------
 * Function:	H5get_libversion
 *
 * Purpose:	Returns the library version numbers through arguments. MAJNUM
 *		will be the major revision number of the library, MINNUM the
 *		minor revision number, and RELNUM the release revision number.
 *
 * Note:	When printing an HDF5 version number it should be printed as
 *
 * 		printf("%u.%u.%u", maj, min, rel)		or
 *		printf("version %u.%u release %u", maj, min, rel)
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5get_libversion(unsigned *majnum, unsigned *minnum, unsigned *relnum)
{
    herr_t                  ret_value = SUCCEED;

    FUNC_ENTER_API(FAIL)
    H5TRACE3("e", "*Iu*Iu*Iu", majnum, minnum, relnum);

    /* Set the version information */
    if (majnum) *majnum = H5_VERS_MAJOR;
    if (minnum) *minnum = H5_VERS_MINOR;
    if (relnum) *relnum = H5_VERS_RELEASE;

done:
    FUNC_LEAVE_API(ret_value)
} /* end H5get_libversion() */


/*-------------------------------------------------------------------------
 * Function:	H5check_version
 *
 * Purpose:	Verifies that the arguments match the version numbers
 *		compiled into the library.  This function is intended to be
 *		called from user to verify that the versions of header files
 *		compiled into the application match the version of the hdf5
 *		library.
 *
 * Return:	Success:	SUCCEED
 *
 *		Failure:	abort()
 *
 *-------------------------------------------------------------------------
 */
#define VERSION_MISMATCH_WARNING \
    "Warning! ***HDF5 library version mismatched error***\n" \
    "The HDF5 header files used to compile this application do not match\n" \
    "the version used by the HDF5 library to which this application is linked.\n" \
    "Data corruption or segmentation faults may occur if the application continues.\n" \
    "This can happen when an application was compiled by one version of HDF5 but\n" \
    "linked with a different version of static or shared HDF5 library.\n" \
    "You should recompile the application or check your shared library related\n" \
    "settings such as 'LD_LIBRARY_PATH'.\n"

herr_t
H5check_version(unsigned majnum, unsigned minnum, unsigned relnum)
{
    char	lib_str[256];
    char	substr[] = H5_VERS_SUBRELEASE;
    static int	checked = 0;            /* If we've already checked the version info */
    static unsigned int	disable_version_check = 0;      /* Set if the version check should be disabled */
    static const char *version_mismatch_warning = VERSION_MISMATCH_WARNING;
    herr_t      ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_API_NOINIT_NOERR_NOFS
    H5TRACE3("e", "IuIuIu", majnum, minnum, relnum);

    /* Don't check again, if we already have */
    if (checked)
	HGOTO_DONE(SUCCEED)

    {    const char *s;  /* Environment string for disabling version check */

        /* Allow different versions of the header files and library? */
        s = HDgetenv ("HDF5_DISABLE_VERSION_CHECK");

        if (s && HDisdigit(*s))
            disable_version_check = (unsigned int)HDstrtol (s, NULL, 0);
    }

    if (H5_VERS_MAJOR!=majnum || H5_VERS_MINOR!=minnum ||
            H5_VERS_RELEASE!=relnum) {
        switch (disable_version_check) {
	case 0:
	    HDfprintf(stderr, "%s%s", version_mismatch_warning,
		     "You can, at your own risk, disable this warning by setting the environment\n"
		     "variable 'HDF5_DISABLE_VERSION_CHECK' to a value of '1'.\n"
		     "Setting it to 2 or higher will suppress the warning messages totally.\n");
	    /* Mention the versions we are referring to */
	    HDfprintf (stderr, "Headers are %u.%u.%u, library is %u.%u.%u\n",
		     majnum, minnum, relnum,
		     (unsigned)H5_VERS_MAJOR, (unsigned)H5_VERS_MINOR, (unsigned)H5_VERS_RELEASE);
	    /* Show library settings if available */
	    HDfprintf (stderr, "%s", H5libhdf5_settings);

	    /* Bail out now. */
	    HDfputs ("Bye...\n", stderr);
	    HDabort ();
	case 1:
	    /* continue with a warning */
	    /* Note that the warning message is embedded in the format string.*/
            HDfprintf (stderr,
                     "%s'HDF5_DISABLE_VERSION_CHECK' "
                     "environment variable is set to %d, application will\n"
                     "continue at your own risk.\n",
		     version_mismatch_warning, disable_version_check);
	    /* Mention the versions we are referring to */
	    HDfprintf (stderr, "Headers are %u.%u.%u, library is %u.%u.%u\n",
		     majnum, minnum, relnum,
		     (unsigned)H5_VERS_MAJOR, (unsigned)H5_VERS_MINOR, (unsigned)H5_VERS_RELEASE);
	    /* Show library settings if available */
	    HDfprintf (stderr, "%s", H5libhdf5_settings);
	    break;
	default:
	    /* 2 or higher: continue silently */
	    break;
        } /* end switch */

    } /* end if */

    /* Indicate that the version check has been performed */
    checked = 1;

    if (!disable_version_check){
	/*
	 * Verify if H5_VERS_INFO is consistent with the other version information.
	 * Check only the first sizeof(lib_str) char.  Assume the information
	 * will fit within this size or enough significance.
	 */
	HDsnprintf(lib_str, sizeof(lib_str), "HDF5 library version: %d.%d.%d",
	    H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE);
	if(*substr) {
	    HDstrncat(lib_str, "-", (size_t)1);
	    HDstrncat(lib_str, substr, (sizeof(lib_str) - HDstrlen(lib_str)) - 1);
	} /* end if */
	if (HDstrcmp(lib_str, H5_lib_vers_info_g)){
	    HDfputs ("Warning!  Library version information error.\n"
		     "The HDF5 library version information are not "
		     "consistent in its source code.\nThis is NOT a fatal error "
		     "but should be corrected.  Setting the environment\n"
		     "variable 'HDF5_DISABLE_VERSION_CHECK' to a value of 1 "
		     "will suppress\nthis warning.\n",
		     stderr);
	    HDfprintf (stderr, "Library version information are:\n"
		     "H5_VERS_MAJOR=%d, H5_VERS_MINOR=%d, H5_VERS_RELEASE=%d, "
		     "H5_VERS_SUBRELEASE=%s,\nH5_VERS_INFO=%s\n",
		     H5_VERS_MAJOR, H5_VERS_MINOR, H5_VERS_RELEASE,
		     H5_VERS_SUBRELEASE, H5_VERS_INFO);
	} /* end if */
    }

done:
    FUNC_LEAVE_API_NOFS(ret_value)
} /* end H5check_version() */


/*-------------------------------------------------------------------------
 * Function:    H5open
 *
 * Purpose:     Initialize the library.  This is normally called
 *              automatically, but if you find that an HDF5 library function
 *              is failing inexplicably, then try calling this function
 *              first.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5open(void)
{
    herr_t ret_value=SUCCEED;   /* Return value */

    FUNC_ENTER_API_NOCLEAR(FAIL)
    H5TRACE0("e","");
    /* all work is done by FUNC_ENTER() */
done:
    FUNC_LEAVE_API(ret_value)
} /* end H5open() */


/*-------------------------------------------------------------------------
 * Function:	H5close
 *
 * Purpose:	Terminate the library and release all resources.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5close(void)
{
    /*
     * Don't call normal FUNC_ENTER() since we don't want to initialize the
     * whole library just to release it all right away.  It is safe to call
     * this function for an uninitialized library.
     */
    FUNC_ENTER_API_NOINIT_NOERR_NOFS
    H5TRACE0("e","");

    H5_term_library();

    FUNC_LEAVE_API_NOFS(SUCCEED)
} /* end H5close() */


/*-------------------------------------------------------------------------
 * Function:	H5allocate_memory
 *
 * Purpose:	    Allocate a memory buffer with the semantics of malloc().
 *
 *              NOTE: This function is intended for use with filter
 *              plugins so that all allocation and free operations
 *              use the same memory allocator. It is not intended for
 *              use as a general memory allocator in applications.
 *
 * Parameters:
 *
 *      size:   The size of the buffer.
 *
 *      clear:  Whether or not to memset the buffer to 0.
 *
 * Return:
 *
 *      Success:    A pointer to the allocated buffer.
 *  
 *      Failure:    NULL
 *
 *-------------------------------------------------------------------------
 */
void *
H5allocate_memory(size_t size, hbool_t clear)
{
    void *ret_value = NULL;

    FUNC_ENTER_API_NOINIT
    H5TRACE2("*x", "zb", size, clear);

    if(clear)
        ret_value = H5MM_calloc(size);
    else
        ret_value = H5MM_malloc(size);

    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5allocate_memory() */


/*-------------------------------------------------------------------------
 * Function:	H5resize_memory
 *
 * Purpose:	    Resize a memory buffer with the semantics of realloc().
 *
 *              NOTE: This function is intended for use with filter
 *              plugins so that all allocation and free operations
 *              use the same memory allocator. It is not intended for
 *              use as a general memory allocator in applications.
 *
 * Parameters:
 *
 *      mem:    The buffer to be resized.
 *
 *      size:   The size of the buffer.
 *
 * Return:
 *
 *      Success:    A pointer to the resized buffer.
 *  
 *      Failure:    NULL (the input buffer will be unchanged)
 *
 *-------------------------------------------------------------------------
 */
void *
H5resize_memory(void *mem, size_t size)
{
    void *ret_value = NULL;

    FUNC_ENTER_API_NOINIT
    H5TRACE2("*x", "*xz", mem, size);

    ret_value = H5MM_realloc(mem, size);

    FUNC_LEAVE_API_NOINIT(ret_value)
} /* end H5resize_memory() */


/*-------------------------------------------------------------------------
 * Function:	H5free_memory
 *
 * Purpose:	    Frees memory allocated by the library that it is the user's
 *              responsibility to free.  Ensures that the same library
 *              that was used to allocate the memory frees it.  Passing
 *              NULL pointers is allowed.
 *
 * Return:	    SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5free_memory(void *mem)
{
    FUNC_ENTER_API_NOINIT
    H5TRACE1("e", "*x", mem);

    /* At this time, it is impossible for this to fail. */
    H5MM_xfree(mem);

    FUNC_LEAVE_API_NOINIT(SUCCEED)
} /* end H5free_memory() */


/*-------------------------------------------------------------------------
 * Function:	H5is_library_threadsafe
 *
 * Purpose:	    Checks to see if the library was built with thread-safety
 *              enabled.
 *
 * Return:	    SUCCEED/FAIL
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5is_library_threadsafe(hbool_t *is_ts)
{
    FUNC_ENTER_API_NOINIT
    H5TRACE1("e", "*b", is_ts);

    HDassert(is_ts);
 
    /* At this time, it is impossible for this to fail. */
#ifdef H5_HAVE_THREADSAFE
    *is_ts = TRUE;
#else /* H5_HAVE_THREADSAFE */
    *is_ts = FALSE;
#endif /* H5_HAVE_THREADSAFE */

    FUNC_LEAVE_API_NOINIT(SUCCEED)
} /* end H5is_library_threadsafe() */


#if defined(H5_HAVE_THREADSAFE) && defined(H5_BUILT_AS_DYNAMIC_LIB) \
    && defined(H5_HAVE_WIN32_API) && defined(H5_HAVE_WIN_THREADS)
/*-------------------------------------------------------------------------
 * Function:    DllMain
 *
 * Purpose:     Handles various conditions in the library on Windows.
 *
 *    NOTE:     The main purpose of this is for handling Win32 thread cleanup
 *              on thread/process detach.
 *
 *              Only enabled when the shared Windows library is built with
 *              thread safety enabled.
 *
 * Return:      TRUE on success, FALSE on failure
 *
 *-------------------------------------------------------------------------
 */
BOOL WINAPI
DllMain(_In_ HINSTANCE hinstDLL, _In_ DWORD fdwReason, _In_ LPVOID lpvReserved)
{
    /* Don't add our function enter/leave macros since this function will be
     * called before the library is initialized.
     *
     * NOTE: Do NOT call any CRT functions in DllMain!
     * This includes any functions that are called by from here!
     */

    BOOL fOkay = TRUE;

    switch(fdwReason)
    {
    case DLL_PROCESS_ATTACH:
        break;

    case DLL_PROCESS_DETACH:
        break;

    case DLL_THREAD_ATTACH:
#ifdef H5_HAVE_WIN_THREADS
        if(H5TS_win32_thread_enter() < 0)
            fOkay = FALSE;
#endif /* H5_HAVE_WIN_THREADS */
        break;

    case DLL_THREAD_DETACH:
#ifdef H5_HAVE_WIN_THREADS
        if(H5TS_win32_thread_exit() < 0)
            fOkay = FALSE;
#endif /* H5_HAVE_WIN_THREADS */
        break;

    default:
        /* Shouldn't get here */
        fOkay = FALSE;
        break;
    }

    return fOkay;
}
#endif /* H5_HAVE_WIN32_API && H5_BUILT_AS_DYNAMIC_LIB && H5_HAVE_WIN_THREADS && H5_HAVE_THREADSAFE*/


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
 * Programmer:  Quincey Koziol
 *              Monday, July 31, 2006
 *
 * Purpose:     Free space tracking functions.
 *
 */

/****************/
/* Module Setup */
/****************/

#define H5F_FRIEND              /*suppress error about including H5Fpkg   */

#include "H5FSmodule.h"         /* This source code file is part of the H5FS module */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"             /* File access                          */
#include "H5FSpkg.h"		/* File free space			*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5VMprivate.h"		/* Vectors and arrays 			*/


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/* User data for skip list iterator callback for iterating over section size nodes */
typedef struct {
    H5FS_t *fspace;             /* Free space manager info */
    H5FS_operator_t op;         /* Operator for the iteration */
    void *op_data;              /* Information to pass to the operator */
} H5FS_iter_ud_t;


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/
static herr_t H5FS__sect_increase(H5FS_t *fspace, const H5FS_section_class_t *cls,
    unsigned flags);
static herr_t H5FS__sect_decrease(H5FS_t *fspace, const H5FS_section_class_t *cls);
static herr_t H5FS__size_node_decr(H5FS_sinfo_t *sinfo, unsigned bin, H5FS_node_t *fspace_node,
    const H5FS_section_class_t *cls);
static herr_t H5FS__sect_unlink_size(H5FS_sinfo_t *sinfo, const H5FS_section_class_t *cls,
    H5FS_section_info_t *sect);
static herr_t H5FS__sect_unlink_rest(H5FS_t *fspace,
    const H5FS_section_class_t *cls, H5FS_section_info_t *sect);
static herr_t H5FS__sect_remove_real(H5FS_t *fspace, H5FS_section_info_t *sect);
static herr_t H5FS__sect_link_size(H5FS_sinfo_t *sinfo, const H5FS_section_class_t *cls,
    H5FS_section_info_t *sect);
static herr_t H5FS__sect_link_rest(H5FS_t *fspace, const H5FS_section_class_t *cls,
    H5FS_section_info_t *sect, unsigned flags);
static herr_t H5FS__sect_link(H5FS_t *fspace, H5FS_section_info_t *sect,
    unsigned flags);
static herr_t H5FS__sect_merge(H5FS_t *fspace, H5FS_section_info_t **sect,
    void *op_data);
static htri_t H5FS__sect_find_node(H5FS_t *fspace, hsize_t request, H5FS_section_info_t **node);
static herr_t H5FS__sect_serialize_size(H5FS_t *fspace);


/*********************/
/* Package Variables */
/*********************/

/* Declare a free list to manage the H5FS_node_t struct */
H5FL_DEFINE(H5FS_node_t);

/* Declare a free list to manage the H5FS_bin_t sequence information */
H5FL_SEQ_DEFINE(H5FS_bin_t);

/* Declare a free list to manage the H5FS_sinfo_t struct */
H5FL_DEFINE(H5FS_sinfo_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5FS__sinfo_new
 *
 * Purpose:     Create new section info structure
 *
 * Return:      Success:    non-NULL, pointer to new section info struct
 *              Failure:    NULL
 *
 * Programmer:  Quincey Koziol
 *              Monday, July 31, 2006
 *
 *-------------------------------------------------------------------------
 */
H5FS_sinfo_t *
H5FS__sinfo_new(H5F_t *f, H5FS_t *fspace)
{
    H5FS_sinfo_t *sinfo = NULL;         /* Section information struct created */
    H5FS_sinfo_t *ret_value = NULL;     /* Return value */

    FUNC_ENTER_PACKAGE

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: fspace->addr = %a\n", FUNC, fspace->addr);
#endif /* H5FS_SINFO_DEBUG */

    /* Allocate the free space header */
    if(NULL == (sinfo = H5FL_CALLOC(H5FS_sinfo_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Set non-zero values */
    sinfo->nbins = H5VM_log2_gen(fspace->max_sect_size);
    sinfo->sect_prefix_size = H5FS_SINFO_PREFIX_SIZE(f);
    sinfo->sect_off_size = (fspace->max_sect_addr + 7) / 8;
    sinfo->sect_len_size = H5VM_limit_enc_size((uint64_t)fspace->max_sect_size);
#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: fspace->max_sect_size = %Hu\n", FUNC, fspace->max_sect_size);
HDfprintf(stderr, "%s: fspace->max_sect_addr = %u\n", FUNC, fspace->max_sect_addr);
HDfprintf(stderr, "%s: sinfo->nbins = %u\n", FUNC, sinfo->nbins);
HDfprintf(stderr, "%s: sinfo->sect_off_size = %u, sinfo->sect_len_size = %u\n", FUNC, sinfo->sect_off_size, sinfo->sect_len_size);
#endif /* H5FS_SINFO_DEBUG */

    /* Allocate space for the section size bins */
    if(NULL == (sinfo->bins = H5FL_SEQ_CALLOC(H5FS_bin_t, (size_t)sinfo->nbins)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed for free space section bin array")

    /* Increment the reference count on the free space manager header */
    if(H5FS__incr(fspace) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINC, NULL, "unable to increment ref. count on free space header")
    sinfo->fspace = fspace;

    /* Link free space manager to section info */
    /* (for deserializing sections) */
    HDassert(fspace->sinfo == NULL);
    fspace->sinfo = sinfo;

    /* Set return value */
    ret_value = sinfo;

done:
    if(ret_value == NULL && sinfo) {
        /* Release bins for skip lists */
        if(sinfo->bins)
            sinfo->bins = H5FL_SEQ_FREE(H5FS_bin_t, sinfo->bins);

        /* Release free space section info */
        sinfo = H5FL_FREE(H5FS_sinfo_t, sinfo);
    } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__sinfo_new() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__sinfo_lock
 *
 * Purpose:     Make certain the section info for the free space manager is
 *              in memory.
 *
 *              Either uses existing section info owned by the free space
 *              header, loads section info from disk, or creates new section
 *              info
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Thursday, February  7, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS__sinfo_lock(H5F_t *f, H5FS_t *fspace, unsigned accmode)
{
    H5FS_sinfo_cache_ud_t cache_udata; /* User-data for cache callback */
    herr_t ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_STATIC

#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: Called, fspace->addr = %a, fspace->sinfo = %p, fspace->sect_addr = %a\n", FUNC, fspace->addr, fspace->sinfo, fspace->sect_addr);
HDfprintf(stderr, "%s: fspace->alloc_sect_size = %Hu, fspace->sect_size = %Hu\n", FUNC, fspace->alloc_sect_size, fspace->sect_size);
#endif /* H5FS_SINFO_DEBUG */

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);

    /* only H5AC__READ_ONLY_FLAG may appear in accmode */
    HDassert((accmode & (unsigned)(~H5AC__READ_ONLY_FLAG)) == 0);

    /* If the free space header doesn't already "own" the section info, load
     *  section info or create it
     */
    if(fspace->sinfo) {
        /* Check if the section info was protected & we want a different access mode */

        /* only H5AC__READ_ONLY_FLAG may appear in fspace->sinfo_accmode */
        HDassert(((fspace->sinfo_accmode) & (unsigned)(~H5AC__READ_ONLY_FLAG)) == 0);

        if(fspace->sinfo_protected && accmode != fspace->sinfo_accmode) {
            /* Check if we need to switch from read-only access to read-write */
	    if(0 == (accmode & (unsigned)(~H5AC__READ_ONLY_FLAG))) {
                /* Unprotect the read-only section info */
                if(H5AC_unprotect(f, H5AC_FSPACE_SINFO, fspace->sect_addr, fspace->sinfo, H5AC__NO_FLAGS_SET) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTUNPROTECT, FAIL, "unable to release free space section info")

                /* Re-protect the section info with read-write access */
                cache_udata.f = f;
                cache_udata.fspace = fspace;
                if(NULL == (fspace->sinfo = (H5FS_sinfo_t *)H5AC_protect(f, H5AC_FSPACE_SINFO, fspace->sect_addr, &cache_udata, H5AC__NO_FLAGS_SET)))
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTPROTECT, FAIL, "unable to load free space sections")

                /* Switch the access mode we have */
                fspace->sinfo_accmode = H5AC__NO_FLAGS_SET;
            } /* end if */
        } /* end if */
    } /* end if */
    else {
        /* If the section address is defined, load it from the file */
        if(H5F_addr_defined(fspace->sect_addr)) {
            /* Sanity check */
            HDassert(fspace->sinfo_protected == FALSE);
            HDassert(H5F_addr_defined(fspace->addr));

#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: Reading in existing sections, fspace->sect_addr = %a\n", FUNC, fspace->sect_addr);
#endif /* H5FS_SINFO_DEBUG */
            /* Protect the free space sections */
            cache_udata.f = f;
            cache_udata.fspace = fspace;
            if(NULL == (fspace->sinfo = (H5FS_sinfo_t *)H5AC_protect(f, H5AC_FSPACE_SINFO, fspace->sect_addr, &cache_udata, accmode)))
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTPROTECT, FAIL, "unable to load free space sections")

            /* Remember that we protected the section info & the access mode */
            fspace->sinfo_protected = TRUE;
            fspace->sinfo_accmode = accmode;
        } /* end if */
        else {
#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: Creating new section info\n", FUNC);
#endif /* H5FS_SINFO_DEBUG */
            /* Sanity check */
            HDassert(fspace->tot_sect_count == 0);
            HDassert(fspace->serial_sect_count == 0);
            HDassert(fspace->ghost_sect_count == 0);

            /* Allocate and initialize free space section info */
            if(NULL == (fspace->sinfo = H5FS__sinfo_new(f, fspace)))
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, FAIL, "can't create section info")

            /* Set initial size of section info to 0 */
            fspace->sect_size = fspace->alloc_sect_size = 0;
        } /* end if */
    } /* end if */
    HDassert(fspace->rc == 2);

    /* Increment the section info lock count */
    fspace->sinfo_lock_count++;

done:
#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: Leaving, fspace->addr = %a, fspace->sinfo = %p, fspace->sect_addr = %a\n", FUNC, fspace->addr, fspace->sinfo, fspace->sect_addr);
HDfprintf(stderr, "%s: fspace->alloc_sect_size = %Hu, fspace->sect_size = %Hu\n", FUNC, fspace->alloc_sect_size, fspace->sect_size);
#endif /* H5FS_SINFO_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__sinfo_lock() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__sinfo_unlock
 *
 * Purpose:     Release the section info, either giving ownership back to
 *              the cache or letting the free space header keep it.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Thursday, February  7, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS__sinfo_unlock(H5F_t *f, H5FS_t *fspace, hbool_t modified)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC
#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: Called, modified = %t, fspace->addr = %a, fspace->sect_addr = %a\n", FUNC, modified, fspace->addr, fspace->sect_addr);
HDfprintf(stderr, "%s: fspace->sinfo_lock_count = %u, fspace->sinfo_modified = %t, fspace->sinfo_protected = %t\n", FUNC, fspace->sinfo_lock_count, fspace->sinfo_modified, fspace->sinfo_protected);
HDfprintf(stderr, "%s: fspace->alloc_sect_size = %Hu, fspace->sect_size = %Hu\n", FUNC, fspace->alloc_sect_size, fspace->sect_size);
#endif /* H5FS_SINFO_DEBUG */

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(fspace->rc == 2);
    HDassert(fspace->sinfo);

    /* Check if we modified any section */
    if(modified) {
        /* Check if the section info was protected with a different access mode */
        if(fspace->sinfo_protected && (0 != ((fspace->sinfo_accmode) & H5AC__READ_ONLY_FLAG)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTDIRTY, FAIL, "attempt to modify read-only section info")

        /* If we modified the section info, mark it dirty */
        fspace->sinfo->dirty = TRUE;

        /* Remember that the section info was modified while locked */
        fspace->sinfo_modified = TRUE;

        /* Assume that the modification will affect the statistics in the header
         *  and mark that dirty also
         */
        if(H5FS__dirty(fspace) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, FAIL, "unable to mark free space header as dirty")
    } /* end if */

    /* Decrement the lock count on the section info */
    fspace->sinfo_lock_count--;

    /* Check if section info lock count dropped to zero */
    if(fspace->sinfo_lock_count == 0) {
        hbool_t release_sinfo_space = FALSE;    /* Flag to indicate section info space in file should be released */

        /* Check if we actually protected the section info */
        if(fspace->sinfo_protected) {
            unsigned    cache_flags = H5AC__NO_FLAGS_SET;       /* Flags for unprotecting heap */

            /* Sanity check */
            HDassert(H5F_addr_defined(fspace->addr));

            /* Check if we've made new changes to the section info while locked */
            if(fspace->sinfo_modified) {
                /* Note that we've modified the section info */
                cache_flags |= H5AC__DIRTIED_FLAG;

                /* Check if the section info size in the file has changed */
                if(fspace->sect_size != fspace->alloc_sect_size)
                    cache_flags |= H5AC__DELETED_FLAG | H5AC__TAKE_OWNERSHIP_FLAG;

            } /* end if */

            /* Sanity check */
            HDassert(H5F_addr_defined(fspace->sect_addr));

            /* Unprotect section info in cache */
            /* (Possibly dirty) */
            /* (Possibly taking ownership from the cache) */
#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: Unprotecting section info, cache_flags = %u\n", FUNC, cache_flags);
#endif /* H5FS_SINFO_DEBUG */
            if(H5AC_unprotect(f, H5AC_FSPACE_SINFO, fspace->sect_addr, fspace->sinfo, cache_flags) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTUNPROTECT, FAIL, "unable to release free space section info")

            /* Reset the protected flag on the section info */
            fspace->sinfo_protected = FALSE;

            /* Check if header is taking ownership of section info */
            if((cache_flags & H5AC__TAKE_OWNERSHIP_FLAG)) {
#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: Taking ownership of section info\n", FUNC);
#endif /* H5FS_SINFO_DEBUG */
                /* Set flag to release section info space in file */
                release_sinfo_space = TRUE;
            } /* end if */
            else {
#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: Relinquishing section info ownership\n", FUNC);
#endif /* H5FS_SINFO_DEBUG */
                /* Free space header relinquished ownership of section info */
                fspace->sinfo = NULL;
            } /* end else */
        } /* end if */
        else {
            /* Check if the section info was modified */
            if(fspace->sinfo_modified) {
                /* Check if we need to release section info in the file */
                if(H5F_addr_defined(fspace->sect_addr))
                    /* Set flag to release section info space in file */
                    release_sinfo_space = TRUE;
                else
                    HDassert(fspace->alloc_sect_size == 0);
            } /* end if */
            else {
                /* Sanity checks... */
                if(H5F_addr_defined(fspace->sect_addr))
                    HDassert(fspace->alloc_sect_size == fspace->sect_size);
                else
                    HDassert(fspace->alloc_sect_size == 0);
            } /* end else */
        } /* end else */

        /* Reset the "section info modified" flag */
        fspace->sinfo_modified = FALSE;

        /* Check if header needs to release section info in the file */
        if(release_sinfo_space) {
            haddr_t old_sect_addr = fspace->sect_addr;   /* Previous location of section info in file */
            hsize_t old_alloc_sect_size = fspace->alloc_sect_size;       /* Previous size of section info in file */

            /* Sanity check */
            HDassert(H5F_addr_defined(fspace->addr));

            /* Reset section info in header */
            fspace->sect_addr = HADDR_UNDEF;
            fspace->alloc_sect_size = 0;

            /* If we haven't already marked the header dirty, do so now */
            if(!modified)
                if(H5FS__dirty(fspace) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, FAIL, "unable to mark free space header as dirty")

#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: Freeing section info on disk, old_sect_addr = %a, old_alloc_sect_size = %Hu\n", FUNC, old_sect_addr, old_alloc_sect_size);
#endif /* H5FS_SINFO_DEBUG */
            /* Release space for section info in file */
            if(!H5F_IS_TMP_ADDR(f, old_sect_addr))
                if(H5MF_xfree(f, H5FD_MEM_FSPACE_SINFO, old_sect_addr, old_alloc_sect_size) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "unable to free free space sections")
        } /* end if */
    } /* end if */

done:
#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: Leaving, ret_value = %d\n", FUNC, ret_value);
#endif /* H5FS_SINFO_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__sinfo_unlock() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__sect_serialize_size
 *
 * Purpose:     Determine serialized size of all sections in free space manager
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS__sect_serialize_size(H5FS_t *fspace)
{
    FUNC_ENTER_STATIC_NOERR

    /* Check arguments. */
    HDassert(fspace);

    /* Compute the size of the buffer required to serialize all the sections */
    if(fspace->serial_sect_count > 0) {
        size_t sect_buf_size;               /* Section buffer size */

        /* Serialized sections prefix */
        sect_buf_size = fspace->sinfo->sect_prefix_size;

        /* Count for each differently sized serializable section */
        sect_buf_size += fspace->sinfo->serial_size_count * H5VM_limit_enc_size((uint64_t)fspace->serial_sect_count);

        /* Size for each differently sized serializable section */
        sect_buf_size += fspace->sinfo->serial_size_count * fspace->sinfo->sect_len_size;

        /* Offsets of each section in address space */
        sect_buf_size += fspace->serial_sect_count * fspace->sinfo->sect_off_size;

        /* Class of each section */
        sect_buf_size += fspace->serial_sect_count * 1 /* byte */;

        /* Extra space required to serialize each section */
        sect_buf_size += fspace->sinfo->serial_size;

        /* Update section size in header */
        fspace->sect_size = sect_buf_size;
    } /* end if */
    else
        /* Reset section size in header */
        fspace->sect_size = fspace->sinfo->sect_prefix_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5FS__sect_serialize_size() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__sect_increase
 *
 * Purpose:     Increase the size of the serialized free space section info
 *              on disk
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS__sect_increase(H5FS_t *fspace, const H5FS_section_class_t *cls,
    unsigned flags)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(cls);

    /* Increment total # of sections on free space list */
    fspace->tot_sect_count++;

    /* Check for serializable or 'ghost' section */
    if(cls->flags & H5FS_CLS_GHOST_OBJ) {
        /* Sanity check */
        HDassert(cls->serial_size == 0);

        /* Increment # of ghost sections */
        fspace->ghost_sect_count++;
    } /* end if */
    else {
        /* Increment # of serializable sections */
        fspace->serial_sect_count++;

        /* Increment amount of space required to serialize all sections */
        fspace->sinfo->serial_size += cls->serial_size;

        /* Update the free space sections' serialized size */
        /* (if we're not deserializing the sections from disk) */
        if(!(flags & H5FS_ADD_DESERIALIZING)) {
            if(H5FS__sect_serialize_size(fspace) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTCOMPUTE, FAIL, "can't adjust free space section size on disk")
        } /* end if */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__sect_increase() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__sect_decrease
 *
 * Purpose:     Decrease the size of the serialized free space section info
 *              on disk
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Monday, May  8, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS__sect_decrease(H5FS_t *fspace, const H5FS_section_class_t *cls)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(cls);

    /* Decrement total # of sections in free space manager */
    fspace->tot_sect_count--;

    /* Check for serializable or 'ghost' section */
    if(cls->flags & H5FS_CLS_GHOST_OBJ) {
        /* Sanity check */
        HDassert(cls->serial_size == 0);

        /* Decrement # of ghost sections */
        fspace->ghost_sect_count--;
    } /* end if */
    else {
        /* Decrement # of serializable sections */
        fspace->serial_sect_count--;

        /* Decrement amount of space required to serialize all sections */
        fspace->sinfo->serial_size -= cls->serial_size;

        /* Update the free space sections' serialized size */
        if(H5FS__sect_serialize_size(fspace) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCOMPUTE, FAIL, "can't adjust free space section size on disk")
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__sect_decrease() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__size_node_decr
 *
 * Purpose:     Decrement the number of sections of a particular size
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS__size_node_decr(H5FS_sinfo_t *sinfo, unsigned bin, H5FS_node_t *fspace_node,
    const H5FS_section_class_t *cls)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(sinfo);
    HDassert(fspace_node);
    HDassert(cls);

    /* Decrement the # of sections in this bin */
    /* (Different from the # of items in the bin's skiplist, since each node on
     *  the bin's skiplist is also a skiplist...)
     */
    sinfo->bins[bin].tot_sect_count--;

    /* Check for 'ghost' or 'serializable' section */
    if(cls->flags & H5FS_CLS_GHOST_OBJ) {
        /* Decrement node's ghost section count */
        fspace_node->ghost_count--;

        /* Decrement bin's ghost section count */
        sinfo->bins[bin].ghost_sect_count--;

        /* If the node has no more ghost sections, decrement number of ghost section sizes managed */
        if(fspace_node->ghost_count == 0)
            sinfo->ghost_size_count--;
    } /* end if */
    else {
        /* Decrement node's serializable section count */
        fspace_node->serial_count--;

        /* Decrement bin's serializable section count */
        sinfo->bins[bin].serial_sect_count--;

        /* If the node has no more serializable sections, decrement number of serializable section sizes managed */
        if(fspace_node->serial_count == 0)
            sinfo->serial_size_count--;
    } /* end else */

    /* Check for no more nodes on list of that size */
    if(H5SL_count(fspace_node->sect_list) == 0) {
        H5FS_node_t *tmp_fspace_node;       /* Free space list size node */

        /* Sanity checks */
        HDassert(fspace_node->ghost_count == 0);
        HDassert(fspace_node->serial_count == 0);

        /* Remove size tracking list from bin */
        tmp_fspace_node = (H5FS_node_t *)H5SL_remove(sinfo->bins[bin].bin_list, &fspace_node->sect_size);
        if(tmp_fspace_node == NULL || tmp_fspace_node != fspace_node)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space node from skip list")

        /* Destroy skip list for size tracking node */
        if(H5SL_close(fspace_node->sect_list) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCLOSEOBJ, FAIL, "can't destroy size tracking node's skip list")

        /* Release free space list node */
        fspace_node = H5FL_FREE(H5FS_node_t, fspace_node);

        /* Decrement total number of section sizes managed */
        sinfo->tot_size_count--;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__size_node_decr() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__sect_unlink_size
 *
 * Purpose:     Remove a section node from size tracking data structures for
 *              a free space manager
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS__sect_unlink_size(H5FS_sinfo_t *sinfo, const H5FS_section_class_t *cls,
    H5FS_section_info_t *sect)
{
    H5FS_node_t *fspace_node;       /* Free list size node */
    H5FS_section_info_t *tmp_sect_node; /* Temporary section node */
    unsigned bin;                   /* Bin to put the free space section in */
    herr_t ret_value = SUCCEED;     /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(sinfo);
    HDassert(sinfo->bins);
    HDassert(sect);
    HDassert(cls);

    /* Determine correct bin which holds items of at least the section's size */
    bin = H5VM_log2_gen(sect->size);
    HDassert(bin < sinfo->nbins);
    if(sinfo->bins[bin].bin_list == NULL)
        HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "node's bin is empty?")

    /* Find space node for section's size */
    if((fspace_node = (H5FS_node_t *)H5SL_search(sinfo->bins[bin].bin_list, &sect->size)) == NULL)
        HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "can't find section size node")

    /* Remove the section's node from the list */
    tmp_sect_node = (H5FS_section_info_t *)H5SL_remove(fspace_node->sect_list, &sect->addr);
    if(tmp_sect_node == NULL || tmp_sect_node != sect)
        HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "can't find section node on size list")

    /* Decrement # of sections in section size node */
    if(H5FS__size_node_decr(sinfo, bin, fspace_node, cls) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space size node from skip list")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__sect_unlink_size() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__sect_unlink_rest
 *
 * Purpose:     Finish unlinking a section from the rest of the free space
 *              manager's data structures, after the section has been removed
 *              from the size tracking data structures
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS__sect_unlink_rest(H5FS_t *fspace, const H5FS_section_class_t *cls,
    H5FS_section_info_t *sect)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(cls);
    HDassert(sect);

    /* Remove node from merge list, if it was entered there */
    if(!(cls->flags & H5FS_CLS_SEPAR_OBJ)) {
        H5FS_section_info_t *tmp_sect_node; /* Temporary section node */

        tmp_sect_node = (H5FS_section_info_t *)H5SL_remove(fspace->sinfo->merge_list, &sect->addr);
        if(tmp_sect_node == NULL || tmp_sect_node != sect)
            HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "can't find section node on size list")
    } /* end if */

    /* Update section info & check if we need less room for the serialized free space sections */
    if(H5FS__sect_decrease(fspace, cls) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't increase free space section size on disk")

    /* Decrement amount of free space managed */
    fspace->tot_space -= sect->size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__sect_unlink_rest() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__sect_remove_real
 *
 * Purpose:     Remove a section from the free space manager
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS__sect_remove_real(H5FS_t *fspace, H5FS_section_info_t *sect)
{
    const H5FS_section_class_t *cls;    /* Class of section */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(sect);

    /* Get section's class */
    cls = &fspace->sect_cls[sect->type];

    /* Remove node from size tracked data structures */
    if(H5FS__sect_unlink_size(fspace->sinfo, cls, sect) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't remove section from size tracking data structures")

    /* Update rest of free space manager data structures for node removal */
    if(H5FS__sect_unlink_rest(fspace, cls, sect) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't remove section from non-size tracking data structures")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__sect_remove_real() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_sect_remove
 *
 * Purpose:     Remove a section from the free space manager
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_sect_remove(H5F_t *f, H5FS_t *fspace, H5FS_section_info_t *sect)
{
    hbool_t sinfo_valid = FALSE;        /* Whether the section info is valid */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(sect);

    /* Get a pointer to the section info */
    if(H5FS__sinfo_lock(f, fspace, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTGET, FAIL, "can't get section info")
    sinfo_valid = TRUE;

    /* Perform actual section removal */
    if(H5FS__sect_remove_real(fspace, sect) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove section")

done:
    /* Release the section info */
    if(sinfo_valid && H5FS__sinfo_unlock(f, fspace, TRUE) < 0)
        HDONE_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't release section info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_remove() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__sect_link_size
 *
 * Purpose:     Add a section of free space to the free list bins
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Monday, March 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS__sect_link_size(H5FS_sinfo_t *sinfo, const H5FS_section_class_t *cls,
    H5FS_section_info_t *sect)
{
    H5FS_node_t *fspace_node = NULL;    /* Pointer to free space node of the correct size */
    hbool_t fspace_node_alloc = FALSE;  /* Whether the free space node was allocated */
    unsigned bin;                       /* Bin to put the free space section in */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(sinfo);
    HDassert(sect);
    HDassert(H5F_addr_defined(sect->addr));
    HDassert(sect->size);

    /* Determine correct bin which holds items of the section's size */
    bin = H5VM_log2_gen(sect->size);
    HDassert(bin < sinfo->nbins);
    if(sinfo->bins[bin].bin_list == NULL) {
        if(NULL == (sinfo->bins[bin].bin_list = H5SL_create(H5SL_TYPE_HSIZE, NULL)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, FAIL, "can't create skip list for free space nodes")
    } /* end if */
    else
        /* Check for node list of the correct size already */
        fspace_node = (H5FS_node_t *)H5SL_search(sinfo->bins[bin].bin_list, &sect->size);

    /* Check if we need to create a new skip list for nodes of this size */
    if(fspace_node == NULL) {
        /* Allocate new free list size node */
        if(NULL == (fspace_node = H5FL_MALLOC(H5FS_node_t)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, FAIL, "memory allocation failed for free space node")
        fspace_node_alloc = TRUE;

        /* Initialize the free list size node */
        fspace_node->sect_size = sect->size;
        fspace_node->serial_count = fspace_node->ghost_count = 0;
        if(NULL == (fspace_node->sect_list = H5SL_create(H5SL_TYPE_HADDR, NULL)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, FAIL, "can't create skip list for free space nodes")

        /* Insert new free space size node into bin's list */
        if(H5SL_insert(sinfo->bins[bin].bin_list, fspace_node, &fspace_node->sect_size) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space node into skip list")
        fspace_node_alloc = FALSE; /* (owned by the bin skip list now, don't need to free on error) */

        /* Increment number of section sizes */
        sinfo->tot_size_count++;
    } /* end if */

    /* Increment # of section in bin */
    /* (Different from the # of items in the bin's skiplist, since each node on
     *  the bin's skiplist is also a skiplist...)
     */
    sinfo->bins[bin].tot_sect_count++;
    if(cls->flags & H5FS_CLS_GHOST_OBJ) {
        sinfo->bins[bin].ghost_sect_count++;
        fspace_node->ghost_count++;

        /* Check for first ghost section in node */
        if(fspace_node->ghost_count == 1)
            sinfo->ghost_size_count++;
    } /* end if */
    else {
        sinfo->bins[bin].serial_sect_count++;
        fspace_node->serial_count++;

        /* Check for first serializable section in node */
        if(fspace_node->serial_count == 1)
            sinfo->serial_size_count++;
    } /* end else */

    /* Insert free space node into correct skip list */
    if(H5SL_insert(fspace_node->sect_list, sect, &sect->addr) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space node into skip list")

done:
    if(ret_value < 0)
        if(fspace_node && fspace_node_alloc) {
            if(fspace_node->sect_list && H5SL_close(fspace_node->sect_list) < 0)
                HDONE_ERROR(H5E_FSPACE, H5E_CANTCLOSEOBJ, FAIL, "can't destroy size free space node's skip list")
            fspace_node = H5FL_FREE(H5FS_node_t, fspace_node);
        } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__sect_link_size() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__sect_link_rest
 *
 * Purpose:     Link a section into the rest of the non-size tracking
 *              free space manager data structures
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS__sect_link_rest(H5FS_t *fspace, const H5FS_section_class_t *cls,
    H5FS_section_info_t *sect, unsigned flags)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(sect);

    /* Add section to the address-ordered list of sections, if allowed */
    if(!(cls->flags & H5FS_CLS_SEPAR_OBJ)) {
        if(fspace->sinfo->merge_list == NULL)
            if(NULL == (fspace->sinfo->merge_list = H5SL_create(H5SL_TYPE_HADDR, NULL)))
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, FAIL, "can't create skip list for merging free space sections")
        if(H5SL_insert(fspace->sinfo->merge_list, sect, &sect->addr) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space node into merging skip list")
    } /* end if */

    /* Update section info & check if we need more room for the serialized free space sections */
    if(H5FS__sect_increase(fspace, cls, flags) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't increase free space section size on disk")

    /* Increment amount of free space managed */
    fspace->tot_space += sect->size;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__sect_link_rest() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__sect_link
 *
 * Purpose:     Link a section into the internal data structures
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS__sect_link(H5FS_t *fspace, H5FS_section_info_t *sect, unsigned flags)
{
    const H5FS_section_class_t *cls;    /* Class of section */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(sect);

    /* Get section's class */
    cls = &fspace->sect_cls[sect->type];

    /* Add section to size tracked data structures */
    if(H5FS__sect_link_size(fspace->sinfo, cls, sect) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't add section to size tracking data structures")

    /* Update rest of free space manager data structures for section addition */
    if(H5FS__sect_link_rest(fspace, cls, sect, flags) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't add section to non-size tracking data structures")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__sect_link() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__sect_merge
 *
 * Purpose:     Attempt to merge a returned free space section with existing
 *              free space.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Wednesday, May 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS__sect_merge(H5FS_t *fspace, H5FS_section_info_t **sect, void *op_data)
{
    H5FS_section_class_t *sect_cls;     /* Section's class */
    hbool_t modified;                   /* Flag to indicate merge or shrink occurred */
    hbool_t remove_sect = FALSE;        /* Whether a section should be removed before shrinking */
    htri_t status;                      /* Status value */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(fspace);
    HDassert(*sect);
    HDassert(H5F_addr_defined((*sect)->addr));
    HDassert((*sect)->size);

    /* Loop until no more merging */
    if(fspace->sinfo->merge_list) {
        do {
            H5SL_node_t *less_sect_node;    /* Skip list node for section less than new section */
            H5SL_node_t *greater_sect_node; /* Skip list node for section greater than new section */
            H5FS_section_info_t *tmp_sect;  /* Temporary free space section */
            H5FS_section_class_t *tmp_sect_cls;     /* Temporary section's class */
            hbool_t greater_sect_node_valid = FALSE;    /* Indicate if 'greater than' section node is valid */

            /* Reset 'modification occurred' flag */
            modified = FALSE;

            /* Look for neighboring section before new section */
            less_sect_node = H5SL_below(fspace->sinfo->merge_list, &(*sect)->addr);

            /* Check for node before new node able to merge with new node */
            if(less_sect_node) {
                /* Check for node greater than section */
                greater_sect_node = H5SL_next(less_sect_node);
                greater_sect_node_valid = TRUE;

                /* Get section for 'less than' skip list node */
                tmp_sect = (H5FS_section_info_t *)H5SL_item(less_sect_node);

                /* Get classes for right & left sections */
                tmp_sect_cls = &fspace->sect_cls[tmp_sect->type];
                sect_cls = &fspace->sect_cls[(*sect)->type];

                /* Check if sections of the left most class can merge with sections
                 *  of another class & whether the sections are the same type,
                 *  then check for 'can merge' callback
                 */
                if((!(tmp_sect_cls->flags & H5FS_CLS_MERGE_SYM) || (tmp_sect->type == (*sect)->type))
                        && tmp_sect_cls->can_merge) {
                    /* Determine if the sections can merge */
                    if((status = (*tmp_sect_cls->can_merge)(tmp_sect, *sect, op_data)) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTMERGE, FAIL, "can't check for merging sections")
                    if(status > 0) {
                        /* Sanity check */
                        HDassert(tmp_sect_cls->merge);

                        /* Remove 'less than' node from data structures */
                        if(H5FS__sect_remove_real(fspace, tmp_sect) < 0)
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't remove section from internal data structures")

                        /* Merge the two sections together */
                        if((*tmp_sect_cls->merge)(&tmp_sect, *sect, op_data) < 0)
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't merge two sections")

                        /* Retarget section pointer to 'less than' node that was merged into */
                        *sect = tmp_sect;
			if(*sect == NULL)
			    HGOTO_DONE(ret_value);

                        /* Indicate successful merge occurred */
                        modified = TRUE;
                    } /* end if */
                } /* end if */
            } /* end if */

            /* Look for section after new (or merged) section, if not already determined */
            if(!greater_sect_node_valid)
                greater_sect_node = H5SL_above(fspace->sinfo->merge_list, &(*sect)->addr);

            /* Check for node after new node able to merge with new node */
            if(greater_sect_node) {
                /* Get section for 'greater than' skip list node */
                tmp_sect = (H5FS_section_info_t *)H5SL_item(greater_sect_node);

                /* Get classes for right & left sections */
                sect_cls = &fspace->sect_cls[(*sect)->type];
                tmp_sect_cls = &fspace->sect_cls[tmp_sect->type];

                /* Check if sections of the left most class can merge with sections
                 *  of another class & whether the sections are the same type,
                 *  then check for 'can merge' callback
                 */
                if((!(sect_cls->flags & H5FS_CLS_MERGE_SYM) || ((*sect)->type == tmp_sect->type))
                        && sect_cls->can_merge) {

                    /* Determine if the sections can merge */
                    if((status = (*sect_cls->can_merge)(*sect, tmp_sect, op_data)) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTMERGE, FAIL, "can't check for merging sections")
                    if(status > 0) {
                        /* Sanity check */
                        HDassert(sect_cls->merge);

                        /* Remove 'greater than' node from data structures */
                        if(H5FS__sect_remove_real(fspace, tmp_sect) < 0)
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't remove section from internal data structures")

                        /* Merge the two sections together */
                        if((*sect_cls->merge)(sect, tmp_sect, op_data) < 0)
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't merge two sections")

                        /* It's possible that the merge caused the section to be deleted (particularly in the paged allocation case) */
                        if(*sect == NULL)
                            HGOTO_DONE(ret_value);

                        /* Indicate successful merge occurred */
                        modified = TRUE;
                    } /* end if */
                } /* end if */
            } /* end if */
	} while(modified);
    } /* end if */
    HDassert(*sect);

    /* Loop until no more shrinking */
    do {
        /* Reset 'modification occurred' flag */
        modified = FALSE;

        /* Check for (possibly merged) section able to shrink the size of the container */
        sect_cls = &fspace->sect_cls[(*sect)->type];
        if(sect_cls->can_shrink) {
            if((status = (*sect_cls->can_shrink)(*sect, op_data)) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTSHRINK, FAIL, "can't check for shrinking container")
            if(status > 0) {
                /* Remove SECT from free-space manager */
                /* (only possible to happen on second+ pass through loop) */
                if(remove_sect) {
                    if(H5FS__sect_remove_real(fspace, *sect) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't remove section from internal data structures")
                    remove_sect = FALSE;
                } /* end if */

                /* Shrink the container */
                /* (callback can indicate that it has discarded the section by setting *sect to NULL) */
                HDassert(sect_cls->shrink);
                if((*sect_cls->shrink)(sect, op_data) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't shrink free space container")

                /* If this section was shrunk away, we may need to shrink another section */
                if(*sect == NULL) {
                    /* Check for sections on merge list */
                    if(fspace->sinfo->merge_list)  {
                        H5SL_node_t *last_node;         /* Last node in merge list */

                        /* Check for last node in the merge list */
                        if(NULL != (last_node = H5SL_last(fspace->sinfo->merge_list))) {
                            /* Get the pointer to the last section, from the last node */
                            *sect = (H5FS_section_info_t *)H5SL_item(last_node);
                            HDassert(*sect);

                            /* Indicate that this section needs to be removed if it causes a shrink */
                            remove_sect = TRUE;
                        } /* end if */
                    } /* end if */
                } /* end if */

                /* Indicate successful merge occurred */
                modified = TRUE;
            } /* end if */
        } /* end if */
    } while(modified && *sect);

    /* Check for section that was shrunk away and next section not shrinking */
    if(remove_sect && (*sect != NULL))
        *sect = NULL;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__sect_merge() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_sect_add
 *
 * Purpose:     Add a section of free space to the free list
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  7, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_sect_add(H5F_t *f, H5FS_t *fspace, H5FS_section_info_t *sect, unsigned flags,
    void *op_data)
{
    H5FS_section_class_t *cls;          /* Section's class */
    hbool_t sinfo_valid = FALSE;        /* Whether the section info is valid */
    hbool_t sinfo_modified = FALSE;     /* Whether the section info was modified */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: *sect = {%a, %Hu, %u, %s}\n", FUNC, sect->addr, sect->size, sect->type, (sect->state == H5FS_SECT_LIVE ? "H5FS_SECT_LIVE" : "H5FS_SECT_SERIALIZED"));
#endif /* H5FS_SINFO_DEBUG */

    /* Check arguments. */
    HDassert(fspace);
    HDassert(sect);
    HDassert(H5F_addr_defined(sect->addr));
    HDassert(sect->size);

    /* Get a pointer to the section info */
    if(H5FS__sinfo_lock(f, fspace, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTGET, FAIL, "can't get section info")
    sinfo_valid = TRUE;

    /* Call "add" section class callback, if there is one */
    cls = &fspace->sect_cls[sect->type];
    if(cls->add)
        if((*cls->add)(&sect, &flags, op_data) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "'add' section class callback failed")

    /* Check for merging returned space with existing section node */
    if(flags & H5FS_ADD_RETURNED_SPACE) {
#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: Returning space\n", FUNC);
#endif /* H5FS_SINFO_DEBUG */

        /* Attempt to merge returned section with existing sections */
        if(H5FS__sect_merge(fspace, &sect, op_data) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTMERGE, FAIL, "can't merge sections")
    } /* end if */

    /* Add new (possibly merged) node to free sections data structures */
    /* (If section has been completely merged or shrunk away, 'sect' will
     *  be NULL at this point - QAK)
     */
    if(sect)
        if(H5FS__sect_link(fspace, sect, flags) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space section into skip list")

#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: fspace->tot_space = %Hu\n", FUNC, fspace->tot_space);
#endif /* H5FS_SINFO_DEBUG */
    /* Mark free space sections as changed */
    /* (if adding sections while deserializing sections, don't set the flag) */
    if(!(flags & (H5FS_ADD_DESERIALIZING | H5FS_PAGE_END_NO_ADD)))
        sinfo_modified = TRUE;

done:
    /* Release the section info */
    if(sinfo_valid && H5FS__sinfo_unlock(f, fspace, sinfo_modified) < 0)
        HDONE_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't release section info")

#ifdef H5FS_DEBUG_ASSERT
if(!(flags & (H5FS_ADD_DESERIALIZING | H5FS_ADD_SKIP_VALID)))
    H5FS__assert(fspace);
#endif /* H5FS_DEBUG_ASSERT */
#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: Leaving, ret_value = %d\n", FUNC, ret_value);
#endif /* H5FS_SINFO_DEBUG */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_add() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_sect_try_extend
 *
 * Purpose:     Try to extend a block using space from a section on the free list
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, January  8, 2008
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5FS_sect_try_extend(H5F_t *f, H5FS_t *fspace, haddr_t addr,
    hsize_t size, hsize_t extra_requested, unsigned flags, void *op_data)
{
    hbool_t sinfo_valid = FALSE;        /* Whether the section info is valid */
    hbool_t sinfo_modified = FALSE;     /* Whether the section info was modified */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: addr = %a, size = %Hu, extra_requested = %hu\n", FUNC, addr, size, extra_requested);
#endif /* H5FS_SINFO_DEBUG */

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(H5F_addr_defined(addr));
    HDassert(size > 0);
    HDassert(extra_requested > 0);

    /* Check for any sections on free space list */
#ifdef H5FS_SINFO_DEBUG
HDfprintf(stderr, "%s: fspace->tot_sect_count = %Hu\n", FUNC, fspace->tot_sect_count);
HDfprintf(stderr, "%s: fspace->serial_sect_count = %Hu\n", FUNC, fspace->serial_sect_count);
HDfprintf(stderr, "%s: fspace->ghost_sect_count = %Hu\n", FUNC, fspace->ghost_sect_count);
#endif /* H5FS_SINFO_DEBUG */
    if(fspace->tot_sect_count > 0) {
        H5FS_section_info_t *sect;      /* Temporary free space section */

        /* Get a pointer to the section info */
        if(H5FS__sinfo_lock(f, fspace, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTGET, FAIL, "can't get section info")
        sinfo_valid = TRUE;


/*

Pseudo-code for algorithm:

_section_ = <Get pointer to section with address > _region.addr_>
if(_section_)
    if(_section_ adjoins _region_ && _section.size_ >= _extra_requested_)
        <remove section from data structures>
        if(_section.size_ > _extra_requested_)
            if(<can adjust _section_>)
                <adjust _section_ by _extra_requested_>
                <add adjusted section back to data structures>
            else
                <re-add UNadjusted section back to data structures>
                <error>
        <mark free space sections as changed in metadata cache>

*/
        /* Look for a section after block to extend */
        if((sect = (H5FS_section_info_t *)H5SL_greater(fspace->sinfo->merge_list, &addr))) {
            /* Check if this section adjoins the block and is large enough to
             *  fulfill extension request.
             *
             * (Note: we assume that the section is fully merged with any
             *  possible neighboring nodes and is not at the end of the file
             *  (or it would have been eliminated), etc)
             */
            if(sect->size >= extra_requested && (addr + size) == sect->addr) {
                H5FS_section_class_t *cls;          /* Section's class */

                /* Remove section from data structures */
                if(H5FS__sect_remove_real(fspace, sect) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't remove section from internal data structures")

                /* Get class for section */
                cls = &fspace->sect_cls[sect->type];

                /* Check for the section needing to be adjusted and re-added */
                /* (Note: we should probably add a can_adjust/adjust callback
                 *      to the section class structure, but we don't need it
                 *      for the current usage, so I've deferred messing with
                 *      it. - QAK - 2008/01/08)
                 */
                if(sect->size > extra_requested) {
                    /* Sanity check (for now) */
                    HDassert(cls->flags & H5FS_CLS_ADJUST_OK);

                    /* Adjust section by amount requested */
                    sect->addr += extra_requested;
                    sect->size -= extra_requested;
                    if(cls->add)
                        if((*cls->add)(&sect, &flags, op_data) < 0)
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "'add' section class callback failed")

                    /* Re-adding the section could cause it to disappear (particularly when paging) */
                    if(sect) {
                        /* Re-add adjusted section to free sections data structures */
                        if(H5FS__sect_link(fspace, sect, 0) < 0)
                            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space section into skip list")
                    } /* end if */
                } /* end if */
                else {
                    /* Sanity check */
                    HDassert(sect->size == extra_requested);

                    /* Exact match, so just free section */
                    if((*cls->free)(sect) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't free section")
                } /* end else */

                /* Note that we modified the section info */
                sinfo_modified = TRUE;

                /* Indicate success */
                HGOTO_DONE(TRUE);
            } /* end if */
        } /* end if */
    } /* end if */

done:
    /* Release the section info */
    if(sinfo_valid && H5FS__sinfo_unlock(f, fspace, sinfo_modified) < 0)
        HDONE_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't release section info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_try_extend() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_sect_try_merge
 *
 * Purpose:     Try to merge/shrink a block
 *
 * Return:      TRUE:       merged/shrunk
 *              FALSE:      not merged/not shrunk
 *              Failure:    negative
 *
 * Programmer:  Vailin Choi
 *              June 10, 2009
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5FS_sect_try_merge(H5F_t *f, H5FS_t *fspace, H5FS_section_info_t *sect,
    unsigned flags, void *op_data)
{
    hbool_t sinfo_valid = FALSE;        /* Whether the section info is valid */
    hbool_t sinfo_modified = FALSE;     /* Whether the section info was modified */
    hsize_t saved_fs_size;              /* Copy of the free-space section size */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments. */
    HDassert(f);
    HDassert(fspace);
    HDassert(sect);
    HDassert(H5F_addr_defined(sect->addr));
    HDassert(sect->size);

    /* Get a pointer to the section info */
    if(H5FS__sinfo_lock(f, fspace, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTGET, FAIL, "can't get section info")
    sinfo_valid = TRUE;
    saved_fs_size = sect->size;

    /* Attempt to merge/shrink section with existing sections */
    if(H5FS__sect_merge(fspace, &sect, op_data) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTMERGE, FAIL, "can't merge sections")

    /* Check if section is shrunk and/or merged away completely */
    if(!sect) {
        sinfo_modified = TRUE;
        HGOTO_DONE(TRUE)
    } /* end if */
    else {
        /* Check if section is merged */
        if(sect->size > saved_fs_size) {
            if(H5FS__sect_link(fspace, sect, flags) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space section into skip list")
            sinfo_modified = TRUE;
            HGOTO_DONE(TRUE)
        } /* end if */
    } /* end else */

done:
    /* Release the section info */
    if(sinfo_valid && H5FS__sinfo_unlock(f, fspace, sinfo_modified) < 0)
        HDONE_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't release section info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_try_merge() */


/*-------------------------------------------------------------------------
 * Function:    H5FS__sect_find_node
 *
 * Purpose:     Locate a section of free space (in existing free space list
 *              bins) that is large enough to fulfill request.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Monday, March 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static htri_t
H5FS__sect_find_node(H5FS_t *fspace, hsize_t request, H5FS_section_info_t **node)
{
    H5FS_node_t *fspace_node;        /* Free list size node */
    unsigned bin;                   /* Bin to put the free space section in */
    htri_t ret_value = FALSE;       /* Return value */

    H5SL_node_t *curr_size_node=NULL;
    const H5FS_section_class_t *cls;    /* Class of section */
    hsize_t alignment;

    FUNC_ENTER_STATIC

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->sinfo);
    HDassert(fspace->sinfo->bins);
    HDassert(request > 0);
    HDassert(node);

    /* Determine correct bin which holds items of at least the section's size */
    bin = H5VM_log2_gen(request);
    HDassert(bin < fspace->sinfo->nbins);
    alignment = fspace->alignment;
    if(!((alignment > 1) && (request >= fspace->align_thres)))
        alignment = 0; /* no alignment */

    do {
        /* Check if there's any sections in this bin */
        if(fspace->sinfo->bins[bin].bin_list) {

            if (!alignment) { /* no alignment */
                /* Find the first free space section that is large enough to fulfill request */
                /* (Since the bins use skip lists to track the sizes of the address-ordered
                 *  lists, this is actually a "best fit" algorithm)
                 */
                /* Look for large enough free space section in this bin */
                if((fspace_node = (H5FS_node_t *)H5SL_greater(fspace->sinfo->bins[bin].bin_list, &request))) {
                    /* Take first node off of the list (ie. node w/lowest address) */
                    if(NULL == (*node = (H5FS_section_info_t *)H5SL_remove_first(fspace_node->sect_list)))
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space node from skip list")

                    /* Get section's class */
                    cls = &fspace->sect_cls[(*node)->type];
                    /* Decrement # of sections in section size node */
                    if(H5FS__size_node_decr(fspace->sinfo, bin, fspace_node, cls) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space size node from skip list")
                    if(H5FS__sect_unlink_rest(fspace, cls, *node) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't remove section from non-size tracking data structures")
                    /* Indicate that we found a node for the request */
                    HGOTO_DONE(TRUE)
                } /* end if */
            }  /* end if */
            else { /* alignment is set */
                /* get the first node of a certain size in this bin */
                curr_size_node = H5SL_first(fspace->sinfo->bins[bin].bin_list);
                while (curr_size_node != NULL) {
                    H5FS_node_t *curr_fspace_node=NULL;
                    H5SL_node_t *curr_sect_node=NULL;

                    /* Get the free space node for free space sections of the same size */
                    curr_fspace_node = (H5FS_node_t *)H5SL_item(curr_size_node);

                    /* Get the Skip list which holds  pointers to actual free list sections */
                    curr_sect_node = (H5SL_node_t *)H5SL_first(curr_fspace_node->sect_list);

                    while(curr_sect_node != NULL) {
                        H5FS_section_info_t *curr_sect=NULL;
                        hsize_t mis_align=0, frag_size=0;
                        H5FS_section_info_t *split_sect=NULL;

                        /* Get section node */
                        curr_sect = (H5FS_section_info_t *)H5SL_item(curr_sect_node);

                        HDassert(H5F_addr_defined(curr_sect->addr));
                        HDassert(curr_fspace_node->sect_size == curr_sect->size);

                        cls = &fspace->sect_cls[curr_sect->type];

                        HDassert(alignment);
                        HDassert(cls);

                        if((mis_align = curr_sect->addr % alignment))
                            frag_size = alignment - mis_align;

                        if((curr_sect->size >= (request + frag_size)) && (cls->split)) {
                            /* remove the section with aligned address */
                            if(NULL == (*node = (H5FS_section_info_t *)H5SL_remove(curr_fspace_node->sect_list, &curr_sect->addr)))
                                HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space node from skip list")
                            /* Decrement # of sections in section size node */
                            if(H5FS__size_node_decr(fspace->sinfo, bin, curr_fspace_node, cls) < 0)
                                HGOTO_ERROR(H5E_FSPACE, H5E_CANTREMOVE, FAIL, "can't remove free space size node from skip list")

                            if(H5FS__sect_unlink_rest(fspace, cls, *node) < 0)
                                HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't remove section from non-size tracking data structures")

                            /*
                             * The split() callback splits NODE into 2 sections:
                             *  split_sect is the unused fragment for aligning NODE
                             *  NODE's addr & size are updated to point to the remaining aligned section
                             * split_sect is re-added to free-space
                             */
                            if(mis_align) {
                                split_sect = cls->split(*node, frag_size);
                                if((H5FS__sect_link(fspace, split_sect, 0) < 0))
                                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space section into skip list")
                                /* sanity check */
                                HDassert(split_sect->addr < (*node)->addr);
                                HDassert(request <= (*node)->size);
                            } /* end if */
                            /* Indicate that we found a node for the request */
                            HGOTO_DONE(TRUE)
                        } /* end if */

                        /* Get the next section node in the list */
                        curr_sect_node = H5SL_next(curr_sect_node);
                    } /* end while of curr_sect_node */

                    /* Get the next size node in the bin */
                    curr_size_node = H5SL_next(curr_size_node);
                } /* end while of curr_size_node */
            }  /* else of alignment */
        } /* if bin_list */
        /* Advance to next larger bin */
        bin++;
    } while(bin < fspace->sinfo->nbins);

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS__sect_find_node() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_sect_find
 *
 * Purpose:     Locate a section of free space (in existing free space list) that
 *              is large enough to fulfill request.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March  7, 2006
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5FS_sect_find(H5F_t *f, H5FS_t *fspace, hsize_t request, H5FS_section_info_t **node)
{
    hbool_t sinfo_valid = FALSE;        /* Whether the section info is valid */
    hbool_t sinfo_modified = FALSE;     /* Whether the section info was modified */
    htri_t ret_value = FALSE;           /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments. */
    HDassert(fspace);
    HDassert(fspace->nclasses);
    HDassert(request);
    HDassert(node);

    /* Check for any sections on free space list */
    if(fspace->tot_sect_count > 0) {
        /* Get a pointer to the section info */
        if(H5FS__sinfo_lock(f, fspace, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTGET, FAIL, "can't get section info")
        sinfo_valid = TRUE;

        /* Look for node in bins */
        if((ret_value = H5FS__sect_find_node(fspace, request, node)) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTFREE, FAIL, "can't remove section from bins")

        /* Decrement # of sections on free list, if we found an object */
        if(ret_value > 0) {
            /* Note that we've modified the section info */
            sinfo_modified = TRUE;
        } /* end if */
    } /* end if */

done:
    /* Release the section info */
    if(sinfo_valid && H5FS__sinfo_unlock(f, fspace, sinfo_modified) < 0)
        HDONE_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't release section info")

#ifdef H5FS_DEBUG_ASSERT
    H5FS__assert(fspace);
#endif /* H5FS_DEBUG_ASSERT */
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_find() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_iterate_sect_cb
 *
 * Purpose:     Skip list iterator callback to iterate over free space sections
 *              of a particular size
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Saturday, May 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_iterate_sect_cb(void *_item, void H5_ATTR_UNUSED *key, void *_udata)
{
    H5FS_section_info_t *sect_info = (H5FS_section_info_t *)_item;   /* Free space section to work on */
    H5FS_iter_ud_t *udata = (H5FS_iter_ud_t *)_udata; /* Callback info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments. */
    HDassert(sect_info);
    HDassert(udata->fspace);
    HDassert(udata->op);

    /* Make callback for this section */
    if((*udata->op)(sect_info, udata->op_data) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_BADITER, FAIL, "iteration callback failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_iterate_sect_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_iterate_node_cb
 *
 * Purpose:     Skip list iterator callback to iterate over free space sections
 *              in a bin
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Saturday, May 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5FS_iterate_node_cb(void *_item, void H5_ATTR_UNUSED *key, void *_udata)
{
    H5FS_node_t *fspace_node = (H5FS_node_t *)_item;   /* Free space size node to work on */
    H5FS_iter_ud_t *udata = (H5FS_iter_ud_t *)_udata; /* Callback info */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments. */
    HDassert(fspace_node);
    HDassert(udata->fspace);
    HDassert(udata->op);

    /* Iterate through all the sections of this size */
    HDassert(fspace_node->sect_list);
    if(H5SL_iterate(fspace_node->sect_list, H5FS_iterate_sect_cb, udata) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_BADITER, FAIL, "can't iterate over section nodes")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_iterate_node_cb() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_sect_iterate
 *
 * Purpose:     Iterate over all the sections managed
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Saturday, May 13, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_sect_iterate(H5F_t *f, H5FS_t *fspace, H5FS_operator_t op, void *op_data)
{
    H5FS_iter_ud_t udata;               /* User data for callbacks */
    hbool_t sinfo_valid = FALSE;        /* Whether the section info is valid */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments. */
    HDassert(fspace);
    HDassert(op);

    /* Set up user data for iterator */
    udata.fspace = fspace;
    udata.op = op;
    udata.op_data = op_data;

    /* Iterate over sections, if there are any */
    if(fspace->tot_sect_count) {
        unsigned bin;           /* Current bin we are on */

        /* Get a pointer to the section info */
        if(H5FS__sinfo_lock(f, fspace, H5AC__READ_ONLY_FLAG) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTGET, FAIL, "can't get section info")
        sinfo_valid = TRUE;

        /* Iterate over all the bins */
        for(bin = 0; bin < fspace->sinfo->nbins; bin++) {
            /* Check if there are any sections in this bin */
            if(fspace->sinfo->bins[bin].bin_list) {
                /* Iterate over list of section size nodes for bin */
                if(H5SL_iterate(fspace->sinfo->bins[bin].bin_list, H5FS_iterate_node_cb, &udata) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_BADITER, FAIL, "can't iterate over section size nodes")
            } /* end if */
        } /* end for */
    } /* end if */

done:
    /* Release the section info */
    if(sinfo_valid && H5FS__sinfo_unlock(f, fspace, FALSE) < 0)
        HDONE_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't release section info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_iterate() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_sect_stats
 *
 * Purpose:     Retrieve info about the sections managed
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, May 30, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_sect_stats(const H5FS_t *fspace, hsize_t *tot_space, hsize_t *nsects)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Check arguments. */
    HDassert(fspace);

    /* Get the stats desired */
    if(tot_space)
        *tot_space = fspace->tot_space;
    if(nsects)
        *nsects = fspace->tot_sect_count;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5FS_sect_stats() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_sect_change_class
 *
 * Purpose:     Make appropriate adjustments to internal data structures when
 *              a section changes class
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Monday, July 10, 2006
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_sect_change_class(H5F_t *f, H5FS_t *fspace, H5FS_section_info_t *sect,
    uint16_t new_class)
{
    const H5FS_section_class_t *old_cls;        /* Old class of section */
    const H5FS_section_class_t *new_cls;        /* New class of section */
    unsigned old_class;                         /* Old class ID of section */
    hbool_t sinfo_valid = FALSE;                /* Whether the section info is valid */
    herr_t ret_value = SUCCEED;                 /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments. */
    HDassert(fspace);
    HDassert(sect);
    HDassert(sect->type < fspace->nclasses);
    HDassert(new_class < fspace->nclasses);

    /* Get a pointer to the section info */
    if(H5FS__sinfo_lock(f, fspace, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTGET, FAIL, "can't get section info")
    sinfo_valid = TRUE;

    /* Get class info */
    old_class = sect->type;
    old_cls = &fspace->sect_cls[sect->type];
    new_cls = &fspace->sect_cls[new_class];

    /* Check if the section's class change will affect the # of serializable or ghost sections */
    if((old_cls->flags & H5FS_CLS_GHOST_OBJ) != (new_cls->flags & H5FS_CLS_GHOST_OBJ)) {
        H5FS_node_t *fspace_node;       /* Free list size node */
        unsigned bin;                   /* Bin to put the free space section in */
        hbool_t to_ghost;       /* Flag if the section is changing to a ghost section */

        /* Determine if this section is becoming a ghost or is becoming serializable */
        if(old_cls->flags & H5FS_CLS_GHOST_OBJ)
            to_ghost = FALSE;
        else
            to_ghost = TRUE;

        /* Sanity check */
        HDassert(fspace->sinfo->bins);

        /* Determine correct bin which holds items of at least the section's size */
        bin = H5VM_log2_gen(sect->size);
        HDassert(bin < fspace->sinfo->nbins);
        HDassert(fspace->sinfo->bins[bin].bin_list);

        /* Get space node for section's size */
        fspace_node = (H5FS_node_t *)H5SL_search(fspace->sinfo->bins[bin].bin_list, &sect->size);
        HDassert(fspace_node);

        /* Adjust serializable/ghost counts */
        if(to_ghost) {
            /* Adjust global section count totals */
            fspace->serial_sect_count--;
            fspace->ghost_sect_count++;

            /* Adjust bin's section count totals */
            fspace->sinfo->bins[bin].serial_sect_count--;
            fspace->sinfo->bins[bin].ghost_sect_count++;

            /* Adjust section size node's section count totals */
            fspace_node->serial_count--;
            fspace_node->ghost_count++;

            /* Check if we switched a section size node's status */
            if(fspace_node->serial_count == 0)
                fspace->sinfo->serial_size_count--;
            if(fspace_node->ghost_count == 1)
                fspace->sinfo->ghost_size_count++;
        } /* end if */
        else {
            /* Adjust global section count totals */
            fspace->serial_sect_count++;
            fspace->ghost_sect_count--;

            /* Adjust bin's section count totals */
            fspace->sinfo->bins[bin].serial_sect_count++;
            fspace->sinfo->bins[bin].ghost_sect_count--;

            /* Adjust section size node's section count totals */
            fspace_node->serial_count++;
            fspace_node->ghost_count--;

            /* Check if we switched a section size node's status */
            if(fspace_node->serial_count == 1)
                fspace->sinfo->serial_size_count++;
            if(fspace_node->ghost_count == 0)
                fspace->sinfo->ghost_size_count--;
        } /* end else */
    } /* end if */

    /* Check if the section's class change will affect the mergable list */
    if((old_cls->flags & H5FS_CLS_SEPAR_OBJ) != (new_cls->flags & H5FS_CLS_SEPAR_OBJ)) {
        hbool_t to_mergable;       /* Flag if the section is changing to a mergable section */

        /* Determine if this section is becoming mergable or is becoming separate */
        if(old_cls->flags & H5FS_CLS_SEPAR_OBJ)
            to_mergable = TRUE;
        else
            to_mergable = FALSE;

        /* Add or remove section from merge list, as appropriate */
        if(to_mergable) {
            if(fspace->sinfo->merge_list == NULL)
                if(NULL == (fspace->sinfo->merge_list = H5SL_create(H5SL_TYPE_HADDR, NULL)))
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTCREATE, FAIL, "can't create skip list for merging free space sections")
            if(H5SL_insert(fspace->sinfo->merge_list, sect, &sect->addr) < 0)
                HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't insert free space node into merging skip list")
        } /* end if */
        else {
            H5FS_section_info_t *tmp_sect_node; /* Temporary section node */

            tmp_sect_node = (H5FS_section_info_t *)H5SL_remove(fspace->sinfo->merge_list, &sect->addr);
            if(tmp_sect_node == NULL || tmp_sect_node != sect)
                HGOTO_ERROR(H5E_FSPACE, H5E_NOTFOUND, FAIL, "can't find section node on size list")
        } /* end else */
    } /* end if */

    /* Change the section's class */
    sect->type = new_class;

    /* Change the serialized size of sections */
    fspace->sinfo->serial_size -= fspace->sect_cls[old_class].serial_size;
    fspace->sinfo->serial_size += fspace->sect_cls[new_class].serial_size;

    /* Update current space used for free space sections */
    if(H5FS__sect_serialize_size(fspace) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTCOMPUTE, FAIL, "can't adjust free space section size on disk")

done:
    /* Release the section info */
    if(sinfo_valid && H5FS__sinfo_unlock(f, fspace, TRUE) < 0)
        HDONE_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't release section info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_change_class() */

#ifdef H5FS_DEBUG_ASSERT

/*-------------------------------------------------------------------------
 * Function:    H5FS__sect_assert
 *
 * Purpose:     Verify that the sections managed are mostly sane
 *
 * Return:      void
 *
 * Programmer:  Quincey Koziol
 *              Jul 17 2006
 *
 *-------------------------------------------------------------------------
 */
void
H5FS__sect_assert(const H5FS_t *fspace)
{
    hsize_t separate_obj;       /* The number of separate objects managed */

    FUNC_ENTER_PACKAGE_NOERR

    /* Initialize state */
    separate_obj = 0;

    /* Check for bins to work on */
    if(fspace->sinfo->bins) {
        hsize_t acc_tot_sect_count;     /* Accumulated total section count from bins */
        hsize_t acc_serial_sect_count;  /* Accumulated serializable section count from bins */
        hsize_t acc_ghost_sect_count;   /* Accumulated ghost section count from bins */
        size_t acc_tot_size_count;      /* Accumulated total section size count from bins */
        size_t acc_serial_size_count;   /* Accumulated serializable section size count from bins */
        size_t acc_ghost_size_count;    /* Accumulated ghost section size count from bins */
        unsigned u;             /* Local index variable */

        /* Walk through all sections in bins */
        acc_tot_sect_count = 0;
        acc_serial_sect_count = 0;
        acc_ghost_sect_count = 0;
        acc_tot_size_count = 0;
        acc_serial_size_count = 0;
        acc_ghost_size_count = 0;
        for(u = 0; u < fspace->sinfo->nbins; u++) {
            acc_tot_sect_count += fspace->sinfo->bins[u].tot_sect_count;
            acc_serial_sect_count += fspace->sinfo->bins[u].serial_sect_count;
            acc_ghost_sect_count += fspace->sinfo->bins[u].ghost_sect_count;
            if(fspace->sinfo->bins[u].bin_list) {
                H5SL_node_t *curr_size_node;    /* Current section size node in skip list */
                size_t bin_serial_count;        /* # of serializable sections in this bin */
                size_t bin_ghost_count;         /* # of ghost sections in this bin */

                acc_tot_size_count += H5SL_count(fspace->sinfo->bins[u].bin_list);

                /* Walk through the sections in this bin */
                curr_size_node = H5SL_first(fspace->sinfo->bins[u].bin_list);
                bin_serial_count = 0;
                bin_ghost_count = 0;
                while(curr_size_node != NULL) {
                    H5FS_node_t *fspace_node;       /* Section size node */
                    H5SL_node_t *curr_sect_node;    /* Current section node in skip list */
                    size_t size_serial_count;       /* # of serializable sections of this size */
                    size_t size_ghost_count;        /* # of ghost sections of this size */

                    /* Get section size node */
                    fspace_node = (H5FS_node_t *)H5SL_item(curr_size_node);

                    /* Check sections on list */
                    curr_sect_node = H5SL_first(fspace_node->sect_list);
                    size_serial_count = 0;
                    size_ghost_count = 0;
                    while(curr_sect_node != NULL) {
                        H5FS_section_class_t *cls;      /* Class of section */
                        H5FS_section_info_t *sect;      /* Section */

                        /* Get section node & it's class */
                        sect = (H5FS_section_info_t *)H5SL_item(curr_sect_node);
                        cls = &fspace->sect_cls[sect->type];

                        /* Sanity check section */
                        HDassert(H5F_addr_defined(sect->addr));
                        HDassert(fspace_node->sect_size == sect->size);
                        if(cls->valid)
                            (*cls->valid)(cls, sect);

                        /* Add to correct count */
                        if(cls->flags & H5FS_CLS_GHOST_OBJ)
                            size_ghost_count++;
                        else
                            size_serial_count++;

                        /* Count node, if separate */
                        if(cls->flags & H5FS_CLS_SEPAR_OBJ)
                            separate_obj++;

                        /* Get the next section node in the list */
                        curr_sect_node = H5SL_next(curr_sect_node);
                    } /* end while */

                    /* Check the number of serializable & ghost sections of this size */
                    HDassert(fspace_node->serial_count == size_serial_count);
                    HDassert(fspace_node->ghost_count == size_ghost_count);

                    /* Add to global count of serializable & ghost section sizes */
                    if(fspace_node->serial_count > 0)
                        acc_serial_size_count++;
                    if(fspace_node->ghost_count > 0)
                        acc_ghost_size_count++;

                    /* Add to bin's serializable & ghost counts */
                    bin_serial_count += size_serial_count;
                    bin_ghost_count += size_ghost_count;

                    /* Get the next section size node in the list */
                    curr_size_node = H5SL_next(curr_size_node);
                } /* end while */

                /* Check the number of serializable & ghost sections in this bin */
                HDassert(fspace->sinfo->bins[u].tot_sect_count == (bin_serial_count + bin_ghost_count));
                HDassert(fspace->sinfo->bins[u].serial_sect_count == bin_serial_count);
                HDassert(fspace->sinfo->bins[u].ghost_sect_count == bin_ghost_count);
            } /* end if */
        } /* end for */

        /* Check counts from bins vs. global counts */
        HDassert(fspace->sinfo->tot_size_count == acc_tot_size_count);
        HDassert(fspace->sinfo->serial_size_count == acc_serial_size_count);
        HDassert(fspace->sinfo->ghost_size_count == acc_ghost_size_count);
        HDassert(fspace->tot_sect_count == acc_tot_sect_count);
        HDassert(fspace->serial_sect_count == acc_serial_sect_count);
        HDassert(fspace->ghost_sect_count == acc_ghost_sect_count);
    } /* end if */
    else {
        /* Check counts are zero */
        HDassert(fspace->tot_sect_count == 0);
        HDassert(fspace->serial_sect_count == 0);
        HDassert(fspace->ghost_sect_count == 0);
    } /* end else */

    /* Make certain that the number of sections on the address list is correct */
    if(fspace->sinfo->merge_list)
        HDassert(fspace->tot_sect_count == (separate_obj + H5SL_count(fspace->sinfo->merge_list)));

    FUNC_LEAVE_NOAPI_VOID
} /* end H5FS__sect_assert() */
#endif /* H5FS_DEBUG_ASSERT */


/*-------------------------------------------------------------------------
 * Function:    H5FS_sect_try_shrink_eoa
 *
 * Purpose:     To shrink the last section on the merge list if the section
 *              is at EOF.
 *
 * Return:      TRUE/FALSE/FAIL
 *
 * Programmer:  Vailin Choi
 *
 *-------------------------------------------------------------------------
 */
htri_t
H5FS_sect_try_shrink_eoa(H5F_t *f, H5FS_t *fspace, void *op_data)
{
    hbool_t sinfo_valid = FALSE;        /* Whether the section info is valid */
    hbool_t section_removed = FALSE;    /* Whether a section was removed */
    htri_t ret_value = FALSE;          	/* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Check arguments. */
    HDassert(fspace);

    if(H5FS__sinfo_lock(f, fspace, H5AC__NO_FLAGS_SET) < 0)
        HGOTO_ERROR(H5E_FSPACE, H5E_CANTGET, FAIL, "can't get section info")
    sinfo_valid = TRUE;

    if(fspace->sinfo && fspace->sinfo->merge_list) {
        H5SL_node_t *last_node;         	/* Last node in merge list */

        /* Check for last node in the merge list */
        if(NULL != (last_node = H5SL_last(fspace->sinfo->merge_list))) {
            H5FS_section_info_t *tmp_sect;  	/* Temporary free space section */
            H5FS_section_class_t *tmp_sect_cls;	/* Temporary section's class */

            /* Get the pointer to the last section, from the last node */
            tmp_sect = (H5FS_section_info_t *)H5SL_item(last_node);
            HDassert(tmp_sect);
            tmp_sect_cls = &fspace->sect_cls[tmp_sect->type];
            if(tmp_sect_cls->can_shrink) {
                /* Check if the section can be shrunk away */
                if((ret_value = (*tmp_sect_cls->can_shrink)(tmp_sect, op_data)) < 0)
                    HGOTO_ERROR(H5E_FSPACE, H5E_CANTSHRINK, FAIL, "can't check for shrinking container")
                if(ret_value > 0) {
                    HDassert(tmp_sect_cls->shrink);

                    /* Remove section from free space manager */
                    if(H5FS__sect_remove_real(fspace, tmp_sect) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't remove section from internal data structures")
                    section_removed = TRUE;

                    /* Shrink away section */
                    if((*tmp_sect_cls->shrink)(&tmp_sect, op_data) < 0)
                        HGOTO_ERROR(H5E_FSPACE, H5E_CANTINSERT, FAIL, "can't shrink free space container")
                } /* end if */
            } /* end if */
        } /* end if */
    } /* end if */

done:
    /* Release the section info */
    if(sinfo_valid && H5FS__sinfo_unlock(f, fspace, section_removed) < 0)
        HDONE_ERROR(H5E_FSPACE, H5E_CANTRELEASE, FAIL, "can't release section info")

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_sect_try_shrink_eoa() */


/*-------------------------------------------------------------------------
 * Function:    H5FS_vfd_alloc_hdr_and_section_info_if_needed
 *
 * Purpose:     This function is part of a hack to patch over a design
 *              flaw in the free space managers for file space allocation.
 *              Specifically, if a free space manager allocates space for
 *              its own section info, it is possible for it to
 *              go into an infinite loop as it:
 *
 *                      1) computes the size of the section info
 *
 *                      2) allocates file space for the section info
 *
 *                      3) notices that the size of the section info
 *                         has changed
 *
 *                      4) deallocates the section info file space and
 *                         returns to 1) above.
 *
 *              Similarly, if it allocates space for its own header, it
 *              can go into an infinite loop as it:
 *
 *                      1) allocates space for the header
 *
 *                      2) notices that the free space manager is empty
 *                         and thus should not have file space allocated
 *                         to its header
 *
 *                      3) frees the space allocated to the header
 *
 *                      4) notices that the free space manager is not
 *                         empty and thus must have file space allocated
 *                         to it, and thus returns to 1) above.
 *
 *              In a nutshell, the solution applied in this hack is to
 *              deallocate file space for the free space manager(s) that
 *              handle FSM header and/or section info file space allocations,
 *              wait until all other allocation/deallocation requests have
 *              been handled, and then test to see if the free space manager(s)
 *              in question are empty.  If they are, do nothing.  If they
 *              are not, allocate space for them at end of file bypassing the
 *              usual file space allocation calls, and thus avoiding the
 *              potential infinite loops.
 *
 *              The purpose of this function is to allocate file space for
 *              the header and section info of the target free space manager
 *              directly from the VFD if needed.  In this case the function
 *              also re-inserts the header and section info in the metadata
 *              cache with this allocation.
 *
 *		When paged allocation is not enabled, allocation of space
 *		for the free space manager header and section info is
 *		straight forward -- we simply allocate the space directly
 *		from file driver.
 *
 *              Note that if f->shared->alignment > 1, and EOA is not a
 *              multiple of the alignment, it is possible that performing
 *              these allocations may generate a fragment of file space in
 *              addition to the space allocated for the section info.  This
 *		excess space is dropped on the floor.  As shall be seen,
 *		it will usually be reclaimed later.
 *
 *		When page allocation is enabled, things are more difficult,
 *		as there is the possibility that page buffering will be
 *		enabled when the free space managers are read.  To allow
 *		for this, we must ensure that space allocated for the
 *		free space manager header and section info is either larger
 *		than a page, or resides completely within a page.
 *
 *		Do this by allocating space for the free space header and
 *		section info starting at page boundaries, and extending
 *		allocation to the next page boundary.  This of course wastes
 *		space, but see below.
 *
 *              On the first free space allocation / deallocation after the
 *		next file open, we will read the self referential free space
 *		managers, float them and reduce the EOA to its value prior
 *		to allocation of file space for the self referential free
 *              space managers on the preceeding file close.  This EOA value
 *		is stored in the free space manager superblock extension
 *		message.
 *
 * Return:      Success:        non-negative
 *              Failure:        negative
 *
 * Programmer:  John Mainzer
 *              6/6/16
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5FS_vfd_alloc_hdr_and_section_info_if_needed(H5F_t *f, H5FS_t *fspace,
    haddr_t *fs_addr_ptr)
{
    hsize_t	hdr_alloc_size;
    hsize_t	sinfo_alloc_size;
    haddr_t     sect_addr = HADDR_UNDEF;        /* address of sinfo */
    haddr_t     eoa_frag_addr = HADDR_UNDEF;    /* Address of fragment at EOA */
    hsize_t     eoa_frag_size = 0;      /* Size of fragment at EOA */
    haddr_t     eoa = HADDR_UNDEF;      /* Initial EOA for the file */
    herr_t ret_value = SUCCEED;         /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Check arguments. */
    HDassert(f);
    HDassert(f->shared);
    HDassert(f->shared->lf);
    HDassert(fspace);
    HDassert(fs_addr_ptr);

    /* the section info should be unlocked */
    HDassert(fspace->sinfo_lock_count == 0);

    /* no space should be allocated */
    HDassert(*fs_addr_ptr == HADDR_UNDEF);
    HDassert(fspace->addr == HADDR_UNDEF);
    HDassert(fspace->sect_addr == HADDR_UNDEF);
    HDassert(fspace->alloc_sect_size == 0);

    /* persistent free space managers must be enabled */
    HDassert(f->shared->fs_persist);

    /* At present, all free space strategies enable the free space managers.
     * This will probably change -- at which point this assertion should
     * be revisited.
     */
    /* Updated: Only the following two strategies enable the free-space managers */
    HDassert((f->shared->fs_strategy == H5F_FSPACE_STRATEGY_FSM_AGGR) ||
             (f->shared->fs_strategy == H5F_FSPACE_STRATEGY_PAGE));

    if(fspace->serial_sect_count > 0) {
        /* the section info is floating, so space->sinfo should be defined */
        HDassert(fspace->sinfo);

        /* start by allocating file space for the header */

        /* Get the EOA for the file -- need for sanity check below */
        if(HADDR_UNDEF == (eoa = H5F_get_eoa(f, H5FD_MEM_FSPACE_HDR)))
           HGOTO_ERROR(H5E_RESOURCE, H5E_CANTGET, FAIL, "Unable to get eoa")

        /* check for overlap into temporary allocation space */
        if(H5F_IS_TMP_ADDR(f, (eoa + fspace->sect_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_BADRANGE, FAIL, "hdr file space alloc will overlap into 'temporary' file space")

        hdr_alloc_size = H5FS_HEADER_SIZE(f);

        /* if page allocation is enabled, extend the hdr_alloc_size to the
         * next page boundary.
         */
        if(H5F_PAGED_AGGR(f)) {
            HDassert(0 == (eoa % f->shared->fs_page_size));

            hdr_alloc_size = ((hdr_alloc_size / f->shared->fs_page_size) + 1) * f->shared->fs_page_size;

            HDassert(hdr_alloc_size >= H5FS_HEADER_SIZE(f));
            HDassert((hdr_alloc_size % f->shared->fs_page_size) == 0);
        } /* end if */

        /* allocate space for the hdr */
        if(HADDR_UNDEF == (fspace->addr = H5FD_alloc(f->shared->lf, H5FD_MEM_FSPACE_HDR,
                f, hdr_alloc_size, &eoa_frag_addr, &eoa_frag_size)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTALLOC, FAIL, "can't allocate file space for hdr")

        /* if the file alignment is 1, there should be no
         * eoa fragment.  Otherwise, drop any fragment on the floor.
         */
        HDassert((eoa_frag_size == 0) || (f->shared->alignment != 1));

        /* Cache the new free space header (pinned) */
        if(H5AC_insert_entry(f, H5AC_FSPACE_HDR, fspace->addr, fspace, H5AC__PIN_ENTRY_FLAG) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINIT, FAIL, "can't add free space header to cache")

        *fs_addr_ptr = fspace->addr;

        /* now allocate file space for the section info */

        /* Get the EOA for the file -- need for sanity check below */
        if(HADDR_UNDEF == (eoa = H5F_get_eoa(f, H5FD_MEM_FSPACE_SINFO)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTGET, FAIL, "Unable to get eoa")

        /* check for overlap into temporary allocation space */
        if(H5F_IS_TMP_ADDR(f, (eoa + fspace->sect_size)))
            HGOTO_ERROR(H5E_FSPACE, H5E_BADRANGE, FAIL, "sinfo file space alloc will overlap into 'temporary' file space")

        sinfo_alloc_size = fspace->sect_size;

        /* if paged allocation is enabled, extend the sinfo_alloc_size to the
         * next page boundary.
         */
        if(H5F_PAGED_AGGR(f)) {
            HDassert(0 == (eoa % f->shared->fs_page_size));

            sinfo_alloc_size = ((sinfo_alloc_size / f->shared->fs_page_size) + 1) * f->shared->fs_page_size;

            HDassert(sinfo_alloc_size >= fspace->sect_size);
            HDassert((sinfo_alloc_size % f->shared->fs_page_size) == 0);
        } /* end if */

        /* allocate space for the section info */
        if(HADDR_UNDEF == (sect_addr = H5FD_alloc(f->shared->lf, H5FD_MEM_FSPACE_SINFO,
                f, sinfo_alloc_size, &eoa_frag_addr, &eoa_frag_size)))
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTALLOC, FAIL, "can't allocate file space")

        /* if the file alignment is 1, there should be no
         * eoa fragment.  Otherwise, drop the fragment on the floor.
         */
        HDassert((eoa_frag_size == 0) || (f->shared->alignment != 1));

        /* update fspace->alloc_sect_size and fspace->sect_addr to reflect
         * the allocation
         */
        fspace->alloc_sect_size = fspace->sect_size;
        fspace->sect_addr = sect_addr;

        /* insert the new section info into the metadata cache.  */

        /* Question: Do we need to worry about this insertion causing an
         * eviction from the metadata cache?  Think on this.  If so, add a
         * flag to H5AC_insert() to force it to skip the call to make space in
         * cache.
         *
         * On reflection, no.
         *
         * On a regular file close, any eviction will not change the
         * the contents of the free space manager(s), as all entries
         * should have correct file space allocated by the time this
         * function is called.
         *
         * In the cache image case, the selection of entries for inclusion
         * in the cache image will not take place until after this call.
         * (Recall that this call is made during the metadata fsm settle
         * routine, which is called during the serialization routine in
         * the cache image case.  Entries are not selected for inclusion
         * in the image until after the cache is serialized.)
         *
         *                                        JRM -- 11/4/16
         */
        if(H5AC_insert_entry(f, H5AC_FSPACE_SINFO, sect_addr, fspace->sinfo, H5AC__NO_FLAGS_SET) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTINIT, FAIL, "can't add free space sinfo to cache")

        /* We have changed the sinfo address -- Mark free space header dirty */
        if(H5AC_mark_entry_dirty(fspace) < 0)
            HGOTO_ERROR(H5E_FSPACE, H5E_CANTMARKDIRTY, FAIL, "unable to mark free space header as dirty")

        /* since space has been allocated for the section info and the sinfo
         * has been inserted into the cache, relinquish ownership (i.e. float)
         * the section info.
         */
        fspace->sinfo = NULL;
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5FS_vfd_alloc_hdr_and_section_info_if_needed() */


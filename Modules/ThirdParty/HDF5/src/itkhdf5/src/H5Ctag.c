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
 * Created:     H5Ctag.c
 *              June 5 2016
 *              Quincey Koziol
 *
 * Purpose:     Functions in this file operate on tags for metadata
 *              cache entries.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Cmodule.h"          /* This source code file is part of the H5C module */
#define H5F_FRIEND		/*suppress error about including H5Fpkg	  */


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5ACprivate.h"        /* Metadata cache                       */
#include "H5Cpkg.h"		/* Cache				*/
#include "H5CXprivate.h"        /* API Contexts                         */
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5Fpkg.h"		/* Files				*/
#include "H5Iprivate.h"		/* IDs			  		*/
#include "H5Pprivate.h"         /* Property lists                       */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/

/* Typedef for tagged entry iterator callback context - evict tagged entries */
typedef struct {
    H5F_t *f;                           /* File pointer for evicting entry */
    hbool_t evicted_entries_last_pass;  /* Flag to indicate that an entry 
                                         * was evicted when iterating over 
                                         * cache 
                                         */
    hbool_t pinned_entries_need_evicted;/* Flag to indicate that a pinned 
                                         * entry was attempted to be evicted 
                                         */
    hbool_t skipped_pf_dirty_entries;   /* Flag indicating that one or more 
                                         * entries marked prefetched_dirty
                                         * were encountered and not 
                                         * evicted.
                                         */
} H5C_tag_iter_evict_ctx_t;

/* Typedef for tagged entry iterator callback context - expunge tag type metadata */
typedef struct {
    H5F_t *f;                           /* File pointer for evicting entry */
    int type_id;                        /* Cache entry type to expunge */
    unsigned flags;                     /* Flags for expunging entry */
} H5C_tag_iter_ettm_ctx_t;

/* Typedef for tagged entry iterator callback context - mark corked */
typedef struct {
    hbool_t cork_val;                   /* Corked value */
} H5C_tag_iter_cork_ctx_t;


/********************/
/* Local Prototypes */
/********************/
static herr_t H5C__mark_tagged_entries(H5C_t *cache_ptr, haddr_t tag);


/*********************/
/* Package Variables */
/*********************/

/* Declare extern free list to manage the tag info struct */
H5FL_EXTERN(H5C_tag_info_t);


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 *
 * Function:    H5C_ignore_tags
 *
 * Purpose:     Override all assertion frameworks associated with making
 *              sure proper tags are applied to cache entries. 
 *
 *              NOTE: This should really only be used in tests that need 
 *              to access internal functions without going through 
 *              standard API paths. Since tags are set inside dxpl_id's
 *              before coming into the cache, any external functions that
 *              use the internal library functions (i.e., tests) should
 *              use this function if they don't plan on setting up proper
 *              metadata tags.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mike McGreevy
 *              December 1, 2009
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_ignore_tags(H5C_t * cache_ptr)
{
    FUNC_ENTER_NOAPI_NOERR

    /* Assertions */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Set variable to ignore tag values upon assignment */
    cache_ptr->ignore_tags = TRUE;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5C_ignore_tags */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_get_ignore_tags
 *
 * Purpose:     Retrieve the 'ignore_tags' field for the cache
 *
 * Return:      'ignore_tags' value (can't fail)
 *
 * Programmer:  Quincey Koziol
 *              April 30, 2016
 *
 *-------------------------------------------------------------------------
 */
hbool_t
H5C_get_ignore_tags(const H5C_t *cache_ptr)
{
    FUNC_ENTER_NOAPI_NOERR

    /* Sanity checks */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Return ignore tag value */
    FUNC_LEAVE_NOAPI(cache_ptr->ignore_tags)
} /* H5C_get_ignore_tags */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_get_num_objs_corked
 *
 * Purpose:     Retrieve the 'num_objs_corked' field for the cache
 *
 * Return:      'num_objs_corked' value (can't fail)
 *
 * Programmer:  Vailin Choi; Feb 2019
 *
 *-------------------------------------------------------------------------
 */
uint32_t
H5C_get_num_objs_corked(const H5C_t *cache_ptr)
{
    FUNC_ENTER_NOAPI_NOERR

    /* Sanity checks */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    /* Return value for num_objs_corked */
    FUNC_LEAVE_NOAPI(cache_ptr->num_objs_corked)
} /* H5C_get_num_objs_corked */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__tag_entry
 *
 * Purpose:     Tags an entry with the provided tag (contained in the API context).
 *              If sanity checking is enabled, this function will perform 
 *              validation that a proper tag is contained within the provided 
 *              data access property list id before application.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mike McGreevy
 *              January 14, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C__tag_entry(H5C_t *cache, H5C_cache_entry_t *entry)
{
    H5C_tag_info_t *tag_info;	/* Points to a tag info struct */
    haddr_t tag;                /* Tag value */
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_PACKAGE

    /* Assertions */
    HDassert(cache != NULL);
    HDassert(entry != NULL);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Get the tag */
    tag = H5CX_get_tag();

    if(cache->ignore_tags) {
        /* if we're ignoring tags, it's because we're running
           tests on internal functions and may not have inserted a tag 
           value into a given API context before creating some metadata. Thus,
           in this case only, if a tag value has not been set, we can
           arbitrarily set it to something for the sake of passing the tests. 
           If the tag value is set, then we'll just let it get assigned without
           additional checking for correctness. */
        if(!H5F_addr_defined(tag))
            tag = H5AC__IGNORE_TAG;
    } /* end if */
#if H5C_DO_TAGGING_SANITY_CHECKS
    else {
        /* Perform some sanity checks to ensure that a correct tag is being applied */
        if(H5C_verify_tag(entry->type->id, tag) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "tag verification failed")
    } /* end else */
#endif

    /* Search the list of tagged object addresses in the cache */
    tag_info = (H5C_tag_info_t *)H5SL_search(cache->tag_list, &tag);

    /* Check if this is the first entry for this tagged object */
    if(NULL == tag_info) {
        /* Allocate new tag info struct */
        if(NULL == (tag_info = H5FL_CALLOC(H5C_tag_info_t)))
            HGOTO_ERROR(H5E_CACHE, H5E_CANTALLOC, FAIL, "can't allocate tag info for cache entry")

        /* Set the tag for all entries */
        tag_info->tag = tag;

        /* Insert tag info into skip list */
        if(H5SL_insert(cache->tag_list, tag_info, &(tag_info->tag)) < 0 )
            HGOTO_ERROR(H5E_CACHE, H5E_CANTINSERT, FAIL, "can't insert tag info in skip list")
    } /* end if */
    else
        HDassert(tag_info->corked || (tag_info->entry_cnt > 0 && tag_info->head));

    /* Sanity check entry, to avoid double insertions, etc */
    HDassert(entry->tl_next == NULL);
    HDassert(entry->tl_prev == NULL);
    HDassert(entry->tag_info == NULL);

    /* Add the entry to the list for the tagged object */
    entry->tl_next = tag_info->head;
    entry->tag_info = tag_info;
    if(tag_info->head)
        tag_info->head->tl_prev = entry;
    tag_info->head = entry;
    tag_info->entry_cnt++;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__tag_entry */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__untag_entry
 *
 * Purpose:     Removes an entry from a tag list, possibly removing the tag
 *		info from the list of tagged objects with entries.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Quincey Koziol
 *              July 8, 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C__untag_entry(H5C_t *cache, H5C_cache_entry_t *entry)
{
    H5C_tag_info_t *tag_info;	/* Points to a tag info struct */
    herr_t ret_value = SUCCEED;  /* Return value */

    FUNC_ENTER_PACKAGE

    /* Assertions */
    HDassert(cache != NULL);
    HDassert(entry != NULL);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Get the entry's tag info struct */
    if(NULL != (tag_info = entry->tag_info)) {
        /* Remove the entry from the list */
        if(entry->tl_next)
            entry->tl_next->tl_prev = entry->tl_prev;
        if(entry->tl_prev)
            entry->tl_prev->tl_next = entry->tl_next;
        if(tag_info->head == entry)
            tag_info->head = entry->tl_next;
        tag_info->entry_cnt--;

        /* Reset pointers, to avoid confusion */
        entry->tl_next = NULL;
        entry->tl_prev = NULL;
        entry->tag_info = NULL;

        /* Remove the tag info from the tag list, if there's no more entries with this tag */
        if(!tag_info->corked && 0 == tag_info->entry_cnt) {
            /* Sanity check */
            HDassert(NULL == tag_info->head);

            if(H5SL_remove(cache->tag_list, &(tag_info->tag)) != tag_info)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTREMOVE, FAIL, "can't remove tag info from list")

            /* Release the tag info */
            tag_info = H5FL_FREE(H5C_tag_info_t, tag_info);
        } /* end if */
        else
            HDassert(tag_info->corked || NULL != tag_info->head);
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__untag_entry */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__iter_tagged_entries_real
 *
 * Purpose:     Iterate over tagged entries, making a callback for matches
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Quincey Koziol
 *              June 7, 2016
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5C__iter_tagged_entries_real(H5C_t *cache, haddr_t tag, H5C_tag_iter_cb_t cb,
    void *cb_ctx)
{
    H5C_tag_info_t *tag_info;	        /* Points to a tag info struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    /* Function enter macro */
    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(cache != NULL);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Search the list of tagged object addresses in the cache */
    tag_info = (H5C_tag_info_t *)H5SL_search(cache->tag_list, &tag);

    /* If there's any entries for this tag, iterate over them */
    if(tag_info) {
        H5C_cache_entry_t *entry;       /* Pointer to current entry */
        H5C_cache_entry_t *next_entry;  /* Pointer to next entry in hash bucket chain */

        /* Sanity check */
        HDassert(tag_info->head);
        HDassert(tag_info->entry_cnt > 0);

        /* Iterate over the entries for this tag */
        entry = tag_info->head;
        while(entry) {
            /* Acquire pointer to next entry */
            next_entry = entry->tl_next;

            /* Make callback for entry */
            if((cb)(entry, cb_ctx) != H5_ITER_CONT)
                HGOTO_ERROR(H5E_CACHE, H5E_BADITER, FAIL, "tagged entry iteration callback failed")
            
            /* Advance to next entry */
            entry = next_entry;
        } /* end while */
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__iter_tagged_entries_real() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__iter_tagged_entries
 *
 * Purpose:     Iterate over tagged entries, making a callback for matches
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Quincey Koziol
 *              June 7, 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C__iter_tagged_entries(H5C_t *cache, haddr_t tag, hbool_t match_global,
    H5C_tag_iter_cb_t cb, void *cb_ctx)
{
    herr_t ret_value = SUCCEED;       /* Return value */

    /* Function enter macro */
    FUNC_ENTER_PACKAGE

    /* Sanity checks */
    HDassert(cache != NULL);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Iterate over the entries for this tag */
    if(H5C__iter_tagged_entries_real(cache, tag, cb, cb_ctx) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_BADITER, FAIL, "iteration of tagged entries failed")

    /* Check for iterating over global metadata */
    if(match_global) { 
        /* Iterate over the entries for SOHM entries */
        if(H5C__iter_tagged_entries_real(cache, H5AC__SOHM_TAG, cb, cb_ctx) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_BADITER, FAIL, "iteration of tagged entries failed")
    
        /* Iterate over the entries for global heap entries */
        if(H5C__iter_tagged_entries_real(cache, H5AC__GLOBALHEAP_TAG, cb, cb_ctx) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_BADITER, FAIL, "iteration of tagged entries failed")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__iter_tagged_entries() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__evict_tagged_entries_cb
 *
 * Purpose:     Callback for evicting tagged entries
 *
 * Return:      H5_ITER_ERROR if error is detected, H5_ITER_CONT otherwise.
 *
 * Programmer:  Mike McGreevy
 *              August 19, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
H5C__evict_tagged_entries_cb(H5C_cache_entry_t *entry, void *_ctx)
{
    H5C_tag_iter_evict_ctx_t *ctx = (H5C_tag_iter_evict_ctx_t *)_ctx; /* Get pointer to iterator context */
    int ret_value = H5_ITER_CONT;       /* Return value */

    /* Function enter macro */
    FUNC_ENTER_STATIC

    /* Santify checks */
    HDassert(entry);
    HDassert(ctx);

    /* Attempt to evict entry */
    if(entry->is_protected)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, H5_ITER_ERROR, "Cannot evict protected entry")
    else if(entry->is_dirty)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, H5_ITER_ERROR, "Cannot evict dirty entry")
    else if(entry->is_pinned)
        /* Can't evict at this time, but let's note that we hit a pinned
            entry and we'll loop back around again (as evicting other
            entries will hopefully unpin this entry) */
        ctx->pinned_entries_need_evicted = TRUE;
    else if(!entry->prefetched_dirty) {
        /* Evict the Entry */
        if(H5C__flush_single_entry(ctx->f, entry, H5C__FLUSH_INVALIDATE_FLAG | H5C__FLUSH_CLEAR_ONLY_FLAG | H5C__DEL_FROM_SLIST_ON_DESTROY_FLAG) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, H5_ITER_ERROR, "Entry eviction failed.")
        ctx->evicted_entries_last_pass = TRUE;
    } /* end else-if */
    else
        ctx->skipped_pf_dirty_entries = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__evict_tagged_entries_cb() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_evict_tagged_entries
 *
 * Purpose:     Evicts all entries with the specified tag from cache
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mike McGreevy
 *              August 19, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_evict_tagged_entries(H5F_t * f, haddr_t tag, hbool_t match_global)
{
    H5C_t *cache;                   /* Pointer to cache structure */
    H5C_tag_iter_evict_ctx_t ctx;   /* Context for iterator callback */
    herr_t ret_value = SUCCEED;     /* Return value */

    /* Function enter macro */
    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    cache = f->shared->cache;   /* Get cache pointer */
    HDassert(cache != NULL);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Construct context for iterator callbacks */
    ctx.f = f;

    /* Start evicting entries */
    do {
        /* Reset pinned/evicted tracking flags */
        ctx.pinned_entries_need_evicted = FALSE;
        ctx.evicted_entries_last_pass = FALSE;
        ctx.skipped_pf_dirty_entries = FALSE;

        /* Iterate through entries in the cache */
        if(H5C__iter_tagged_entries(cache, tag, match_global, H5C__evict_tagged_entries_cb, &ctx) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_BADITER, FAIL, "Iteration of tagged entries failed")

        /* Keep doing this until we have stopped evicted entries */
    } while(TRUE == ctx.evicted_entries_last_pass);

    /* In most cases, fail if we have finished evicting entries and pinned 
     * entries still need evicted 
     *
     * However, things can get strange if the file was opened R/O and 
     * the file contains a cache image and the cache image contains dirty 
     * entries.  
     *
     * Since the file was opened read only, dirty entries in the cache 
     * image were marked as clean when they were inserted into the metadata
     * cache.  This is necessary, as if they are marked dirty, the metadata
     * cache will attempt to write them on file close, which is frowned 
     * upon when the file is opened R/O.
     *
     * On the other hand, such entries (marked prefetched_dirty) must not 
     * be evicted, as should the cache be asked to re-load them, the cache
     * will attempt to read them from the file, and at best load an outdated
     * version.
     * 
     * To avoid this, H5C__evict_tagged_entries_cb has been modified to 
     * skip such entries.  However, by doing so, it may prevent pinned 
     * entries from becoming unpinned.
     *
     * Thus we must ignore ctx.pinned_entries_need_evicted if 
     * ctx.skipped_pf_dirty_entries is TRUE.
     */
    if((!ctx.skipped_pf_dirty_entries) && (ctx.pinned_entries_need_evicted))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Pinned entries still need evicted?!")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_evict_tagged_entries() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__mark_tagged_entries_cb
 *
 * Purpose:     Callback to set the flush marker on dirty entries in the cache
 *
 * Return:      H5_ITER_CONT (can't fail)
 *
 * Programmer:  Mike McGreevy
 *              September 9, 2010
 *
 *-------------------------------------------------------------------------
 */
static int
H5C__mark_tagged_entries_cb(H5C_cache_entry_t *entry, void H5_ATTR_UNUSED *_ctx)
{
    /* Function enter macro */
    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(entry);

    /* We only want to set the flush marker on entries that
     * actually need flushed (i.e., dirty ones) */
    if(entry->is_dirty)
        entry->flush_marker = TRUE;

    FUNC_LEAVE_NOAPI(H5_ITER_CONT)
} /* H5C__mark_tagged_entries_cb() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__mark_tagged_entries
 *
 * Purpose:     Set the flush marker on dirty entries in the cache that have
 *              the specified tag, as well as all globally tagged entries.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mike McGreevy
 *              September 9, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t 
H5C__mark_tagged_entries(H5C_t *cache, haddr_t tag)
{
    herr_t ret_value = SUCCEED;         /* Return value */

    /* Function enter macro */
    FUNC_ENTER_STATIC

    /* Sanity check */
    HDassert(cache);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Iterate through hash table entries, marking those with specified tag, as
     * well as any major global entries which should always be flushed
     * when flushing based on tag value */
    if(H5C__iter_tagged_entries(cache, tag, TRUE, H5C__mark_tagged_entries_cb, NULL) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_BADITER, FAIL, "Iteration of tagged entries failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__mark_tagged_entries() */

#if H5C_DO_TAGGING_SANITY_CHECKS

/*-------------------------------------------------------------------------
 *
 * Function:    H5C_verify_tag
 *
 * Purpose:     Performs sanity checking on an entrytype/tag pair.
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              January 14, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_verify_tag(int id, haddr_t tag)
{
    herr_t ret_value = SUCCEED;

    FUNC_ENTER_NOAPI_NOINIT

    /* Perform some sanity checks on tag value. Certain entry
     * types require certain tag values, so check that these
     * constraints are met. */
    if(tag == H5AC__IGNORE_TAG)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "cannot ignore a tag while doing verification.")
    else if(tag == H5AC__INVALID_TAG) {
        if(id != H5AC_PROXY_ENTRY_ID)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "no metadata tag provided")
    } /* end else-if */
    else {
        /* Perform some sanity checks on tag value. Certain entry
         * types require certain tag values, so check that these
         * constraints are met. */

        /* Superblock */
        if((id == H5AC_SUPERBLOCK_ID) || (id == H5AC_DRVRINFO_ID)) {
            if(tag != H5AC__SUPERBLOCK_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "superblock not tagged with H5AC__SUPERBLOCK_TAG")
        } /* end if */
        else {
            if(tag == H5AC__SUPERBLOCK_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "H5AC__SUPERBLOCK_TAG applied to non-superblock entry")
        } /* end else */
    
        /* Free Space Manager */
        if(tag == H5AC__FREESPACE_TAG && ((id != H5AC_FSPACE_HDR_ID) && (id != H5AC_FSPACE_SINFO_ID)))
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "H5AC__FREESPACE_TAG applied to non-freespace entry")
    
        /* SOHM */
        if((id == H5AC_SOHM_TABLE_ID) || (id == H5AC_SOHM_LIST_ID))
            if(tag != H5AC__SOHM_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "sohm entry not tagged with H5AC__SOHM_TAG")
    
        /* Global Heap */
        if(id == H5AC_GHEAP_ID) {
            if(tag != H5AC__GLOBALHEAP_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "global heap not tagged with H5AC__GLOBALHEAP_TAG")
        } /* end if */
        else {
            if(tag == H5AC__GLOBALHEAP_TAG)
                HGOTO_ERROR(H5E_CACHE, H5E_CANTTAG, FAIL, "H5AC__GLOBALHEAP_TAG applied to non-globalheap entry")
        } /* end else */
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_verify_tag */
#endif


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_flush_tagged_entries
 *
 * Purpose:     Flushes all entries with the specified tag to disk.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Mike McGreevy
 *              August 19, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_flush_tagged_entries(H5F_t *f, haddr_t tag)
{
    /* Variable Declarations */
    H5C_t      *cache_ptr = NULL;
    herr_t      ret_value = SUCCEED;

    FUNC_ENTER_NOAPI(FAIL)

    /* Assertions */
    HDassert(f);
    HDassert(f->shared);

    /* Get cache pointer */
    cache_ptr = f->shared->cache;

    /* Mark all entries with specified tag */
    if(H5C__mark_tagged_entries(cache_ptr, tag) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't mark tagged entries")

    /* Flush all marked entries */
    if(H5C__flush_marked_entries(f) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_CANTFLUSH, FAIL, "Can't flush marked entries")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_flush_tagged_entries */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_retag_entries
 *
 * Purpose:     Searches through cache index for all entries with the
 *              value specified by src_tag and changes it to the value
 *              specified by dest_tag.
 *
 * Return:      SUCCEED or FAIL.
 *
 * Programmer:  Mike McGreevy
 *              March 17, 2010
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_retag_entries(H5C_t *cache, haddr_t src_tag, haddr_t dest_tag) 
{
    H5C_tag_info_t *tag_info;	        /* Points to a tag info struct */
    herr_t ret_value = SUCCEED;         /* Return value */

    /* Function enter macro */
    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(cache);

    /* Remove tag info from tag list */
    if(NULL != (tag_info = (H5C_tag_info_t *)H5SL_remove(cache->tag_list, &src_tag))) {
        /* Change to new tag */
        tag_info->tag = dest_tag;

        /* Re-insert tag info into skip list */
        if(H5SL_insert(cache->tag_list, tag_info, &(tag_info->tag)) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTINSERT, FAIL, "can't insert tag info in skip list")
    } /* end if */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_retag_entries() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C__expunge_tag_type_metadata_cb
 *
 * Purpose:     Expunge from the cache entries associated
 *              with 'tag' and type id.
 *
 * Return:      H5_ITER_ERROR if error is detected, H5_ITER_CONT otherwise.
 *
 * Programmer:  Vailin Choi
 *		May 2016
 *
 *-------------------------------------------------------------------------
 */
static int
H5C__expunge_tag_type_metadata_cb(H5C_cache_entry_t *entry, void *_ctx)
{
    H5C_tag_iter_ettm_ctx_t *ctx = (H5C_tag_iter_ettm_ctx_t *)_ctx; /* Get pointer to iterator context */
    int ret_value = H5_ITER_CONT;       /* Return value */

    /* Function enter macro */
    FUNC_ENTER_STATIC

    /* Santify checks */
    HDassert(entry);
    HDassert(ctx);

    /* Found one with the same tag and type id */
    if(entry->type->id == ctx->type_id)
        if(H5C_expunge_entry(ctx->f, entry->type, entry->addr, ctx->flags) < 0)
            HGOTO_ERROR(H5E_CACHE, H5E_CANTEXPUNGE, H5_ITER_ERROR, "can't expunge entry")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C__expunge_tag_type_metadata_cb() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_expunge_tag_type_metadata
 *
 * Purpose:     Search and expunge from the cache entries associated
 *              with 'tag' and type id.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  Vailin Choi
 *		May 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5C_expunge_tag_type_metadata(H5F_t *f, haddr_t tag, int type_id, unsigned flags)
{
    H5C_t *cache;                       /* Pointer to cache structure */
    H5C_tag_iter_ettm_ctx_t ctx;        /* Context for iterator callback */
    herr_t ret_value = SUCCEED;         /* Return value */

    /* Function enter macro */
    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(f);
    HDassert(f->shared);
    cache = f->shared->cache;   /* Get cache pointer */
    HDassert(cache != NULL);
    HDassert(cache->magic == H5C__H5C_T_MAGIC);

    /* Construct context for iterator callbacks */
    ctx.f = f;
    ctx.type_id = type_id;
    ctx.flags = flags;

    /* Iterate through hash table entries, expunge those with specified tag and type id */
    if(H5C__iter_tagged_entries(cache, tag, FALSE, H5C__expunge_tag_type_metadata_cb, &ctx) < 0)
        HGOTO_ERROR(H5E_CACHE, H5E_BADITER, FAIL, "Iteration of tagged entries failed")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_expunge_tag_type_metadata() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_get_tag()
 *
 * Purpose:     Get the tag for a metadata cache entry.
 *
 * Return:      SUCCEED (can't fail)
 *
 * Programmer:  Dana Robinson
 *              Fall 2016
 *
 *-------------------------------------------------------------------------
 */
herr_t 
H5C_get_tag(const void *thing, haddr_t *tag /*OUT*/)
{
    const H5C_cache_entry_t *entry = (const H5C_cache_entry_t *)thing;  /* Pointer to cache entry */

    FUNC_ENTER_NOAPI_NOERR

    HDassert(entry);
    HDassert(entry->tag_info);
    HDassert(tag);

    /* Return the tag */
    *tag = entry->tag_info->tag;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5C_get_tag() */


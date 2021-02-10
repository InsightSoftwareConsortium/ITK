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
 * Created:     H5Cdbg.c
 *              July 8 2016
 *              Quincey Koziol
 *
 * Purpose:     Debugging Routines for the generic cache structure or entries.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Cmodule.h"          /* This source code file is part of the H5C module */

#define H5AC_FRIEND




/***********/
/* Headers */
/***********/
#include "H5private.h"      /* Generic Functions            */
#include "H5ACpkg.h"        /* Metadata Cache               */
#include "H5Cpkg.h"         /* Cache                        */
#include "H5Eprivate.h"     /* Error Handling               */


/****************/
/* Local Macros */
/****************/


/******************/
/* Local Typedefs */
/******************/


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


#ifndef NDEBUG

/*-------------------------------------------------------------------------
 * Function:    H5C_dump_cache
 *
 * Purpose:     Print a summary of the contents of the metadata cache for
 *              debugging purposes.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              10/10/10
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_dump_cache(H5C_t * cache_ptr, const char *  cache_name)
{
    H5C_cache_entry_t * entry_ptr;
    H5SL_t *            slist_ptr = NULL;
    int                 i;                      /* Local index variable */
    herr_t              ret_value = SUCCEED;    /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity check */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(cache_name != NULL );

    /* First, create a skip list */
    if(NULL == (slist_ptr = H5SL_create(H5SL_TYPE_HADDR, NULL)))
        HGOTO_ERROR(H5E_CACHE, H5E_CANTCREATE, FAIL, "can't create skip list")

    /* Next, scan the index, and insert all entries in the skip list.
     * Do this, as we want to display cache entries in increasing address
     * order.
     */
    for(i = 0; i < H5C__HASH_TABLE_LEN; i++) {
        entry_ptr = cache_ptr->index[i];

        while(entry_ptr != NULL) {
            HDassert(entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
            if(H5SL_insert(slist_ptr, entry_ptr, &(entry_ptr->addr)) < 0)
                HGOTO_ERROR(H5E_CACHE, H5E_BADVALUE, FAIL, "can't insert entry in skip list")

            entry_ptr = entry_ptr->ht_next;
        } /* end while */
    } /* end for */

    /* If we get this far, all entries in the cache are listed in the
     * skip list -- scan the skip list generating the desired output.
     */

    HDfprintf(stdout, "\n\nDump of metadata cache \"%s\"\n", cache_name);

    /* Print header */
    HDfprintf(stdout, "Entry ");
    HDfprintf(stdout, "|       Address      ");
    HDfprintf(stdout, "|         Tag        ");
    HDfprintf(stdout, "|  Size ");
    HDfprintf(stdout, "| Ring ");
    HDfprintf(stdout, "|              Type              ");
    HDfprintf(stdout, "| Prot/Pin/Dirty");
    HDfprintf(stdout, "\n");

    HDfprintf(stdout, "----------------------------------------------------------------------------------------------------------------\n");

    i = 0;
    entry_ptr = (H5C_cache_entry_t *)H5SL_remove_first(slist_ptr);
    while(entry_ptr != NULL) {
        HDassert(entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);

        /* Print entry */
        HDfprintf(stdout, "%s%5d ", cache_ptr->prefix, i);
        HDfprintf(stdout, "  0x%16llx ", (long long)(entry_ptr->addr));
        if(NULL == entry_ptr->tag_info)
            HDfprintf(stdout, "    %16s ", "N/A");
        else
            HDfprintf(stdout, "  0x%16llx ", (long long)(entry_ptr->tag_info->tag));
        HDfprintf(stdout, "  %5lld ", (long long)(entry_ptr->size));
        HDfprintf(stdout, "    %d  ", (int)(entry_ptr->ring));
        HDfprintf(stdout, "  %2d %-32s ", (int)(entry_ptr->type->id), (entry_ptr->type->name));
        HDfprintf(stdout, " %d", (int)(entry_ptr->is_protected));
        HDfprintf(stdout, " %d", (int)(entry_ptr->is_pinned));
        HDfprintf(stdout, " %d", (int)(entry_ptr->is_dirty));
        HDfprintf(stdout, "\n");

        /* remove the next (first) item in the skip list */
        entry_ptr = (H5C_cache_entry_t *)H5SL_remove_first(slist_ptr);

        i++;
    } /* end while */

    HDfprintf(stdout, "\n\n");

    /* Verify that all the entries were removed from the skip list */
    HDassert(H5SL_count(slist_ptr) == 0);

done:
    /* Discard the skip list */
    if(slist_ptr)
        H5SL_close(slist_ptr);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_dump_cache() */
#endif /* NDEBUG */

#ifndef NDEBUG

/*-------------------------------------------------------------------------
 * Function:    H5C_dump_cache_LRU
 *
 * Purpose:     Print a summary of the contents of the metadata cache 
 *              LRU for debugging purposes.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              10/10/10
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_dump_cache_LRU(H5C_t *cache_ptr, const char *cache_name)
{
    H5C_cache_entry_t * entry_ptr;
    int                 i = 0;

    FUNC_ENTER_NOAPI_NOERR

    /* Sanity check */
    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(cache_name != NULL );

    HDfprintf(stdout, "\n\nDump of metadata cache LRU \"%s\"\n", cache_name);
    HDfprintf(stdout, "LRU len = %d, LRU size = %d\n", 
              cache_ptr->LRU_list_len, (int)(cache_ptr->LRU_list_size));
    HDfprintf(stdout, "index_size = %d, max_cache_size = %d, delta = %d\n\n", 
              (int)(cache_ptr->index_size), (int)(cache_ptr->max_cache_size),
              (int)(cache_ptr->max_cache_size) - (int)(cache_ptr->index_size));

    /* Print header */
    HDfprintf(stdout, "Entry ");
    HDfprintf(stdout, "|       Address      ");
    HDfprintf(stdout, "|         Tag        ");
    HDfprintf(stdout, "|  Size ");
    HDfprintf(stdout, "| Ring ");
    HDfprintf(stdout, "|              Type              ");
    HDfprintf(stdout, "| Dirty");
    HDfprintf(stdout, "\n");

    HDfprintf(stdout, "----------------------------------------------------------------------------------------------------------------\n");

    entry_ptr = cache_ptr->LRU_head_ptr;
    while(entry_ptr != NULL) {
        HDassert(entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);

        /* Print entry */
        HDfprintf(stdout, "%s%5d ", cache_ptr->prefix, i);
        HDfprintf(stdout, "  0x%16llx ", (long long)(entry_ptr->addr));

        if(NULL == entry_ptr->tag_info)
            HDfprintf(stdout, "    %16s ", "N/A");
        else
            HDfprintf(stdout, "  0x%16llx ", 
                      (long long)(entry_ptr->tag_info->tag));

        HDfprintf(stdout, "  %5lld ", (long long)(entry_ptr->size));
        HDfprintf(stdout, "    %d  ", (int)(entry_ptr->ring));
        HDfprintf(stdout, "  %2d %-32s ", (int)(entry_ptr->type->id), 
                  (entry_ptr->type->name));
        HDfprintf(stdout, " %d", (int)(entry_ptr->is_dirty));
        HDfprintf(stdout, "\n");

        i++;
        entry_ptr = entry_ptr->next;
    } /* end while */

    HDfprintf(stdout, "----------------------------------------------------------------------------------------------------------------\n");

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* H5C_dump_cache_LRU() */
#endif /* NDEBUG */


/*-------------------------------------------------------------------------
 * Function:    H5C_dump_cache_skip_list
 *
 * Purpose:     Debugging routine that prints a summary of the contents of 
 *		the skip list used by the metadata cache metadata cache to 
 *		maintain an address sorted list of dirty entries.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              11/15/14
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
herr_t
H5C_dump_cache_skip_list(H5C_t * cache_ptr, char * calling_fcn)
{
    herr_t              ret_value = SUCCEED;   /* Return value */
    int                 i;
    H5C_cache_entry_t * entry_ptr = NULL;
    H5SL_node_t *       node_ptr = NULL;

    FUNC_ENTER_NOAPI_NOERR

    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(calling_fcn != NULL);

    HDfprintf(stdout, "\n\nDumping metadata cache skip list from %s.\n", calling_fcn);
    HDfprintf(stdout, "	slist len = %u.\n", cache_ptr->slist_len);
    HDfprintf(stdout, "	slist size = %lld.\n", (long long)(cache_ptr->slist_size));

    if(cache_ptr->slist_len > 0) {
        /* If we get this far, all entries in the cache are listed in the
         * skip list -- scan the skip list generating the desired output.
         */
        HDfprintf(stdout,
                  "Num:    Addr:               Len: Prot/Pind: Dirty: Type:\n");

        i = 0;
        node_ptr = H5SL_first(cache_ptr->slist_ptr);
        if(node_ptr != NULL)
            entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);
        else
            entry_ptr = NULL;

        while(entry_ptr != NULL) {
            HDassert( entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC );

            HDfprintf(stdout,
               "%s%d       0x%016llx  %4lld    %d/%d       %d    %s\n",
               cache_ptr->prefix, i,
               (long long)(entry_ptr->addr),
               (long long)(entry_ptr->size),
               (int)(entry_ptr->is_protected),
               (int)(entry_ptr->is_pinned),
               (int)(entry_ptr->is_dirty),
               entry_ptr->type->name);

            HDfprintf(stdout, "		node_ptr = 0x%llx, item = %p\n",
                      (unsigned long long)node_ptr,
                      H5SL_item(node_ptr));

            /* increment node_ptr before we delete its target */
            node_ptr = H5SL_next(node_ptr);
            if(node_ptr != NULL)
                entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);
            else
                entry_ptr = NULL;

            i++;
        } /* end while */
    } /* end if */

    HDfprintf(stdout, "\n\n");

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_dump_cache_skip_list() */
#endif /* NDEBUG */


/*-------------------------------------------------------------------------
 * Function:    H5C_dump_coll_write_list
 *
 * Purpose:     Debugging routine that prints a summary of the contents of 
 *		the collective write skip list used by the metadata cache 
 *              in the parallel case to maintain a list of entries to write 
 *              collectively at a sync point.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              4/1/17
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_PARALLEL
#ifndef NDEBUG
herr_t
H5C_dump_coll_write_list(H5C_t * cache_ptr, char * calling_fcn)
{
    herr_t              ret_value = SUCCEED;   /* Return value */
    int                 i;
    int                 list_len;
    H5AC_aux_t *        aux_ptr = NULL;
    H5C_cache_entry_t * entry_ptr = NULL;
    H5SL_node_t *       node_ptr = NULL;

    FUNC_ENTER_NOAPI_NOERR

    HDassert(cache_ptr != NULL);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(cache_ptr->aux_ptr);

    aux_ptr = (H5AC_aux_t *)cache_ptr->aux_ptr;

    HDassert(aux_ptr->magic == H5AC__H5AC_AUX_T_MAGIC);

    HDassert(calling_fcn != NULL);

    list_len = (int)H5SL_count(cache_ptr->coll_write_list);

    HDfprintf(stdout, "\n\nDumping MDC coll write list from %d:%s.\n", 
              aux_ptr->mpi_rank, calling_fcn);
    HDfprintf(stdout, "	slist len = %u.\n", cache_ptr->slist_len);

    if ( list_len > 0 ) {

        /* scan the collective write list generating the desired output */
        HDfprintf(stdout,
                  "Num:    Addr:               Len: Prot/Pind: Dirty: Type:\n");

        i = 0;

        node_ptr = H5SL_first(cache_ptr->coll_write_list);

        if ( node_ptr != NULL )

            entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);

        else

            entry_ptr = NULL;

        while ( entry_ptr != NULL ) {

            HDassert(entry_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);

            HDfprintf(stdout,
               "%s%d       0x%016llx  %4lld    %d/%d       %d    %s\n",
               cache_ptr->prefix, i,
               (long long)(entry_ptr->addr),
               (long long)(entry_ptr->size),
               (int)(entry_ptr->is_protected),
               (int)(entry_ptr->is_pinned),
               (int)(entry_ptr->is_dirty),
               entry_ptr->type->name);

            node_ptr = H5SL_next(node_ptr);

            if ( node_ptr != NULL )

                entry_ptr = (H5C_cache_entry_t *)H5SL_item(node_ptr);

            else

                entry_ptr = NULL;

            i++;

        } /* end while */
    } /* end if */

    HDfprintf(stdout, "\n\n");

    FUNC_LEAVE_NOAPI(ret_value)

} /* H5C_dump_coll_write_list() */
#endif /* NDEBUG */
#endif /* H5_HAVE_PARALLEL */


/*-------------------------------------------------------------------------
 * Function:    H5C_set_prefix
 *
 * Purpose:     Set the values of the prefix field of H5C_t.  This
 *		filed is used to label some debugging output.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              1/20/06
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_set_prefix(H5C_t * cache_ptr, char * prefix)
{
    herr_t ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    if((cache_ptr == NULL) || (cache_ptr->magic != H5C__H5C_T_MAGIC) ||
            (prefix == NULL) || (HDstrlen(prefix) >= H5C__PREFIX_LEN))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad param(s) on entry")

    HDstrncpy(&(cache_ptr->prefix[0]), prefix, (size_t)(H5C__PREFIX_LEN));

    cache_ptr->prefix[H5C__PREFIX_LEN - 1] = '\0';

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_set_prefix() */


/*-------------------------------------------------------------------------
 * Function:    H5C_stats
 *
 * Purpose:     Prints statistics about the cache.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  John Mainzer
 *              6/2/04
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5C_stats(H5C_t * cache_ptr,
          const char *  cache_name,
          hbool_t
#if !H5C_COLLECT_CACHE_STATS
          H5_ATTR_UNUSED
#endif /* H5C_COLLECT_CACHE_STATS */
          display_detailed_stats)
{
#if H5C_COLLECT_CACHE_STATS
    int		i;
    int64_t     total_hits = 0;
    int64_t     total_misses = 0;
    int64_t	total_write_protects = 0;
    int64_t	total_read_protects = 0;
    int64_t	max_read_protects = 0;
    int64_t     total_insertions = 0;
    int64_t     total_pinned_insertions = 0;
    int64_t     total_clears = 0;
    int64_t     total_flushes = 0;
    int64_t     total_evictions = 0;
    int64_t     total_take_ownerships = 0;
    int64_t     total_moves = 0;
    int64_t     total_entry_flush_moves = 0;
    int64_t     total_cache_flush_moves = 0;
    int64_t	total_size_increases = 0;
    int64_t	total_size_decreases = 0;
    int64_t	total_entry_flush_size_changes = 0;
    int64_t	total_cache_flush_size_changes = 0;
    int64_t	total_pins = 0;
    int64_t	total_unpins = 0;
    int64_t	total_dirty_pins = 0;
    int64_t	total_pinned_flushes = 0;
    int64_t	total_pinned_clears = 0;
    int32_t     aggregate_max_accesses = 0;
    int32_t     aggregate_min_accesses = 1000000;
    int32_t     aggregate_max_clears = 0;
    int32_t     aggregate_max_flushes = 0;
    size_t      aggregate_max_size = 0;
    int32_t	aggregate_max_pins = 0;
    double      hit_rate;
    double      prefetch_use_rate;
    double	average_successful_search_depth = 0.0f;
    double	average_failed_search_depth = 0.0f;
    double      average_entries_skipped_per_calls_to_msic = 0.0f;
    double      average_dirty_pf_entries_skipped_per_call_to_msic = 0.0f;
    double      average_entries_scanned_per_calls_to_msic = 0.0f;
#endif /* H5C_COLLECT_CACHE_STATS */
    herr_t	ret_value = SUCCEED;   /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    HDassert( cache_ptr->magic == H5C__H5C_T_MAGIC );

    /* This would normally be an assert, but we need to use an HGOTO_ERROR
     * call to shut up the compiler.
     */
    if((NULL == cache_ptr) || (cache_ptr->magic != H5C__H5C_T_MAGIC) ||
            (NULL == cache_name))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Bad cache_ptr or cache_name")

#if H5C_COLLECT_CACHE_STATS
    for(i = 0; i <= cache_ptr->max_type_id; i++ ) {
        total_hits              += cache_ptr->hits[i];
        total_misses            += cache_ptr->misses[i];
	total_write_protects	+= cache_ptr->write_protects[i];
	total_read_protects	+= cache_ptr->read_protects[i];
	if(max_read_protects < cache_ptr->max_read_protects[i])
	    max_read_protects = cache_ptr->max_read_protects[i];
        total_insertions        += cache_ptr->insertions[i];
        total_pinned_insertions += cache_ptr->pinned_insertions[i];
        total_clears            += cache_ptr->clears[i];
        total_flushes           += cache_ptr->flushes[i];
        total_evictions         += cache_ptr->evictions[i];
        total_take_ownerships   += cache_ptr->take_ownerships[i];
        total_moves             += cache_ptr->moves[i];
	total_entry_flush_moves += cache_ptr->entry_flush_moves[i];
	total_cache_flush_moves += cache_ptr->cache_flush_moves[i];
        total_size_increases    += cache_ptr->size_increases[i];
        total_size_decreases    += cache_ptr->size_decreases[i];
    	total_entry_flush_size_changes
				+= cache_ptr->entry_flush_size_changes[i];
    	total_cache_flush_size_changes
				+= cache_ptr->cache_flush_size_changes[i];
	total_pins              += cache_ptr->pins[i];
	total_unpins            += cache_ptr->unpins[i];
	total_dirty_pins        += cache_ptr->dirty_pins[i];
	total_pinned_flushes    += cache_ptr->pinned_flushes[i];
	total_pinned_clears     += cache_ptr->pinned_clears[i];
#if H5C_COLLECT_CACHE_ENTRY_STATS
        if(aggregate_max_accesses < cache_ptr->max_accesses[i])
            aggregate_max_accesses = cache_ptr->max_accesses[i];
        if(aggregate_min_accesses > aggregate_max_accesses)
            aggregate_min_accesses = aggregate_max_accesses;
        if(aggregate_min_accesses > cache_ptr->min_accesses[i])
            aggregate_min_accesses = cache_ptr->min_accesses[i];
        if(aggregate_max_clears < cache_ptr->max_clears[i])
            aggregate_max_clears = cache_ptr->max_clears[i];
        if(aggregate_max_flushes < cache_ptr->max_flushes[i])
            aggregate_max_flushes = cache_ptr->max_flushes[i];
        if(aggregate_max_size < cache_ptr->max_size[i])
            aggregate_max_size = cache_ptr->max_size[i];
        if(aggregate_max_pins < cache_ptr->max_pins[i])
            aggregate_max_pins = cache_ptr->max_pins[i];
#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */
    } /* end for */

    if((total_hits > 0) || (total_misses > 0))
        hit_rate = (double)100.0f * ((double)(total_hits)) /
                   ((double)(total_hits + total_misses));
    else
        hit_rate = 0.0f;

    if(cache_ptr->successful_ht_searches > 0)
        average_successful_search_depth =
            ((double)(cache_ptr->total_successful_ht_search_depth)) /
            ((double)(cache_ptr->successful_ht_searches));

    if(cache_ptr->failed_ht_searches > 0)
        average_failed_search_depth =
            ((double)(cache_ptr->total_failed_ht_search_depth)) /
            ((double)(cache_ptr->failed_ht_searches));


    HDfprintf(stdout, "\n%sH5C: cache statistics for %s\n",
              cache_ptr->prefix, cache_name);

    HDfprintf(stdout, "\n");

    HDfprintf(stdout,
              "%s  hash table insertion / deletions   = %ld / %ld\n",
              cache_ptr->prefix,
              (long)(cache_ptr->total_ht_insertions),
              (long)(cache_ptr->total_ht_deletions));

    HDfprintf(stdout,
              "%s  HT successful / failed searches    = %ld / %ld\n",
              cache_ptr->prefix,
              (long)(cache_ptr->successful_ht_searches),
              (long)(cache_ptr->failed_ht_searches));

    HDfprintf(stdout,
              "%s  Av. HT suc / failed search depth   = %f / %f\n",
              cache_ptr->prefix,
              average_successful_search_depth,
              average_failed_search_depth);

    HDfprintf(stdout,
             "%s  current (max) index size / length  = %ld (%ld) / %lu (%lu)\n",
              cache_ptr->prefix,
              (long)(cache_ptr->index_size),
              (long)(cache_ptr->max_index_size),
              (unsigned long)(cache_ptr->index_len),
              (unsigned long)(cache_ptr->max_index_len));

    HDfprintf(stdout,
             "%s  current (max) clean/dirty idx size = %ld (%ld) / %ld (%ld)\n",
              cache_ptr->prefix,
              (long)(cache_ptr->clean_index_size),
              (long)(cache_ptr->max_clean_index_size),
              (long)(cache_ptr->dirty_index_size),
              (long)(cache_ptr->max_dirty_index_size));

    HDfprintf(stdout,
             "%s  current (max) slist size / length  = %ld (%ld) / %lu (%lu)\n",
              cache_ptr->prefix,
              (long)(cache_ptr->slist_size),
              (long)(cache_ptr->max_slist_size),
              (unsigned long)(cache_ptr->slist_len),
              (unsigned long)(cache_ptr->max_slist_len));

    HDfprintf(stdout,
             "%s  current (max) PL size / length     = %ld (%ld) / %lu (%lu)\n",
              cache_ptr->prefix,
              (long)(cache_ptr->pl_size),
              (long)(cache_ptr->max_pl_size),
              (unsigned long)(cache_ptr->pl_len),
              (unsigned long)(cache_ptr->max_pl_len));

    HDfprintf(stdout,
             "%s  current (max) PEL size / length    = %ld (%ld) / %lu (%lu)\n",
              cache_ptr->prefix,
              (long)(cache_ptr->pel_size),
              (long)(cache_ptr->max_pel_size),
              (unsigned long)(cache_ptr->pel_len),
              (unsigned long)(cache_ptr->max_pel_len));

    HDfprintf(stdout,
              "%s  current LRU list size / length     = %ld / %lu\n",
              cache_ptr->prefix,
              (long)(cache_ptr->LRU_list_size),
              (unsigned long)(cache_ptr->LRU_list_len));

#if H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS
    HDfprintf(stdout,
              "%s  current clean LRU size / length    = %ld / %lu\n",
              cache_ptr->prefix,
              (long)(cache_ptr->cLRU_list_size),
              (unsigned long)(cache_ptr->cLRU_list_len));

    HDfprintf(stdout,
              "%s  current dirty LRU size / length    = %ld / %lu\n",
              cache_ptr->prefix,
              (long)(cache_ptr->dLRU_list_size),
              (unsigned long)(cache_ptr->dLRU_list_len));
#endif /* H5C_MAINTAIN_CLEAN_AND_DIRTY_LRU_LISTS */

    HDfprintf(stdout,
              "%s  Total hits / misses / hit_rate     = %ld / %ld / %f\n",
              cache_ptr->prefix,
              (long)total_hits,
              (long)total_misses,
              hit_rate);

    HDfprintf(stdout,
              "%s  Total write / read (max) protects  = %ld / %ld (%ld)\n",
              cache_ptr->prefix,
              (long)total_write_protects,
              (long)total_read_protects,
              (long)max_read_protects);

    HDfprintf(stdout,
              "%s  Total clears / flushes             = %ld / %ld\n",
              cache_ptr->prefix,
              (long)total_clears,
              (long)total_flushes);

    HDfprintf(stdout,
              "%s  Total evictions / take ownerships  = %ld / %ld\n",
              cache_ptr->prefix,
              (long)total_evictions,
              (long)total_take_ownerships);

    HDfprintf(stdout,
	      "%s  Total insertions(pinned) / moves   = %ld(%ld) / %ld\n",
              cache_ptr->prefix,
              (long)total_insertions,
              (long)total_pinned_insertions,
              (long)total_moves);

    HDfprintf(stdout,
	      "%s  Total entry / cache flush moves    = %ld / %ld\n",
              cache_ptr->prefix,
              (long)total_entry_flush_moves,
              (long)total_cache_flush_moves);

    HDfprintf(stdout, "%s  Total entry size incrs / decrs     = %ld / %ld\n",
              cache_ptr->prefix,
              (long)total_size_increases,
              (long)total_size_decreases);

    HDfprintf(stdout, "%s  Ttl entry/cache flush size changes = %ld / %ld\n",
              cache_ptr->prefix,
              (long)total_entry_flush_size_changes,
              (long)total_cache_flush_size_changes);

    HDfprintf(stdout,
	      "%s  Total entry pins (dirty) / unpins  = %ld (%ld) / %ld\n",
              cache_ptr->prefix,
              (long)total_pins,
	      (long)total_dirty_pins,
              (long)total_unpins);

    HDfprintf(stdout, "%s  Total pinned flushes / clears      = %ld / %ld\n",
              cache_ptr->prefix,
              (long)total_pinned_flushes,
              (long)total_pinned_clears);

    HDfprintf(stdout, "%s  MSIC: (make space in cache) calls  = %lld\n",
              cache_ptr->prefix,
              (long long)(cache_ptr->calls_to_msic));

    if (cache_ptr->calls_to_msic > 0)
        average_entries_skipped_per_calls_to_msic =
            (((double)(cache_ptr->total_entries_skipped_in_msic)) /
            ((double)(cache_ptr->calls_to_msic)));

    HDfprintf(stdout, "%s  MSIC: Average/max entries skipped  = %lf / %ld\n",
              cache_ptr->prefix,
              (double)average_entries_skipped_per_calls_to_msic,
              (long)(cache_ptr->max_entries_skipped_in_msic));

    if(cache_ptr->calls_to_msic > 0)
        average_dirty_pf_entries_skipped_per_call_to_msic =
            (((double)(cache_ptr->total_dirty_pf_entries_skipped_in_msic)) /
            ((double)(cache_ptr->calls_to_msic)));

    HDfprintf(stdout, 
              "%s  MSIC: Average/max dirty pf entries skipped  = %lf / %ld\n",
              cache_ptr->prefix,
              average_dirty_pf_entries_skipped_per_call_to_msic,
              (long)(cache_ptr->max_dirty_pf_entries_skipped_in_msic));

    if(cache_ptr->calls_to_msic > 0)
        average_entries_scanned_per_calls_to_msic =
            (((double)(cache_ptr->total_entries_scanned_in_msic)) /
            ((double)(cache_ptr->calls_to_msic)));

    HDfprintf(stdout, "%s  MSIC: Average/max entries scanned  = %lf / %ld\n",
              cache_ptr->prefix,
              (double)average_entries_scanned_per_calls_to_msic,
              (long)(cache_ptr->max_entries_scanned_in_msic));

    HDfprintf(stdout, "%s  MSIC: Scanned to make space(evict) = %lld\n",
              cache_ptr->prefix,
              (long long)(cache_ptr->entries_scanned_to_make_space));

    HDfprintf(stdout, "%s  MSIC: Scanned to satisfy min_clean = %lld\n",
              cache_ptr->prefix,
              (long long)(cache_ptr->total_entries_scanned_in_msic -
                            cache_ptr->entries_scanned_to_make_space));

    HDfprintf(stdout, 
              "%s  slist/LRU/index scan restarts   = %lld / %lld / %lld.\n",
              cache_ptr->prefix, 
              (long long)(cache_ptr->slist_scan_restarts),
              (long long)(cache_ptr->LRU_scan_restarts),
              (long long)(cache_ptr->index_scan_restarts));

    HDfprintf(stdout,
	    "%s  cache image creations/reads/loads/size = %d / %d /%d / %Hu\n",
              cache_ptr->prefix,
              cache_ptr->images_created,
              cache_ptr->images_read,
              cache_ptr->images_loaded,
              cache_ptr->last_image_size);

    HDfprintf(stdout,
	      "%s  prefetches / dirty prefetches      = %lld / %lld\n",
              cache_ptr->prefix,
              (long long)(cache_ptr->prefetches),
              (long long)(cache_ptr->dirty_prefetches));

    HDfprintf(stdout,
	      "%s  prefetch hits/flushes/evictions    = %lld / %lld / %lld\n",
              cache_ptr->prefix,
              (long long)(cache_ptr->prefetch_hits),
              (long long)(cache_ptr->flushes[H5AC_PREFETCHED_ENTRY_ID]),
              (long long)(cache_ptr->evictions[H5AC_PREFETCHED_ENTRY_ID]));

    if(cache_ptr->prefetches > 0)
        prefetch_use_rate = 
                   (double)100.0f * ((double)(cache_ptr->prefetch_hits)) /
                   ((double)(cache_ptr->prefetches));
    else
        prefetch_use_rate = 0.0f;

    HDfprintf(stdout,
	      "%s  prefetched entry use rate          = %lf\n",
              cache_ptr->prefix,
              prefetch_use_rate);

#if H5C_COLLECT_CACHE_ENTRY_STATS

    HDfprintf(stdout, "%s  aggregate max / min accesses       = %d / %d\n",
              cache_ptr->prefix,
              (int)aggregate_max_accesses,
              (int)aggregate_min_accesses);

    HDfprintf(stdout, "%s  aggregate max_clears / max_flushes = %d / %d\n",
              cache_ptr->prefix,
              (int)aggregate_max_clears,
              (int)aggregate_max_flushes);

    HDfprintf(stdout, "%s  aggregate max_size / max_pins      = %d / %d\n",
              cache_ptr->prefix,
              (int)aggregate_max_size,
	      (int)aggregate_max_pins);

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

    if(display_detailed_stats) {
        for(i = 0; i <= cache_ptr->max_type_id; i++) {
            HDfprintf(stdout, "\n");

            HDfprintf(stdout, "%s  Stats on %s:\n",
                      cache_ptr->prefix,
                      ((cache_ptr->class_table_ptr))[i]->name);

            if((cache_ptr->hits[i] > 0) || (cache_ptr->misses[i] > 0))
                hit_rate = (double)100.0f * ((double)(cache_ptr->hits[i])) /
                          ((double)(cache_ptr->hits[i] + cache_ptr->misses[i]));
            else
                hit_rate = 0.0f;

            HDfprintf(stdout,
                      "%s    hits / misses / hit_rate       = %ld / %ld / %f\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->hits[i]),
                      (long)(cache_ptr->misses[i]),
                      hit_rate);

            HDfprintf(stdout,
                      "%s    write / read (max) protects    = %ld / %ld (%d)\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->write_protects[i]),
                      (long)(cache_ptr->read_protects[i]),
                      (int)(cache_ptr->max_read_protects[i]));

            HDfprintf(stdout,
                      "%s    clears / flushes               = %ld / %ld\n", 
                      cache_ptr->prefix,
                      (long)(cache_ptr->clears[i]),
                      (long)(cache_ptr->flushes[i]));

            HDfprintf(stdout,
                      "%s    evictions / take ownerships    = %ld / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->evictions[i]),
                      (long)(cache_ptr->take_ownerships[i]));

            HDfprintf(stdout,
                      "%s    insertions(pinned) / moves     = %ld(%ld) / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->insertions[i]),
                      (long)(cache_ptr->pinned_insertions[i]),
                      (long)(cache_ptr->moves[i]));

            HDfprintf(stdout,
                      "%s    entry / cache flush moves      = %ld / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->entry_flush_moves[i]),
                      (long)(cache_ptr->cache_flush_moves[i]));

            HDfprintf(stdout,
                      "%s    size increases / decreases     = %ld / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->size_increases[i]),
                      (long)(cache_ptr->size_decreases[i]));

            HDfprintf(stdout,
                      "%s    entry/cache flush size changes = %ld / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->entry_flush_size_changes[i]),
                      (long)(cache_ptr->cache_flush_size_changes[i]));


            HDfprintf(stdout,
                      "%s    entry pins / unpins            = %ld / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->pins[i]),
                      (long)(cache_ptr->unpins[i]));

            HDfprintf(stdout,
                      "%s    entry dirty pins/pin'd flushes = %ld / %ld\n",
                      cache_ptr->prefix,
                      (long)(cache_ptr->dirty_pins[i]),
                      (long)(cache_ptr->pinned_flushes[i]));

#if H5C_COLLECT_CACHE_ENTRY_STATS

            HDfprintf(stdout,
                      "%s    entry max / min accesses       = %d / %d\n",
                      cache_ptr->prefix,
                      cache_ptr->max_accesses[i],
                      cache_ptr->min_accesses[i]);

            HDfprintf(stdout,
                      "%s    entry max_clears / max_flushes = %d / %d\n",
                      cache_ptr->prefix,
                      cache_ptr->max_clears[i],
                      cache_ptr->max_flushes[i]);

            HDfprintf(stdout,
                      "%s    entry max_size / max_pins      = %d / %d\n",
                      cache_ptr->prefix,
                      (int)(cache_ptr->max_size[i]),
		      (int)(cache_ptr->max_pins[i]));


#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */

        } /* end for */
    } /* end if */

    HDfprintf(stdout, "\n");

#endif /* H5C_COLLECT_CACHE_STATS */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_stats() */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_stats__reset
 *
 * Purpose:     Reset the stats fields to their initial values.
 *
 * Return:      void
 *
 * Programmer:  John Mainzer, 4/28/04
 *
 *-------------------------------------------------------------------------
 */
void
#ifndef NDEBUG
H5C_stats__reset(H5C_t * cache_ptr)
#else /* NDEBUG */
#if H5C_COLLECT_CACHE_STATS
H5C_stats__reset(H5C_t * cache_ptr)
#else /* H5C_COLLECT_CACHE_STATS */
H5C_stats__reset(H5C_t H5_ATTR_UNUSED * cache_ptr)
#endif /* H5C_COLLECT_CACHE_STATS */
#endif /* NDEBUG */
{
#if H5C_COLLECT_CACHE_STATS
    int i;
#endif /* H5C_COLLECT_CACHE_STATS */

    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

#if H5C_COLLECT_CACHE_STATS
    for(i = 0; i <= cache_ptr->max_type_id; i++) {
        cache_ptr->hits[i]			= 0;
        cache_ptr->misses[i]			= 0;
        cache_ptr->write_protects[i]		= 0;
        cache_ptr->read_protects[i]		= 0;
        cache_ptr->max_read_protects[i]		= 0;
        cache_ptr->insertions[i]		= 0;
        cache_ptr->pinned_insertions[i]		= 0;
        cache_ptr->clears[i]			= 0;
        cache_ptr->flushes[i]			= 0;
        cache_ptr->evictions[i]	 		= 0;
        cache_ptr->take_ownerships[i] 		= 0;
        cache_ptr->moves[i]	 		= 0;
        cache_ptr->entry_flush_moves[i]		= 0;
        cache_ptr->cache_flush_moves[i]		= 0;
        cache_ptr->pins[i]	 		= 0;
        cache_ptr->unpins[i]	 		= 0;
        cache_ptr->dirty_pins[i]	 	= 0;
        cache_ptr->pinned_flushes[i]	 	= 0;
        cache_ptr->pinned_clears[i]	 	= 0;
        cache_ptr->size_increases[i] 		= 0;
        cache_ptr->size_decreases[i] 		= 0;
	cache_ptr->entry_flush_size_changes[i]	= 0;
	cache_ptr->cache_flush_size_changes[i]	= 0;
    } /* end for */

    cache_ptr->total_ht_insertions		= 0;
    cache_ptr->total_ht_deletions		= 0;
    cache_ptr->successful_ht_searches		= 0;
    cache_ptr->total_successful_ht_search_depth	= 0;
    cache_ptr->failed_ht_searches		= 0;
    cache_ptr->total_failed_ht_search_depth	= 0;

    cache_ptr->max_index_len			= 0;
    cache_ptr->max_index_size			= (size_t)0;
    cache_ptr->max_clean_index_size		= (size_t)0;
    cache_ptr->max_dirty_index_size		= (size_t)0;

    cache_ptr->max_slist_len			= 0;
    cache_ptr->max_slist_size			= (size_t)0;

    cache_ptr->max_pl_len			= 0;
    cache_ptr->max_pl_size			= (size_t)0;

    cache_ptr->max_pel_len			= 0;
    cache_ptr->max_pel_size			= (size_t)0;

    cache_ptr->calls_to_msic                          = 0;
    cache_ptr->total_entries_skipped_in_msic          = 0;
    cache_ptr->total_dirty_pf_entries_skipped_in_msic = 0;
    cache_ptr->total_entries_scanned_in_msic          = 0;
    cache_ptr->max_entries_skipped_in_msic            = 0;
    cache_ptr->max_dirty_pf_entries_skipped_in_msic   = 0;
    cache_ptr->max_entries_scanned_in_msic            = 0;
    cache_ptr->entries_scanned_to_make_space          = 0;

    cache_ptr->slist_scan_restarts		= 0;
    cache_ptr->LRU_scan_restarts		= 0;
    cache_ptr->index_scan_restarts              = 0;

    cache_ptr->images_created           = 0;
    cache_ptr->images_read              = 0;
    cache_ptr->images_loaded            = 0;
    cache_ptr->last_image_size          = (hsize_t)0;

    cache_ptr->prefetches               = 0;
    cache_ptr->dirty_prefetches			= 0;
    cache_ptr->prefetch_hits			= 0;

#if H5C_COLLECT_CACHE_ENTRY_STATS
    for(i = 0; i <= cache_ptr->max_type_id; i++) {
        cache_ptr->max_accesses[i]		= 0;
        cache_ptr->min_accesses[i]		= 1000000;
        cache_ptr->max_clears[i]		= 0;
        cache_ptr->max_flushes[i]		= 0;
        cache_ptr->max_size[i]			= (size_t)0;
        cache_ptr->max_pins[i]			= 0;
    } /* end for */

#endif /* H5C_COLLECT_CACHE_ENTRY_STATS */
#endif /* H5C_COLLECT_CACHE_STATS */

    return;
} /* H5C_stats__reset() */

extern void
H5C__dump_entry(H5C_t *cache_ptr, const H5C_cache_entry_t *entry_ptr,
    hbool_t dump_parents, const char *prefix, int indent);

static void
H5C__dump_parents(H5C_t *cache_ptr, const H5C_cache_entry_t *entry_ptr, const char *prefix, int indent)
{
    unsigned u;

    for(u = 0; u < entry_ptr->flush_dep_nparents; u++)
        H5C__dump_entry(cache_ptr, entry_ptr->flush_dep_parent[u], TRUE, prefix, indent + 2);
}

typedef struct H5C__dump_child_ctx_t {
    H5C_t *cache_ptr;
    const H5C_cache_entry_t *parent;
    hbool_t dump_parents;
    const char *prefix;
    int indent;
} H5C__dump_child_ctx_t;

static int
H5C__dump_children_cb(H5C_cache_entry_t *entry_ptr, void *_ctx)
{
    H5C__dump_child_ctx_t *ctx = (H5C__dump_child_ctx_t *)_ctx;

    if(entry_ptr->tag_info->tag != entry_ptr->addr) {
        unsigned u;

        HDassert(entry_ptr->flush_dep_nparents);
        for(u = 0; u < entry_ptr->flush_dep_nparents; u++)
            if(ctx->parent == entry_ptr->flush_dep_parent[u])
                H5C__dump_entry(ctx->cache_ptr, entry_ptr, ctx->dump_parents, ctx->prefix, ctx->indent + 2);
    } /* end if */

    return(H5_ITER_CONT);
} /* end H5C__dump_children_cb() */

static void
H5C__dump_children(H5C_t *cache_ptr, const H5C_cache_entry_t *entry_ptr,
    hbool_t dump_parents, const char *prefix, int indent)
{
    H5C__dump_child_ctx_t ctx;

    HDassert(entry_ptr->tag_info);

    ctx.cache_ptr = cache_ptr;
    ctx.parent = entry_ptr;
    ctx.dump_parents = dump_parents;
    ctx.prefix = prefix;
    ctx.indent = indent;
    H5C__iter_tagged_entries(cache_ptr, entry_ptr->tag_info->tag, FALSE, H5C__dump_children_cb, &ctx);
} /* end H5C__dump_children() */

void
H5C__dump_entry(H5C_t *cache_ptr, const H5C_cache_entry_t *entry_ptr,
    hbool_t dump_parents, const char *prefix, int indent)
{
    HDassert(cache_ptr);
    HDassert(entry_ptr);

    HDfprintf(stderr, "%*s%s: entry_ptr = (%a, '%s', %a, %t, %u, %u/%u)\n", indent, "", prefix, entry_ptr->addr, entry_ptr->type->name, entry_ptr->tag_info ? entry_ptr->tag_info->tag : HADDR_UNDEF, entry_ptr->is_dirty, entry_ptr->flush_dep_nparents, entry_ptr->flush_dep_nchildren, entry_ptr->flush_dep_ndirty_children);
    if(dump_parents && entry_ptr->flush_dep_nparents)
        H5C__dump_parents(cache_ptr, entry_ptr, "Parent", indent);
    if(entry_ptr->flush_dep_nchildren)
        H5C__dump_children(cache_ptr, entry_ptr, FALSE, "Child", indent);
} /* end H5C__dump_entry() */


/*-------------------------------------------------------------------------
 * Function:    H5C_flush_dependency_exists()
 *
 * Purpose:	Test to see if a flush dependency relationship exists 
 *          between the supplied parent and child.  Both parties 
 *          are indicated by addresses so as to avoid the necessity
 *          of protect / unprotect calls prior to this call. 
 *
 *          If either the parent or the child is not in the metadata 
 *          cache, the function sets *fd_exists_ptr to FALSE.
 *
 *          If both are in the cache, the childs list of parents is 
 *          searched for the proposed parent.  If the proposed parent
 *          is found in the childs parent list, the function sets
 *          *fd_exists_ptr to TRUE.  In all other non-error cases, 
 *          the function sets *fd_exists_ptr FALSE.
 *
 * Return:      SUCCEED on success/FAIL on failure.  Note that 
 *              *fd_exists_ptr is undefined on failure.
 *
 * Programmer:  John Mainzer
 *              9/28/16
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
herr_t
H5C_flush_dependency_exists(H5C_t *cache_ptr, haddr_t parent_addr, haddr_t child_addr,
    hbool_t *fd_exists_ptr)
{
    hbool_t             fd_exists = FALSE;  /* whether flush dependency exists */
    H5C_cache_entry_t *	parent_ptr;         /* Ptr to parent entry */
    H5C_cache_entry_t *	child_ptr;          /* Ptr to child entry */
    hbool_t             ret_value = FALSE;  /* Return value */

    FUNC_ENTER_NOAPI(NULL)

    /* Sanity checks */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(H5F_addr_defined(parent_addr));
    HDassert(H5F_addr_defined(child_addr));
    HDassert(fd_exists_ptr);

    H5C__SEARCH_INDEX(cache_ptr, parent_addr, parent_ptr, FAIL)
    H5C__SEARCH_INDEX(cache_ptr, child_addr, child_ptr, FAIL)

    if(parent_ptr && child_ptr) {
        HDassert(parent_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
        HDassert(child_ptr->magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);

        if(child_ptr->flush_dep_nparents > 0) {
            unsigned u;         /* Local index variable */

            HDassert(child_ptr->flush_dep_parent);
            HDassert(child_ptr->flush_dep_parent_nalloc >= child_ptr->flush_dep_nparents);

            for(u = 0; u < child_ptr->flush_dep_nparents; u++) {
                if(child_ptr->flush_dep_parent[u] == parent_ptr) {
                    fd_exists = TRUE;
                    HDassert(parent_ptr->flush_dep_nchildren > 0);
                    break;
                } /* end if */
            } /* end for */
        } /* end if */
    } /* end if */

    *fd_exists_ptr = fd_exists;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_flush_dependency_exists() */
#endif /* NDEBUG */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_validate_index_list
 *
 * Purpose:     Debugging function that scans the index list for errors.
 *
 *		If an error is detected, the function generates a
 *		diagnostic and returns FAIL.  If no error is detected,
 *		the function returns SUCCEED.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  John Mainzer, 9/16/16
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
herr_t
H5C_validate_index_list(H5C_t *cache_ptr)
{
    H5C_cache_entry_t *	entry_ptr = NULL;
    uint32_t            len = 0;
    int32_t		index_ring_len[H5C_RING_NTYPES];
    size_t              size = 0;
    size_t              clean_size = 0;
    size_t              dirty_size = 0;
    size_t		index_ring_size[H5C_RING_NTYPES];
    size_t		clean_index_ring_size[H5C_RING_NTYPES];
    size_t		dirty_index_ring_size[H5C_RING_NTYPES];
    int			i;
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* Sanity checks */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    for(i = 0; i < H5C_RING_NTYPES; i++) {
	index_ring_len[i] = 0;
	index_ring_size[i] = 0;
	clean_index_ring_size[i] = 0;
	dirty_index_ring_size[i] = 0;
    } /* end if */

    if(((cache_ptr->il_head == NULL) || (cache_ptr->il_tail == NULL))
            && (cache_ptr->il_head != cache_ptr->il_tail))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Index list pointer validation failed")

    if((cache_ptr->index_len == 1) && ((cache_ptr->il_head != cache_ptr->il_tail)
            || (cache_ptr->il_head == NULL) || (cache_ptr->il_head->size != cache_ptr->index_size)))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Index list pointer sanity checks failed")

    if((cache_ptr->index_len >= 1)
            && ((cache_ptr->il_head == NULL)
                || (cache_ptr->il_head->il_prev != NULL)
                || (cache_ptr->il_tail == NULL)
                || (cache_ptr->il_tail->il_next != NULL)))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Index list length sanity checks failed")

    entry_ptr = cache_ptr->il_head;
    while(entry_ptr != NULL) {
        if((entry_ptr != cache_ptr->il_head)
                && ((entry_ptr->il_prev == NULL) || (entry_ptr->il_prev->il_next != entry_ptr)))
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Index list pointers for entry are invalid")

        if((entry_ptr != cache_ptr->il_tail)
                && ((entry_ptr->il_next == NULL) || (entry_ptr->il_next->il_prev != entry_ptr)))
            HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Index list pointers for entry are invalid")

	HDassert(entry_ptr->ring > 0);
	HDassert(entry_ptr->ring < H5C_RING_NTYPES);

        len++;
	index_ring_len[entry_ptr->ring] += 1;

        size += entry_ptr->size;
        index_ring_size[entry_ptr->ring] += entry_ptr->size;

	if(entry_ptr->is_dirty) {
	    dirty_size += entry_ptr->size;
	    dirty_index_ring_size[entry_ptr->ring] += entry_ptr->size;
	} /* end if */
        else {
	    clean_size += entry_ptr->size;
	    clean_index_ring_size[entry_ptr->ring] += entry_ptr->size;
	} /* end else */

        entry_ptr = entry_ptr->il_next;
    } /* end while */

    if((cache_ptr->index_len != len) || (cache_ptr->il_len != len)
            || (cache_ptr->index_size != size) || (cache_ptr->il_size != size)
            || (cache_ptr->clean_index_size != clean_size)
            || (cache_ptr->dirty_index_size != dirty_size)
            || (clean_size + dirty_size != size))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Index, clean and dirty sizes for cache are invalid")

    size = 0;
    clean_size = 0;
    dirty_size = 0;
    for(i = 0; i < H5C_RING_NTYPES; i++) {
	size += clean_index_ring_size[i] + dirty_index_ring_size[i];
	clean_size += clean_index_ring_size[i];
	dirty_size += dirty_index_ring_size[i];
    } /* end for */

    if((cache_ptr->index_size != size)
            || (cache_ptr->clean_index_size != clean_size)
            || (cache_ptr->dirty_index_size != dirty_size))
        HGOTO_ERROR(H5E_CACHE, H5E_SYSTEM, FAIL, "Index, clean and dirty sizes for cache are invalid")

done:
    if(ret_value != SUCCEED)
        HDassert(0);

    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_validate_index_list() */
#endif /* NDEBUG */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_get_entry_ptr_from_addr()
 *
 * Purpose:     Debugging function that attempts to look up an entry in the 
 *              cache by its file address, and if found, returns a pointer 
 *              to the entry in *entry_ptr_ptr.  If the entry is not in the 
 *              cache, *entry_ptr_ptr is set to NULL.
 *
 *              WARNING: This call should be used only in debugging  
 *                       routines, and it should be avoided when 
 *                       possible.
 *
 *                       Further, if we ever multi-thread the cache, 
 *                       this routine will have to be either discarded 
 *                       or heavily re-worked.
 *
 *                       Finally, keep in mind that the entry whose 
 *                       pointer is obtained in this fashion may not 
 *                       be in a stable state.  
 *
 *              Note that this function is only defined if NDEBUG
 *              is not defined.
 *
 *              As heavy use of this function is almost certainly a 
 *              bad idea, the metadata cache tracks the number of 
 *              successful calls to this function, and (if 
 *              H5C_DO_SANITY_CHECKS is defined) displays any 
 *              non-zero count on cache shutdown.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  John Mainzer, 5/30/14
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
herr_t
H5C_get_entry_ptr_from_addr(H5C_t *cache_ptr, haddr_t addr, void **entry_ptr_ptr)
{
    H5C_cache_entry_t * entry_ptr = NULL;
    herr_t		ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(H5F_addr_defined(addr));
    HDassert(entry_ptr_ptr);

    H5C__SEARCH_INDEX(cache_ptr, addr, entry_ptr, FAIL)

    if(entry_ptr == NULL)
        /* the entry doesn't exist in the cache -- report this
         * and quit.
         */
        *entry_ptr_ptr = NULL;
    else {
        *entry_ptr_ptr = entry_ptr;

	/* increment call counter */
	(cache_ptr->get_entry_ptr_from_addr_counter)++;
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_get_entry_ptr_from_addr() */
#endif /* NDEBUG */


/*-------------------------------------------------------------------------
 * Function:    H5C_get_serialization_in_progress
 *
 * Purpose:     Return the current value of 
 *              cache_ptr->serialization_in_progress.
 *
 * Return:      Current value of cache_ptr->serialization_in_progress.
 *
 * Programmer:  John Mainzer
 *		8/24/15
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
hbool_t
H5C_get_serialization_in_progress(const H5C_t *cache_ptr)
{
    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity check */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);

    FUNC_LEAVE_NOAPI(cache_ptr->serialization_in_progress)
} /* H5C_get_serialization_in_progress() */
#endif /* NDEBUG */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_cache_is_clean()
 *
 * Purpose:     Debugging function that verifies that all rings in the 
 *		metadata cache are clean from the outermost ring, inwards
 *		to the inner ring specified.
 *
 *		Returns TRUE if all specified rings are clean, and FALSE
 *		if not.  Throws an assertion failure on error.
 *
 * Return:      TRUE if the indicated ring(s) are clean, and FALSE otherwise.
 *
 * Programmer:  John Mainzer, 6/18/16
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
hbool_t
H5C_cache_is_clean(const H5C_t *cache_ptr, H5C_ring_t inner_ring)
{
    H5C_ring_t		ring = H5C_RING_USER;
    hbool_t             ret_value = TRUE;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* Sanity checks */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(inner_ring >= H5C_RING_USER);
    HDassert(inner_ring <= H5C_RING_SB);

    while(ring <= inner_ring) {
	if(cache_ptr->dirty_index_ring_size[ring] > 0)
            HGOTO_DONE(FALSE)

	ring++;
    } /* end while */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_cache_is_clean() */
#endif /* NDEBUG */


/*-------------------------------------------------------------------------
 *
 * Function:    H5C_verify_entry_type()
 *
 * Purpose:     Debugging function that attempts to look up an entry in the 
 *		cache by its file address, and if found, test to see if its
 *		type field contains the expted value.
 *
 *		If the specified entry is in cache, *in_cache_ptr is set
 *		to TRUE, and *type_ok_ptr is set to TRUE or FALSE depending 
 *		on whether the entries type field matches the expected_type 
 *		parameter.
 *
 *		If the target entry is not in cache, *in_cache_ptr is 
 *		set to FALSE, and *type_ok_ptr is undefined.
 *
 *		Note that this function is only defined if NDEBUG
 *		is not defined.
 *
 * Return:      FAIL if error is detected, SUCCEED otherwise.
 *
 * Programmer:  John Mainzer, 5/30/14
 *
 *-------------------------------------------------------------------------
 */
#ifndef NDEBUG
herr_t
H5C_verify_entry_type(H5C_t *cache_ptr, haddr_t addr,
    const H5C_class_t *expected_type, hbool_t *in_cache_ptr,
    hbool_t *type_ok_ptr)
{
    H5C_cache_entry_t * entry_ptr = NULL;
    herr_t              ret_value = SUCCEED;      /* Return value */

    FUNC_ENTER_NOAPI(FAIL)

    /* Sanity checks */
    HDassert(cache_ptr);
    HDassert(cache_ptr->magic == H5C__H5C_T_MAGIC);
    HDassert(H5F_addr_defined(addr));
    HDassert(expected_type);
    HDassert(in_cache_ptr);
    HDassert(type_ok_ptr);

    H5C__SEARCH_INDEX(cache_ptr, addr, entry_ptr, FAIL)

    if(entry_ptr == NULL)
        /* the entry doesn't exist in the cache -- report this
         * and quit.
         */
        *in_cache_ptr = FALSE;
    else {
        *in_cache_ptr = TRUE;

	if(entry_ptr->prefetched)
	    *type_ok_ptr = (expected_type->id == entry_ptr->prefetch_type_id);
	else
	    *type_ok_ptr = (expected_type == entry_ptr->type);
    } /* end else */

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5C_verify_entry_type() */
#endif /* NDEBUG */


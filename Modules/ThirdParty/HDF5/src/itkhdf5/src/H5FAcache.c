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
 * Created:     H5FAcache.c
 *		Jul  2 2009
 *		Quincey Koziol
 *
 * Purpose:     Implement fixed array metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/**********************/
/* Module Declaration */
/**********************/

#include "H5FAmodule.h"         /* This source code file is part of the H5FA module */


/***********************/
/* Other Packages Used */
/***********************/


/***********/
/* Headers */
/***********/
#include "H5private.h"		/* Generic Functions			*/
#include "H5Eprivate.h"		/* Error handling		  	*/
#include "H5FApkg.h"		/* Fixed Arrays				*/
#include "H5MFprivate.h"	/* File memory management		*/
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5VMprivate.h"	/* Vectors and arrays 			*/
#include "H5WBprivate.h"        /* Wrapped Buffers                      */


/****************/
/* Local Macros */
/****************/

/* Fixed Array format version #'s */
#define H5FA_HDR_VERSION        0               /* Header */
#define H5FA_DBLOCK_VERSION     0               /* Data block */


/******************/
/* Local Typedefs */
/******************/


/********************/
/* Package Typedefs */
/********************/


/********************/
/* Local Prototypes */
/********************/

/* Metadata cache (H5AC) callbacks */
static herr_t H5FA__cache_hdr_get_initial_load_size(void *udata, size_t *image_len);
static htri_t H5FA__cache_hdr_verify_chksum(const void *image_ptr, size_t len, void *udata_ptr);
static void *H5FA__cache_hdr_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty);
static herr_t H5FA__cache_hdr_image_len(const void *thing, size_t *image_len);
static herr_t H5FA__cache_hdr_serialize(const H5F_t *f, void *image, size_t len,
    void *thing);
static herr_t H5FA__cache_hdr_notify(H5AC_notify_action_t action, void *thing);
static herr_t H5FA__cache_hdr_free_icr(void *thing);

static herr_t H5FA__cache_dblock_get_initial_load_size(void *udata, size_t *image_len);
static htri_t H5FA__cache_dblock_verify_chksum(const void *image_ptr, size_t len, void *udata_ptr);
static void *H5FA__cache_dblock_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty);
static herr_t H5FA__cache_dblock_image_len(const void *thing, size_t *image_len);
static herr_t H5FA__cache_dblock_serialize(const H5F_t *f, void *image, size_t len,
    void *thing);
static herr_t H5FA__cache_dblock_notify(H5AC_notify_action_t action, void *thing);
static herr_t H5FA__cache_dblock_free_icr(void *thing);
static herr_t H5FA__cache_dblock_fsf_size(const void *thing, hsize_t *fsf_size);

static herr_t H5FA__cache_dblk_page_get_initial_load_size(void *udata, size_t *image_len);
static htri_t H5FA__cache_dblk_page_verify_chksum(const void *image_ptr, size_t len, void *udata_ptr);
static void *H5FA__cache_dblk_page_deserialize(const void *image, size_t len,
    void *udata, hbool_t *dirty);
static herr_t H5FA__cache_dblk_page_image_len(const void *thing, size_t *image_len);
static herr_t H5FA__cache_dblk_page_serialize(const H5F_t *f, void *image, size_t len,
    void *thing);
static herr_t H5FA__cache_dblk_page_notify(H5AC_notify_action_t action, void *thing);
static herr_t H5FA__cache_dblk_page_free_icr(void *thing);


/*********************/
/* Package Variables */
/*********************/

/* H5FA header inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FARRAY_HDR[1] = {{
    H5AC_FARRAY_HDR_ID,                 /* Metadata client ID */
    "Fixed-array Header",               /* Metadata client name (for debugging) */
    H5FD_MEM_FARRAY_HDR,                /* File space memory type for client */
    H5AC__CLASS_NO_FLAGS_SET,           /* Client class behavior flags */
    H5FA__cache_hdr_get_initial_load_size,      /* 'get_initial_load_size' callback */
    NULL,				/* 'get_final_load_size' callback */
    H5FA__cache_hdr_verify_chksum,	/* 'verify_chksum' callback */
    H5FA__cache_hdr_deserialize,        /* 'deserialize' callback */
    H5FA__cache_hdr_image_len,          /* 'image_len' callback */
    NULL,                               /* 'pre_serialize' callback */
    H5FA__cache_hdr_serialize,          /* 'serialize' callback */
    H5FA__cache_hdr_notify,             /* 'notify' callback */
    H5FA__cache_hdr_free_icr,           /* 'free_icr' callback */
    NULL,                               /* 'fsf_size' callback */
}};

/* H5FA data block inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FARRAY_DBLOCK[1] = {{
    H5AC_FARRAY_DBLOCK_ID,              /* Metadata client ID */
    "Fixed Array Data Block",           /* Metadata client name (for debugging) */
    H5FD_MEM_FARRAY_DBLOCK,             /* File space memory type for client */
    H5AC__CLASS_NO_FLAGS_SET,           /* Client class behavior flags */
    H5FA__cache_dblock_get_initial_load_size,   /* 'get_initial_load_size' callback */
    NULL,				/* 'get_final_load_size' callback */
    H5FA__cache_dblock_verify_chksum,	/* 'verify_chksum' callback */
    H5FA__cache_dblock_deserialize,     /* 'deserialize' callback */
    H5FA__cache_dblock_image_len,       /* 'image_len' callback */
    NULL,                               /* 'pre_serialize' callback */
    H5FA__cache_dblock_serialize,       /* 'serialize' callback */
    H5FA__cache_dblock_notify,		/* 'notify' callback */
    H5FA__cache_dblock_free_icr,        /* 'free_icr' callback */
    H5FA__cache_dblock_fsf_size,        /* 'fsf_size' callback */
}};

/* H5FA data block page inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_FARRAY_DBLK_PAGE[1] = {{
    H5AC_FARRAY_DBLK_PAGE_ID,           /* Metadata client ID */
    "Fixed Array Data Block Page",      /* Metadata client name (for debugging) */
    H5FD_MEM_FARRAY_DBLK_PAGE,          /* File space memory type for client */
    H5AC__CLASS_NO_FLAGS_SET,           /* Client class behavior flags */
    H5FA__cache_dblk_page_get_initial_load_size, /* 'get_initial_load_size' callback */
    NULL,				/* 'get_final_load_size' callback */
    H5FA__cache_dblk_page_verify_chksum, /* 'verify_chksum' callback */
    H5FA__cache_dblk_page_deserialize,  /* 'deserialize' callback */
    H5FA__cache_dblk_page_image_len,    /* 'image_len' callback */
    NULL,                               /* 'pre_serialize' callback */
    H5FA__cache_dblk_page_serialize,    /* 'serialize' callback */
    H5FA__cache_dblk_page_notify,	/* 'notify' callback */
    H5FA__cache_dblk_page_free_icr,     /* 'free_icr' callback */
    NULL,                               /* 'fsf_size' callback */
}};


/*****************************/
/* Library Private Variables */
/*****************************/


/*******************/
/* Local Variables */
/*******************/



/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_hdr_get_initial_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              July 31, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5FA__cache_hdr_get_initial_load_size(void *_udata, size_t *image_len))

    /* Local variables */
    H5FA_hdr_cache_ud_t *udata = (H5FA_hdr_cache_ud_t *)_udata; /* User data for callback */

    /* Check arguments */
    HDassert(udata);
    HDassert(udata->f);
    HDassert(image_len);

    /* Set the image length size */
    *image_len = (size_t)H5FA_HEADER_SIZE_FILE(udata->f);

END_FUNC(STATIC)   /* end H5FA__cache_hdr_get_initial_load_size() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_hdr_verify_chksum
 *
 * Purpose:     Verify the computed checksum of the data structure is the
 *              same as the stored chksum.
 *
 * Return:      Success:        TRUE/FALSE
 *              Failure:        Negative
 *
 * Programmer:	Vailin Choi; Aug 2015
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
htri_t, TRUE, -,
H5FA__cache_hdr_verify_chksum(const void *_image, size_t len, void H5_ATTR_UNUSED *_udata))

    /* Local variables */
    const uint8_t *image = (const uint8_t *)_image;       /* Pointer into raw data buffer */
    uint32_t stored_chksum;     /* Stored metadata checksum value */
    uint32_t computed_chksum;   /* Computed metadata checksum value */

    /* Check arguments */
    HDassert(image);

    /* Get stored and computed checksums */
    H5F_get_checksums(image, len, &stored_chksum, &computed_chksum);

    if(stored_chksum != computed_chksum)
	ret_value = FALSE;

END_FUNC(STATIC) 	/* end H5FA__cache_hdr_verify_chksum() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_hdr_deserialize
 *
 * Purpose:	Loads a data structure from the disk.
 *
 * Return:	Success:	Pointer to a new Fixed array
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              August 12, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
void *, NULL, NULL,
H5FA__cache_hdr_deserialize(const void *_image, size_t H5_ATTR_NDEBUG_UNUSED len,
    void *_udata, hbool_t H5_ATTR_UNUSED *dirty))

    /* Local variables */
    H5FA_cls_id_t       id;		/* ID of fixed array class, as found in file */
    H5FA_hdr_t		*hdr = NULL;    /* Fixed array info */
    H5FA_hdr_cache_ud_t *udata = (H5FA_hdr_cache_ud_t *)_udata;
    const uint8_t	*image = (const uint8_t *)_image;       /* Pointer into raw data buffer */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */

    /* Check arguments */
    HDassert(udata);
    HDassert(udata->f);
    HDassert(H5F_addr_defined(udata->addr));

    /* Allocate space for the fixed array data structure */
    if(NULL == (hdr = H5FA__hdr_alloc(udata->f)))
        H5E_THROW(H5E_CANTALLOC, "memory allocation failed for fixed array shared header")

    /* Set the fixed array header's address */
    hdr->addr = udata->addr;

    /* Magic number */
    if(HDmemcmp(image, H5FA_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC))
        H5E_THROW(H5E_BADVALUE, "wrong fixed array header signature")
    image += H5_SIZEOF_MAGIC;

    /* Version */
    if(*image++ != H5FA_HDR_VERSION)
        H5E_THROW(H5E_VERSION, "wrong fixed array header version")

    /* Fixed array class */
    id = (H5FA_cls_id_t)*image++;
    if(id >= H5FA_NUM_CLS_ID)
        H5E_THROW(H5E_BADTYPE, "incorrect fixed array class")
    hdr->cparam.cls = H5FA_client_class_g[id];

    /* General array creation/configuration information */
    hdr->cparam.raw_elmt_size = *image++;             /* Element size in file (in bytes) */
    hdr->cparam.max_dblk_page_nelmts_bits = *image++; /* Log2(Max. # of elements in data block page) -
						         i.e. # of bits needed to store max. # of
						         elements in data block page. */

    /* Array statistics */
    H5F_DECODE_LENGTH(udata->f, image, hdr->cparam.nelmts);	/* Number of elements */

    /* Internal information */
    H5F_addr_decode(udata->f, &image, &hdr->dblk_addr); 		/* Address of index block */

    /* Check for data block */
    if(H5F_addr_defined(hdr->dblk_addr)) {
        H5FA_dblock_t  dblock;  	/* Fake data block for computing size */
        size_t	dblk_page_nelmts;	/* # of elements per data block page */

        /* Set up fake data block for computing size on disk */
        dblock.hdr = hdr;
        dblock.dblk_page_init_size = 0;
        dblock.npages = 0;
        dblk_page_nelmts = (size_t)1 << hdr->cparam.max_dblk_page_nelmts_bits;
        if(hdr->cparam.nelmts > dblk_page_nelmts) {
            dblock.npages = (size_t)(((hdr->cparam.nelmts + dblk_page_nelmts) - 1) / dblk_page_nelmts);
            dblock.dblk_page_init_size = (dblock.npages + 7) / 8;
        } /* end if */

        /* Compute Fixed Array data block size for hdr statistics */
        hdr->stats.dblk_size = (size_t)H5FA_DBLOCK_SIZE(&dblock);
    } /* end if */

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(image - (const uint8_t *)_image) == (len - H5FA_SIZEOF_CHKSUM));

    /* checksum verification already done in verify_chksum cb */

    /* Metadata checksum */
    UINT32DECODE(image, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(image - (const uint8_t *)_image) == len);

    /* Finish initializing fixed array header */
    if(H5FA__hdr_init(hdr, udata->ctx_udata) < 0)
        H5E_THROW(H5E_CANTINIT, "initialization failed for fixed array header")
    HDassert(hdr->size == len);

    /* Set return value */
    ret_value = hdr;

CATCH

    /* Release resources */
    if(!ret_value)
        if(hdr && H5FA__hdr_dest(hdr) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy fixed array header")

END_FUNC(STATIC)   /* end H5FA__cache_hdr_deserialize() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_hdr_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 12, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5FA__cache_hdr_image_len(const void *_thing, size_t *image_len))

    /* Local variables */
    const H5FA_hdr_t *hdr = (const H5FA_hdr_t *)_thing;      /* Pointer to the object */

    /* Check arguments */
    HDassert(hdr);
    HDassert(image_len);

    /* Set the image length size */
    *image_len = hdr->size;

END_FUNC(STATIC)   /* end H5FA__cache_hdr_image_len() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_hdr_serialize
 *
 * Purpose:	Flushes a dirty object to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              August 12, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5FA__cache_hdr_serialize(const H5F_t *f, void *_image, size_t H5_ATTR_UNUSED len,
    void *_thing))

    /* Local variables */
    H5FA_hdr_t *hdr = (H5FA_hdr_t *)_thing;     /* Pointer to the fixed array header */
    uint8_t *image = (uint8_t *)_image;         /* Pointer into raw data buffer */
    uint32_t metadata_chksum;   /* Computed metadata checksum value */

    /* check arguments */
    HDassert(f);
    HDassert(image);
    HDassert(hdr);

    /* Magic number */
    H5MM_memcpy(image, H5FA_HDR_MAGIC, (size_t)H5_SIZEOF_MAGIC);
    image += H5_SIZEOF_MAGIC;

    /* Version # */
    *image++ = H5FA_HDR_VERSION;

    /* Fixed array type */
    HDassert(hdr->cparam.cls->id <= 255);
    *image++ = (uint8_t)hdr->cparam.cls->id;

    /* General array creation/configuration information */
    *image++ = hdr->cparam.raw_elmt_size;          /* Element size in file (in bytes) */
    *image++ = hdr->cparam.max_dblk_page_nelmts_bits;  /* Log2(Max. # of elements in data block page) - i.e. # of bits needed to store max. # of elements in data block page */

    /* Array statistics */
    H5F_ENCODE_LENGTH(f, image, hdr->stats.nelmts);       /* Number of elements for the fixed array */

    /* Internal information */
    H5F_addr_encode(f, &image, hdr->dblk_addr);  /* Address of fixed array data block */

    /* Compute metadata checksum */
    metadata_chksum = H5_checksum_metadata(_image, (size_t)(image - (uint8_t *)_image), 0);

    /* Metadata checksum */
    UINT32ENCODE(image, metadata_chksum);

    /* Sanity check */
    HDassert((size_t)(image - (uint8_t *)_image) == len);

END_FUNC(STATIC)   /* end H5FA__cache_hdr_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_hdr_notify
 *
 * Purpose:	Handle cache action notifications
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Dana Robinson
 *              December 2015
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_hdr_notify(H5AC_notify_action_t action, void *_thing))

    /* Local variables */
    H5FA_hdr_t *hdr = (H5FA_hdr_t *)_thing;      /* Pointer to the object */

    /* Sanity check */
    HDassert(hdr);

    /* Check if the file was opened with SWMR-write access */
    if(hdr->swmr_write) {
        /* Determine which action to take */
        switch(action) {
            case H5AC_NOTIFY_ACTION_AFTER_INSERT:
            case H5AC_NOTIFY_ACTION_AFTER_LOAD:
            case H5AC_NOTIFY_ACTION_AFTER_FLUSH:
            case H5AC_NOTIFY_ACTION_ENTRY_DIRTIED:
            case H5AC_NOTIFY_ACTION_ENTRY_CLEANED:
            case H5AC_NOTIFY_ACTION_CHILD_DIRTIED:
            case H5AC_NOTIFY_ACTION_CHILD_CLEANED:
            case H5AC_NOTIFY_ACTION_CHILD_UNSERIALIZED:
            case H5AC_NOTIFY_ACTION_CHILD_SERIALIZED:
                /* do nothing */
                break;

            case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
                /* If hdr->parent != NULL, hdr->parent is used to destroy
                 * the flush dependency before the header is evicted.
                 */
                if(hdr->parent) {
                    /* Sanity check */
                    HDassert(hdr->top_proxy);

		    /* Destroy flush dependency on object header proxy */
		    if(H5AC_proxy_entry_remove_child((H5AC_proxy_entry_t *)hdr->parent, (void *)hdr->top_proxy) < 0)
		        H5E_THROW(H5E_CANTUNDEPEND, "unable to destroy flush dependency between fixed array and proxy")
                    hdr->parent = NULL;
		} /* end if */

                /* Detach from 'top' proxy for fixed array */
                if(hdr->top_proxy) {
                    if(H5AC_proxy_entry_remove_child(hdr->top_proxy, hdr) < 0)
                        H5E_THROW(H5E_CANTUNDEPEND, "unable to destroy flush dependency between header and fixed array 'top' proxy")
                    /* Don't reset hdr->top_proxy here, it's destroyed when the header is freed -QAK */
                } /* end if */
                break;

            default:
#ifdef NDEBUG
                H5E_THROW(H5E_BADVALUE, "unknown action from metadata cache")
#else /* NDEBUG */
                HDassert(0 && "Unknown action?!?");
#endif /* NDEBUG */
        } /* end switch */
    } /* end if */
    else
        HDassert(NULL == hdr->parent);

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_hdr_notify() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_hdr_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *              August 12, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_hdr_free_icr(void *thing))

    /* Check arguments */
    HDassert(thing);

    /* Release the extensible array header */
    if(H5FA__hdr_dest((H5FA_hdr_t *)thing) < 0)
        H5E_THROW(H5E_CANTFREE, "can't free fixed array header")

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_hdr_free_icr() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblock_get_initial_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 12, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5FA__cache_dblock_get_initial_load_size(void *_udata, size_t *image_len))

    /* Local variables */
    H5FA_dblock_cache_ud_t *udata = (H5FA_dblock_cache_ud_t *)_udata;      /* User data */
    H5FA_dblock_t dblock;           			/* Fake data block for computing size */
    size_t dblk_page_nelmts;				/* # of elements per data block page */

    /* Check arguments */
    HDassert(udata);
    HDassert(udata->hdr);
    HDassert(image_len);

    /* Set up fake data block for computing size on disk */
    /* (Note: extracted from H5FA__dblock_alloc) */
    HDmemset(&dblock, 0, sizeof(dblock));

    /* Set up fake data block for computing size on disk
     *
     * need: dblock->hdr
     *       dblock->npages
     *       dblock->dblk_page_init_size
     */
    dblock.hdr = udata->hdr;
    dblk_page_nelmts = (size_t)1 << udata->hdr->cparam.max_dblk_page_nelmts_bits;
    if(udata->hdr->cparam.nelmts > dblk_page_nelmts) {
        dblock.npages = (size_t)(((udata->hdr->cparam.nelmts + dblk_page_nelmts) - 1) / dblk_page_nelmts);
        dblock.dblk_page_init_size = (dblock.npages + 7) / 8;
    } /* end if */

    /* Set the image length size */
    if(!dblock.npages)
        *image_len = (size_t)H5FA_DBLOCK_SIZE(&dblock);
    else
        *image_len = (size_t)H5FA_DBLOCK_PREFIX_SIZE(&dblock);

END_FUNC(STATIC)   /* end H5FA__cache_dblock_get_initial_load_size() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_dblock_verify_chksum
 *
 * Purpose:     Verify the computed checksum of the data structure is the
 *              same as the stored chksum.
 *
 * Return:      Success:        TRUE/FALSE
 *              Failure:        Negative
 *
 * Programmer:	Vailin Choi; Aug 2015
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
htri_t, TRUE, -,
H5FA__cache_dblock_verify_chksum(const void *_image, size_t len, void H5_ATTR_UNUSED *_udata))

    /* Local variables */
    const uint8_t *image = (const uint8_t *)_image;       /* Pointer into raw data buffer */
    uint32_t stored_chksum;     /* Stored metadata checksum value */
    uint32_t computed_chksum;   /* Computed metadata checksum value */

    /* Check arguments */
    HDassert(image);

    /* Get stored and computed checksums */
    H5F_get_checksums(image, len, &stored_chksum, &computed_chksum);

    if(stored_chksum != computed_chksum)
	ret_value = FALSE;

END_FUNC(STATIC) 	/* end H5FA__cache_dblock_verify_chksum() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_dblock_deserialize
 *
 * Purpose:	Loads a data structure from the disk.
 *
 * Return:	Success:	Pointer to a new B-tree.
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              August 14, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
void *, NULL, NULL,
H5FA__cache_dblock_deserialize(const void *_image, size_t H5_ATTR_NDEBUG_UNUSED len,
    void *_udata, hbool_t H5_ATTR_UNUSED *dirty))

    /* Local variables */
    H5FA_dblock_t  *dblock = NULL;  /* Data block info */
    H5FA_dblock_cache_ud_t *udata = (H5FA_dblock_cache_ud_t *)_udata; /* User data for loading data block */
    const uint8_t  *image = (const uint8_t *)_image;    /* Pointer into raw data buffer */
    uint32_t       stored_chksum;   /* Stored metadata checksum value */
    haddr_t        arr_addr;        /* Address of array header in the file */

    /* Sanity check */
    HDassert(udata);
    HDassert(udata->hdr);

    /* Allocate the fixed array data block */
    if(NULL == (dblock = H5FA__dblock_alloc(udata->hdr)))
        H5E_THROW(H5E_CANTALLOC, "memory allocation failed for fixed array data block")

    HDassert(((!dblock->npages) && (len == (size_t)H5FA_DBLOCK_SIZE(dblock)))
             || (len == (size_t)H5FA_DBLOCK_PREFIX_SIZE(dblock)));

    /* Set the fixed array data block's information */
    dblock->addr = udata->dblk_addr;

    /* Magic number */
    if(HDmemcmp(image, H5FA_DBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC))
        H5E_THROW(H5E_BADVALUE, "wrong fixed array data block signature")
    image += H5_SIZEOF_MAGIC;

    /* Version */
    if(*image++ != H5FA_DBLOCK_VERSION)
        H5E_THROW(H5E_VERSION, "wrong fixed array data block version")

    /* Fixed array type */
    if(*image++ != (uint8_t)udata->hdr->cparam.cls->id)
        H5E_THROW(H5E_BADTYPE, "incorrect fixed array class")

    /* Address of header for array that owns this block (just for file integrity checks) */
    H5F_addr_decode(udata->hdr->f, &image, &arr_addr);
    if(H5F_addr_ne(arr_addr, udata->hdr->addr))
        H5E_THROW(H5E_BADVALUE, "wrong fixed array header address")

    /* Page initialization flags */
    if(dblock->npages > 0) {
	H5MM_memcpy(dblock->dblk_page_init, image, dblock->dblk_page_init_size);
        image += dblock->dblk_page_init_size;
    } /* end if */

    /* Only decode elements if the data block is not paged */
    if(!dblock->npages) {
        /* Decode elements in data block */
        /* Convert from raw elements on disk into native elements in memory */
        if((udata->hdr->cparam.cls->decode)(image, dblock->elmts, (size_t)udata->hdr->cparam.nelmts, udata->hdr->cb_ctx) < 0)
            H5E_THROW(H5E_CANTDECODE, "can't decode fixed array data elements")
        image += (udata->hdr->cparam.nelmts * udata->hdr->cparam.raw_elmt_size);
    } /* end if */

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(image - (const uint8_t *)_image) == (len - H5FA_SIZEOF_CHKSUM));

    /* Set the data block's size */
    dblock->size = H5FA_DBLOCK_SIZE(dblock);

    /* checksum verification already done in verify_chksum cb */

    /* Metadata checksum */
    UINT32DECODE(image, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(image - (const uint8_t *)_image) == len);

    /* Set return value */
    ret_value = dblock;

CATCH

    /* Release resources */
    if(!ret_value)
        if(dblock && H5FA__dblock_dest(dblock) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy fixed array data block")

END_FUNC(STATIC)   /* end H5FA__cache_dblock_deserialize() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblock_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 14, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5FA__cache_dblock_image_len(const void *_thing, size_t *image_len))

    /* Local variables */
    const H5FA_dblock_t *dblock = (const H5FA_dblock_t *)_thing;      /* Pointer to the object */

    /* Check arguments */
    HDassert(dblock);
    HDassert(image_len);

    /* Set the image length size */
    if(!dblock->npages)
        *image_len = (size_t)dblock->size;
    else
        *image_len = H5FA_DBLOCK_PREFIX_SIZE(dblock);

END_FUNC(STATIC)   /* end H5FA__cache_dblock_image_len() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_dblock_serialize
 *
 * Purpose:	Flushes a dirty object to disk.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              August 14, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_dblock_serialize(const H5F_t *f, void *_image, size_t H5_ATTR_UNUSED len,
    void *_thing))

    /* Local variables */
    H5FA_dblock_t *dblock = (H5FA_dblock_t *)_thing;      /* Pointer to the object to serialize */
    uint8_t *image = (uint8_t *)_image;         /* Pointer into raw data buffer */
    uint32_t metadata_chksum; /* Computed metadata checksum value */

    /* Check arguments */
    HDassert(f);
    HDassert(image);
    HDassert(dblock);
    HDassert(dblock->hdr);

    /* Magic number */
    H5MM_memcpy(image, H5FA_DBLOCK_MAGIC, (size_t)H5_SIZEOF_MAGIC);
    image += H5_SIZEOF_MAGIC;

    /* Version # */
    *image++ = H5FA_DBLOCK_VERSION;

    /* Fixed array type */
    HDassert(dblock->hdr->cparam.cls->id <= 255);
    *image++ = (uint8_t)dblock->hdr->cparam.cls->id;

    /* Address of array header for array which owns this block */
    H5F_addr_encode(f, &image, dblock->hdr->addr);

    /* Page init flags */
    if(dblock->npages > 0) {
        /* Store the 'page init' bitmasks */
        H5MM_memcpy(image, dblock->dblk_page_init, dblock->dblk_page_init_size);
        image += dblock->dblk_page_init_size;
    } /* end if */

    /* Only encode elements if the data block is not paged */
    if(!dblock->npages) {
        /* Encode elements in data block */

        /* Convert from native elements in memory into raw elements on disk */
        H5_CHECK_OVERFLOW(dblock->hdr->cparam.nelmts, /* From: */hsize_t, /* To: */size_t);
        if((dblock->hdr->cparam.cls->encode)(image, dblock->elmts, (size_t)dblock->hdr->cparam.nelmts, dblock->hdr->cb_ctx) < 0)
            H5E_THROW(H5E_CANTENCODE, "can't encode fixed array data elements")
        image += (dblock->hdr->cparam.nelmts * dblock->hdr->cparam.raw_elmt_size);
    } /* end if */

    /* Compute metadata checksum */
    metadata_chksum = H5_checksum_metadata(_image, (size_t)(image - (uint8_t *)_image), 0);

    /* Metadata checksum */
    UINT32ENCODE(image, metadata_chksum);

    /* Sanity check */
    HDassert((size_t)(image - (uint8_t *)_image) == len);

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_dblock_serialize() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblock_notify
 *
 * Purpose:     Handle cache action notifications
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Dana Robinson
 *              Fall 2012
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_dblock_notify(H5AC_notify_action_t action, void *_thing))

    /* Local variables */
    H5FA_dblock_t *dblock = (H5FA_dblock_t *)_thing;

    /* Sanity check */
    HDassert(dblock);

    /* Check if the file was opened with SWMR-write access */
    if(dblock->hdr->swmr_write) {
        /* Determine which action to take */
        switch(action) {
            case H5AC_NOTIFY_ACTION_AFTER_INSERT:
	    case H5AC_NOTIFY_ACTION_AFTER_LOAD:
                /* Create flush dependency on parent */
                if(H5FA__create_flush_depend((H5AC_info_t *)dblock->hdr, (H5AC_info_t *)dblock) < 0)
                    H5E_THROW(H5E_CANTDEPEND, "unable to create flush dependency between data block and header, address = %llu", (unsigned long long)dblock->addr)
                break;

	    case H5AC_NOTIFY_ACTION_AFTER_FLUSH:
            case H5AC_NOTIFY_ACTION_ENTRY_DIRTIED:
            case H5AC_NOTIFY_ACTION_ENTRY_CLEANED:
            case H5AC_NOTIFY_ACTION_CHILD_DIRTIED:
            case H5AC_NOTIFY_ACTION_CHILD_CLEANED:
            case H5AC_NOTIFY_ACTION_CHILD_UNSERIALIZED:
            case H5AC_NOTIFY_ACTION_CHILD_SERIALIZED:
                /* do nothing */
                break;

            case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
		/* Destroy flush dependency on parent */
                if(H5FA__destroy_flush_depend((H5AC_info_t *)dblock->hdr, (H5AC_info_t *)dblock) < 0)
                    H5E_THROW(H5E_CANTUNDEPEND, "unable to destroy flush dependency")

                /* Detach from 'top' proxy for fixed array */
                if(dblock->top_proxy) {
                    if(H5AC_proxy_entry_remove_child(dblock->top_proxy, dblock) < 0)
                        H5E_THROW(H5E_CANTUNDEPEND, "unable to destroy flush dependency between data block and fixed array 'top' proxy")
                    dblock->top_proxy = NULL;
                } /* end if */
                break;

            default:
#ifdef NDEBUG
                H5E_THROW(H5E_BADVALUE, "unknown action from metadata cache")
#else /* NDEBUG */
                HDassert(0 && "Unknown action?!?");
#endif /* NDEBUG */
        } /* end switch */
    } /* end if */

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_dblock_notify() */



/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_dblock_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *              August 14, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_dblock_free_icr(void *_thing))

    H5FA_dblock_t *dblock = (H5FA_dblock_t *)_thing;    /* Pointer to the object */

    /* Check arguments */
    HDassert(dblock);

    /* Release the fixed array data block */
    if(H5FA__dblock_dest(dblock) < 0)
        H5E_THROW(H5E_CANTFREE, "can't free fixed array data block")

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_dblock_free_icr() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_dblock_fsf_size
 *
 * Purpose:	Tell the metadata cache the actual amount of file space
 *		to free when a dblock entry is destroyed with the free
 *		file space block set.
 *
 *		This function is needed when the data block is paged, as
 *		the datablock header and all its pages are allocted as a
 *		single contiguous chunk of file space, and must be
 *		deallocated the same way.
 *
 *		The size of the chunk of memory in which the dblock
 *		header and all its pages is stored in the size field,
 *		so we simply pass that value back to the cache.
 *
 *		If the datablock is not paged, then the size field of
 *		the cache_info contains the correct size.  However this
 *		value will be the same as the size field, so we return
 *		the contents of the size field to the cache in this case
 *		as well.
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	John Mainzer
 *              12/5/14
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5FA__cache_dblock_fsf_size(const void *_thing, hsize_t *fsf_size))

    const H5FA_dblock_t *dblock = (const H5FA_dblock_t *)_thing;    /* Pointer to the object */

    /* Check arguments */
    HDassert(dblock);
    HDassert(dblock->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(dblock->cache_info.type == H5AC_FARRAY_DBLOCK);
    HDassert(fsf_size);

    *fsf_size = dblock->size;

END_FUNC(STATIC)   /* end H5FA__cache_dblock_fsf_size() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblk_page_get_initial_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 14, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5FA__cache_dblk_page_get_initial_load_size(void *_udata, size_t *image_len))

    /* Local variables */
    H5FA_dblk_page_cache_ud_t *udata = (H5FA_dblk_page_cache_ud_t *)_udata;      /* User data */

    /* Check arguments */
    HDassert(udata);
    HDassert(udata->hdr);
    HDassert(udata->nelmts > 0);
    HDassert(image_len);

    /* Set the image length size */
    *image_len = (size_t)H5FA_DBLK_PAGE_SIZE(udata->hdr, udata->nelmts);

END_FUNC(STATIC)   /* end H5FA__cache_dblk_page_get_initial_load_size() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_dblk_page_verify_chksum
 *
 * Purpose:     Verify the computed checksum of the data structure is the
 *              same as the stored chksum.
 *
 * Return:      Success:        TRUE/FALSE
 *              Failure:        Negative
 *
 * Programmer:	Vailin Choi; Aug 2015
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
htri_t, TRUE, -,
H5FA__cache_dblk_page_verify_chksum(const void *_image, size_t len, void H5_ATTR_UNUSED *_udata))

    /* Local variables */
    const uint8_t *image = (const uint8_t *)_image;       /* Pointer into raw data buffer */
    uint32_t stored_chksum;     /* Stored metadata checksum value */
    uint32_t computed_chksum;   /* Computed metadata checksum value */

    /* Check arguments */
    HDassert(image);

    /* Get stored and computed checksums */
    H5F_get_checksums(image, len, &stored_chksum, &computed_chksum);

    if(stored_chksum != computed_chksum)
	ret_value = FALSE;

END_FUNC(STATIC) 	/* end H5FA__cache_dblk_page_verify_chksum() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_dblk_page_deserialize
 *
 * Purpose:	Loads a data structure from the disk.
 *
 * Return:	Success:	Pointer to a new B-tree.
 *		Failure:	NULL
 *
 * Programmer:	Quincey Koziol
 *              August 14, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
void *, NULL, NULL,
H5FA__cache_dblk_page_deserialize(const void *_image, size_t len,
    void *_udata, hbool_t H5_ATTR_UNUSED *dirty))

    /* Local variables */
    H5FA_dblk_page_t    *dblk_page = NULL; /* Data block page info */
    H5FA_dblk_page_cache_ud_t *udata = (H5FA_dblk_page_cache_ud_t *)_udata; /* User data for loading data block page */
    const uint8_t	*image = (const uint8_t *)_image;       /* Pointer into raw data buffer */
    uint32_t            stored_chksum;  /* Stored metadata checksum value */

    /* Sanity check */
    HDassert(udata);
    HDassert(udata->hdr);
    HDassert(udata->nelmts > 0);
    HDassert(H5F_addr_defined(udata->dblk_page_addr));

    /* Allocate the fixed array data block page */
    if(NULL == (dblk_page = H5FA__dblk_page_alloc(udata->hdr, udata->nelmts)))
        H5E_THROW(H5E_CANTALLOC, "memory allocation failed for fixed array data block page")

    /* Set the fixed array data block's information */
    dblk_page->addr = udata->dblk_page_addr;

    /* Internal information */

    /* Decode elements in data block page */
    /* Convert from raw elements on disk into native elements in memory */
    if((udata->hdr->cparam.cls->decode)(image, dblk_page->elmts, udata->nelmts, udata->hdr->cb_ctx) < 0)
        H5E_THROW(H5E_CANTDECODE, "can't decode fixed array data elements")
    image += (udata->nelmts * udata->hdr->cparam.raw_elmt_size);

    /* Sanity check */
    /* (allow for checksum not decoded yet) */
    HDassert((size_t)(image - (const uint8_t *)_image) == (len - H5FA_SIZEOF_CHKSUM));

    /* Set the data block page's size */
    dblk_page->size = len;

    /* checksum verification already done in verify_chksum cb */

    /* Metadata checksum */
    UINT32DECODE(image, stored_chksum);

    /* Sanity check */
    HDassert((size_t)(image - (const uint8_t *)_image) == dblk_page->size);

    /* Set return value */
    ret_value = dblk_page;

CATCH

    /* Release resources */
    if(!ret_value)
        if(dblk_page && H5FA__dblk_page_dest(dblk_page) < 0)
            H5E_THROW(H5E_CANTFREE, "unable to destroy fixed array data block page")

END_FUNC(STATIC)   /* end H5FA__cache_dblk_page_deserialize() */


/*-------------------------------------------------------------------------
 * Function:    H5FA__cache_dblk_page_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              August 14, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, NOERR,
herr_t, SUCCEED, -,
H5FA__cache_dblk_page_image_len(const void *_thing, size_t *image_len))

    /* Local variables */
    const H5FA_dblk_page_t *dblk_page = (const H5FA_dblk_page_t *)_thing;      /* Pointer to the object */

    /* Check arguments */
    HDassert(dblk_page);
    HDassert(image_len);

    /* Set the image length size */
    *image_len = dblk_page->size;

END_FUNC(STATIC)   /* end H5FA__cache_dblk_page_image_len() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_dblk_page_serialize
 *
 * Purpose:	Flushes a dirty object to disk.
 *
 * Return:	SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *              August 14, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_dblk_page_serialize(const H5F_t H5_ATTR_NDEBUG_UNUSED *f, void *_image, size_t H5_ATTR_UNUSED len,
    void *_thing))

    /* Local variables */
    H5FA_dblk_page_t *dblk_page = (H5FA_dblk_page_t *)_thing;      /* Pointer to the object to serialize */
    uint8_t *image = (uint8_t *)_image;         /* Pointer into raw data buffer */
    uint32_t metadata_chksum;   /* Computed metadata checksum value */

    /* Sanity check */
    HDassert(f);
    HDassert(image);
    HDassert(dblk_page);
    HDassert(dblk_page->hdr);

    /* Internal information */

    /* Encode elements in data block page */

    /* Convert from native elements in memory into raw elements on disk */
    if((dblk_page->hdr->cparam.cls->encode)(image, dblk_page->elmts, dblk_page->nelmts, dblk_page->hdr->cb_ctx) < 0)
        H5E_THROW(H5E_CANTENCODE, "can't encode fixed array data elements")
    image += (dblk_page->nelmts * dblk_page->hdr->cparam.raw_elmt_size);

    /* Compute metadata checksum */
    metadata_chksum = H5_checksum_metadata(_image, (size_t)(image - (uint8_t *)_image), 0);

    /* Metadata checksum */
    UINT32ENCODE(image, metadata_chksum);

    /* Sanity check */
    HDassert((size_t)(image - (uint8_t *)_image) == len);

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_dblk_page_serialize() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_dblk_page_notify
 *
 * Purpose:	Handle cache action notifications
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *		Oct 17 2016
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_dblk_page_notify(H5AC_notify_action_t action, void *_thing))

    /* Local variables */
    H5FA_dblk_page_t *dblk_page = (H5FA_dblk_page_t *)_thing;      /* Pointer to the object */

    /* Sanity check */
    HDassert(dblk_page);

    /* Determine which action to take */
    switch(action) {
        case H5AC_NOTIFY_ACTION_AFTER_INSERT:
        case H5AC_NOTIFY_ACTION_AFTER_LOAD:
        case H5AC_NOTIFY_ACTION_AFTER_FLUSH:
            /* do nothing */
            break;

        case H5AC_NOTIFY_ACTION_BEFORE_EVICT:
            /* Detach from 'top' proxy for fixed array */
            if(dblk_page->top_proxy) {
                if(H5AC_proxy_entry_remove_child(dblk_page->top_proxy, dblk_page) < 0)
                    H5E_THROW(H5E_CANTUNDEPEND, "unable to destroy flush dependency between data block page and fixed array 'top' proxy")
                dblk_page->top_proxy = NULL;
            } /* end if */
            break;

        case H5AC_NOTIFY_ACTION_ENTRY_DIRTIED:
        case H5AC_NOTIFY_ACTION_ENTRY_CLEANED:
        case H5AC_NOTIFY_ACTION_CHILD_DIRTIED:
        case H5AC_NOTIFY_ACTION_CHILD_CLEANED:
        case H5AC_NOTIFY_ACTION_CHILD_UNSERIALIZED:
        case H5AC_NOTIFY_ACTION_CHILD_SERIALIZED:
            /* do nothing */
            break;

        default:
#ifdef NDEBUG
            H5E_THROW(H5E_BADVALUE, "unknown action from metadata cache")
#else /* NDEBUG */
            HDassert(0 && "Unknown action?!?");
#endif /* NDEBUG */
    } /* end switch */

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_dblk_page_notify() */


/*-------------------------------------------------------------------------
 * Function:	H5FA__cache_dblk_page_free_icr
 *
 * Purpose:	Destroy/release an "in core representation" of a data
 *              structure
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              August 14, 2013
 *
 *-------------------------------------------------------------------------
 */
BEGIN_FUNC(STATIC, ERR,
herr_t, SUCCEED, FAIL,
H5FA__cache_dblk_page_free_icr(void *thing))

    /* Check arguments */
    HDassert(thing);

    /* Release the fixed array data block page */
    if(H5FA__dblk_page_dest((H5FA_dblk_page_t *)thing) < 0)
        H5E_THROW(H5E_CANTFREE, "can't free fixed array data block page")

CATCH

END_FUNC(STATIC)   /* end H5FA__cache_dblk_page_free_icr() */


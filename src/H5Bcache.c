/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the COPYING file, which can be found at the root of the source code       *
 * distribution tree, or in https://www.hdfgroup.org/licenses.               *
 * If you do not have access to either file, you may request a copy from     *
 * help@hdfgroup.org.                                                        *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/*-------------------------------------------------------------------------
 *
 * Created:		H5Bcache.c
 *			Oct 31 2005
 *			Quincey Koziol
 *
 * Purpose:		Implement B-tree metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Bmodule.h" /* This source code file is part of the H5B module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions			*/
#include "H5Bpkg.h"      /* B-link trees				*/
#include "H5Eprivate.h"  /* Error handling		  	*/
#include "H5MMprivate.h" /* Memory management			*/

/****************/
/* Local Macros */
/****************/

/******************/
/* Local Typedefs */
/******************/

/********************/
/* Local Prototypes */
/********************/

/* Metadata cache callbacks */
static herr_t H5B__cache_get_initial_load_size(void *udata, size_t *image_len);
static void * H5B__cache_deserialize(const void *image, size_t len, void *udata, hbool_t *dirty);
static herr_t H5B__cache_image_len(const void *thing, size_t *image_len);
static herr_t H5B__cache_serialize(const H5F_t *f, void *image, size_t len, void *thing);
static herr_t H5B__cache_free_icr(void *thing);

/*********************/
/* Package Variables */
/*********************/

/* H5B inherits cache-like properties from H5AC */
const H5AC_class_t H5AC_BT[1] = {{
    H5AC_BT_ID,                       /* Metadata client ID */
    "v1 B-tree",                      /* Metadata client name (for debugging) */
    H5FD_MEM_BTREE,                   /* File space memory type for client */
    H5AC__CLASS_NO_FLAGS_SET,         /* Client class behavior flags */
    H5B__cache_get_initial_load_size, /* 'get_initial_load_size' callback */
    NULL,                             /* 'get_final_load_size' callback */
    NULL,                             /* 'verify_chksum' callback */
    H5B__cache_deserialize,           /* 'deserialize' callback */
    H5B__cache_image_len,             /* 'image_len' callback */
    NULL,                             /* 'pre_serialize' callback */
    H5B__cache_serialize,             /* 'serialize' callback */
    NULL,                             /* 'notify' callback */
    H5B__cache_free_icr,              /* 'free_icr' callback */
    NULL,                             /* 'fsf_size' callback */
}};

/*******************/
/* Local Variables */
/*******************/

/*-------------------------------------------------------------------------
 * Function:    H5B__cache_get_initial_load_size
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 18, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B__cache_get_initial_load_size(void *_udata, size_t *image_len)
{
    H5B_cache_ud_t *udata = (H5B_cache_ud_t *)_udata; /* User data for callback */
    H5B_shared_t *  shared;                           /* Pointer to shared B-tree info */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(udata);
    HDassert(image_len);

    /* Get shared info for B-tree */
    shared = (H5B_shared_t *)H5UC_GET_OBJ(udata->rc_shared);
    HDassert(shared);

    /* Set the image length size */
    *image_len = shared->sizeof_rnode;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5B__cache_get_initial_load_size() */

/*-------------------------------------------------------------------------
 * Function:    H5B__cache_deserialize
 *
 * Purpose:     Deserialize the data structure from disk.
 *
 * Return:	Success:	Pointer to a new B-tree node.
 *		Failure:	NULL
 *
 * Programmer:  Quincey Koziol
 *              Mar 24, 2008
 *
 *-------------------------------------------------------------------------
 */
static void *
H5B__cache_deserialize(const void *_image, size_t H5_ATTR_UNUSED len, void *_udata,
                       hbool_t H5_ATTR_UNUSED *dirty)
{
    H5B_t *         bt    = NULL;                     /* Pointer to the deserialized B-tree node */
    H5B_cache_ud_t *udata = (H5B_cache_ud_t *)_udata; /* User data for callback */
    H5B_shared_t *  shared;                           /* Pointer to shared B-tree info */
    const uint8_t * image = (const uint8_t *)_image;  /* Pointer into image buffer */
    uint8_t *       native;                           /* Pointer to native keys */
    unsigned        u;                                /* Local index variable */
    H5B_t *         ret_value = NULL;                 /* Return value */

    FUNC_ENTER_STATIC

    /* check arguments */
    HDassert(image);
    HDassert(udata);

    /* Allocate the B-tree node in memory */
    if (NULL == (bt = H5FL_MALLOC(H5B_t)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTALLOC, NULL, "can't allocate B-tree struct")
    HDmemset(&bt->cache_info, 0, sizeof(H5AC_info_t));

    /* Set & increment the ref-counted "shared" B-tree information for the node */
    bt->rc_shared = udata->rc_shared;
    H5UC_INC(bt->rc_shared);

    /* Get a pointer to the shared info, for convenience */
    shared = (H5B_shared_t *)H5UC_GET_OBJ(bt->rc_shared);
    HDassert(shared);

    /* Allocate space for the native keys and child addresses */
    if (NULL == (bt->native = H5FL_BLK_MALLOC(native_block, shared->sizeof_keys)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTALLOC, NULL, "can't allocate buffer for native keys")
    if (NULL == (bt->child = H5FL_SEQ_MALLOC(haddr_t, (size_t)shared->two_k)))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTALLOC, NULL, "can't allocate buffer for child addresses")

    /* magic number */
    if (HDmemcmp(image, H5B_MAGIC, (size_t)H5_SIZEOF_MAGIC) != 0)
        HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, NULL, "wrong B-tree signature")
    image += H5_SIZEOF_MAGIC;

    /* node type and level */
    if (*image++ != (uint8_t)udata->type->id)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTLOAD, NULL, "incorrect B-tree node type")
    bt->level = *image++;

    /* entries used */
    UINT16DECODE(image, bt->nchildren);

    /* Check if bt->nchildren is greater than two_k */
    if (bt->nchildren > shared->two_k)
        HGOTO_ERROR(H5E_BTREE, H5E_BADVALUE, NULL, "number of children is greater than maximum")

    /* sibling pointers */
    H5F_addr_decode(udata->f, (const uint8_t **)&image, &(bt->left));
    H5F_addr_decode(udata->f, (const uint8_t **)&image, &(bt->right));

    /* the child/key pairs */
    native = bt->native;
    for (u = 0; u < bt->nchildren; u++) {
        /* Decode native key value */
        if ((udata->type->decode)(shared, image, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDECODE, NULL, "unable to decode key")
        image += shared->sizeof_rkey;
        native += udata->type->sizeof_nkey;

        /* Decode address value */
        H5F_addr_decode(udata->f, (const uint8_t **)&image, bt->child + u);
    } /* end for */

    /* Decode final key */
    if (bt->nchildren > 0) {
        /* Decode native key value */
        if ((udata->type->decode)(shared, image, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTDECODE, NULL, "unable to decode key")
    } /* end if */

    /* Sanity check */
    HDassert((size_t)((const uint8_t *)image - (const uint8_t *)_image) <= len);

    /* Set return value */
    ret_value = bt;

done:
    if (!ret_value && bt)
        if (H5B__node_dest(bt) < 0)
            HDONE_ERROR(H5E_BTREE, H5E_CANTFREE, NULL, "unable to destroy B-tree node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B__cache_deserialize() */

/*-------------------------------------------------------------------------
 * Function:    H5B__cache_image_len
 *
 * Purpose:     Compute the size of the data structure on disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              May 20, 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B__cache_image_len(const void *_thing, size_t *image_len)
{
    const H5B_t * bt = (const H5B_t *)_thing; /* Pointer to the B-tree node */
    H5B_shared_t *shared;                     /* Pointer to shared B-tree info */

    FUNC_ENTER_STATIC_NOERR

    /* Check arguments */
    HDassert(bt);
    HDassert(image_len);

    /* Get shared info for B-tree */
    shared = (H5B_shared_t *)H5UC_GET_OBJ(bt->rc_shared);
    HDassert(shared);

    /* Set the image length size */
    *image_len = shared->sizeof_rnode;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5B__cache_image_len() */

/*-------------------------------------------------------------------------
 * Function:    H5B__cache_serialize
 *
 * Purpose:     Serialize the data structure for writing to disk.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Mar 24, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B__cache_serialize(const H5F_t *f, void *_image, size_t H5_ATTR_UNUSED len, void *_thing)
{
    H5B_t *       bt = (H5B_t *)_thing;      /* Pointer to the B-tree node */
    H5B_shared_t *shared;                    /* Pointer to shared B-tree info */
    uint8_t *     image = (uint8_t *)_image; /* Pointer into image buffer */
    uint8_t *     native;                    /* Pointer to native keys */
    unsigned      u;                         /* Local index counter */
    herr_t        ret_value = SUCCEED;       /* Return value */

    FUNC_ENTER_STATIC

    /* check arguments */
    HDassert(image);
    HDassert(bt);
    HDassert(bt->rc_shared);
    shared = (H5B_shared_t *)H5UC_GET_OBJ(bt->rc_shared);
    HDassert(shared);
    HDassert(shared->type);
    HDassert(shared->type->encode);

    /* magic number */
    H5MM_memcpy(image, H5B_MAGIC, (size_t)H5_SIZEOF_MAGIC);
    image += 4;

    /* node type and level */
    *image++ = (uint8_t)shared->type->id;

    /* 2^8 limit: only 1 byte is used to store node level */
    if (bt->level >= HDpow(2, LEVEL_BITS))
        HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, FAIL, "unable to encode node level")

    H5_CHECK_OVERFLOW(bt->level, unsigned, uint8_t);
    *image++ = (uint8_t)bt->level;

    /* entries used */
    UINT16ENCODE(image, bt->nchildren);

    /* sibling pointers */
    H5F_addr_encode(f, &image, bt->left);
    H5F_addr_encode(f, &image, bt->right);

    /* child keys and pointers */
    native = bt->native;
    for (u = 0; u < bt->nchildren; ++u) {
        /* encode the key */
        if (shared->type->encode(shared, image, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, FAIL, "unable to encode B-tree key")
        image += shared->sizeof_rkey;
        native += shared->type->sizeof_nkey;

        /* encode the child address */
        H5F_addr_encode(f, &image, bt->child[u]);
    } /* end for */
    if (bt->nchildren > 0) {
        /* Encode the final key */
        if (shared->type->encode(shared, image, native) < 0)
            HGOTO_ERROR(H5E_BTREE, H5E_CANTENCODE, FAIL, "unable to encode B-tree key")
        image += shared->sizeof_rkey;
    } /* end if */

    /* Sanity check */
    HDassert((size_t)(image - (uint8_t *)_image) <= len);

    /* Clear rest of node */
    HDmemset(image, 0, len - (size_t)(image - (uint8_t *)_image));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B__cache_serialize() */

/*-------------------------------------------------------------------------
 * Function:    H5B__cache_free_icr
 *
 * Purpose:     Destroy/release an "in core representation" of a data structure
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Mar 26, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5B__cache_free_icr(void *thing)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* Check arguments */
    HDassert(thing);

    /* Destroy B-tree node */
    if (H5B__node_dest((H5B_t *)thing) < 0)
        HGOTO_ERROR(H5E_BTREE, H5E_CANTFREE, FAIL, "unable to destroy B-tree node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5B__cache_free_icr() */

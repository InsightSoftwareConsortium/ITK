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
 * Created:		H5Gcache.c
 *			Feb  5 2008
 *			Quincey Koziol
 *
 * Purpose:		Implement group metadata cache methods.
 *
 *-------------------------------------------------------------------------
 */

/****************/
/* Module Setup */
/****************/

#include "H5Gmodule.h" /* This source code file is part of the H5G module */

/***********/
/* Headers */
/***********/
#include "H5private.h"   /* Generic Functions			*/
#include "H5Eprivate.h"  /* Error handling		  	*/
#include "H5Gpkg.h"      /* Groups		  		*/
#include "H5MFprivate.h" /* File memory management		*/
#include "H5MMprivate.h" /* Memory management			*/
#include "H5WBprivate.h" /* Wrapped Buffers                      */

/****************/
/* Local Macros */
/****************/

#define H5G_NODE_VERS 1 /* Symbol table node version number   */

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
static herr_t H5G__cache_node_get_initial_load_size(void *udata, size_t *image_len);
static void * H5G__cache_node_deserialize(const void *image, size_t len, void *udata, hbool_t *dirty);
static herr_t H5G__cache_node_image_len(const void *thing, size_t *image_len);
static herr_t H5G__cache_node_serialize(const H5F_t *f, void *image, size_t len, void *thing);
static herr_t H5G__cache_node_free_icr(void *thing);

/*********************/
/* Package Variables */
/*********************/

/*****************************/
/* Library Private Variables */
/*****************************/

/*******************/
/* Local Variables */
/*******************/

/* Symbol table nodes inherit cache-like properties from H5AC */
const H5AC_class_t H5AC_SNODE[1] = {{
    H5AC_SNODE_ID,                         /* Metadata client ID */
    "Symbol table node",                   /* Metadata client name (for debugging) */
    H5FD_MEM_BTREE,                        /* File space memory type for client */
    H5AC__CLASS_NO_FLAGS_SET,              /* Client class behavior flags */
    H5G__cache_node_get_initial_load_size, /* 'get_initial_load_size' callback */
    NULL,                                  /* 'get_final_load_size' callback */
    NULL,                                  /* 'verify_chksum' callback */
    H5G__cache_node_deserialize,           /* 'deserialize' callback */
    H5G__cache_node_image_len,             /* 'image_len' callback */
    NULL,                                  /* 'pre_serialize' callback */
    H5G__cache_node_serialize,             /* 'serialize' callback */
    NULL,                                  /* 'notify' callback */
    H5G__cache_node_free_icr,              /* 'free_icr' callback */
    NULL,                                  /* 'fsf_size' callback */
}};

/* Declare extern the free list to manage the H5G_node_t struct */
H5FL_EXTERN(H5G_node_t);

/* Declare extern the free list to manage sequences of H5G_entry_t's */
H5FL_SEQ_EXTERN(H5G_entry_t);

/*-------------------------------------------------------------------------
 * Function:    H5G__cache_node_get_initial_load_size()
 *
 * Purpose:	Determine the size of the on-disk image of the node, and
 *		return this value in *image_len.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/21/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G__cache_node_get_initial_load_size(void *_udata, size_t *image_len)
{
    H5F_t *f = (H5F_t *)_udata; /* User data for callback */

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(f);
    HDassert(image_len);

    /* Set the image length size */
    *image_len = (size_t)(H5G_NODE_SIZE(f));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G__cache_node_get_initial_load_size() */

/*-------------------------------------------------------------------------
 * Function:    H5G__cache_node_deserialize
 *
 * Purpose:	Given a buffer containing the on disk image of a symbol table
 *		node, allocate an instance of H5G_node_t, load the contence of the
 *		image into it, and return a pointer to the instance.
 *
 *		Note that deserializing the image requires access to the file
 *		pointer, which is not included in the parameter list for this
 *		callback.  Finesse this issue by passing in the file pointer
 *		twice to the H5AC_protect() call -- once as the file pointer
 *		proper, and again as the user data
 *
 * Return:      Success:        Pointer to in core representation
 *              Failure:        NULL
 *
 * Programmer:  John Mainzer
 *              6/21/14
 *
 *-------------------------------------------------------------------------
 */
static void *
H5G__cache_node_deserialize(const void *_image, size_t len, void *_udata, hbool_t H5_ATTR_UNUSED *dirty)
{
    H5F_t *        f         = (H5F_t *)_udata;         /* User data for callback */
    H5G_node_t *   sym       = NULL;                    /* Symbol table node created */
    const uint8_t *image     = (const uint8_t *)_image; /* Pointer to image to deserialize */
    const uint8_t *image_end = image + len - 1;         /* Pointer to end of image buffer */
    void *         ret_value = NULL;                    /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(image);
    HDassert(len > 0);
    HDassert(f);
    HDassert(dirty);

    /* Allocate symbol table data structures */
    if (NULL == (sym = H5FL_CALLOC(H5G_node_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
    sym->node_size = (size_t)(H5G_NODE_SIZE(f));
    if (NULL == (sym->entry = H5FL_SEQ_CALLOC(H5G_entry_t, (size_t)(2 * H5F_SYM_LEAF_K(f)))))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* magic */
    if (HDmemcmp(image, H5G_NODE_MAGIC, (size_t)H5_SIZEOF_MAGIC) != 0)
        HGOTO_ERROR(H5E_SYM, H5E_BADVALUE, NULL, "bad symbol table node signature")
    image += H5_SIZEOF_MAGIC;

    /* version */
    if (H5G_NODE_VERS != *image++)
        HGOTO_ERROR(H5E_SYM, H5E_VERSION, NULL, "bad symbol table node version")

    /* reserved */
    image++;

    /* number of symbols */
    UINT16DECODE(image, sym->nsyms);

    /* entries */
    if (H5G__ent_decode_vec(f, &image, image_end, sym->entry, sym->nsyms) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTLOAD, NULL, "unable to decode symbol table entries")

    /* Set return value */
    ret_value = sym;

done:
    if (!ret_value)
        if (sym && H5G__node_free(sym) < 0)
            HDONE_ERROR(H5E_SYM, H5E_CANTFREE, NULL, "unable to destroy symbol table node")

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G__cache_node_deserialize() */

/*-------------------------------------------------------------------------
 * Function:    H5G__cache_node_image_len
 *
 * Purpose:     Compute the size of the data structure on disk and return
 *              it in *image_len.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              6/21/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G__cache_node_image_len(const void *_thing, size_t *image_len)
{
    const H5G_node_t *sym = (const H5G_node_t *)_thing; /* Pointer to object */

    FUNC_ENTER_STATIC_NOERR

    /* Sanity checks */
    HDassert(sym);
    HDassert(sym->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(sym->cache_info.type == H5AC_SNODE);
    HDassert(image_len);

    *image_len = sym->node_size;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5G__cache_node_image_len() */

/*-------------------------------------------------------------------------
 * Function:    H5G__cache_node_serialize
 *
 * Purpose:	Given a correctly sized buffer and an instace of H5G_node_t,
 *		serialize the contents of the instance of H5G_node_t, and write
 *		this data into the supplied buffer.  This buffer will be written
 *		to disk.
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              7/21/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G__cache_node_serialize(const H5F_t *f, void *_image, size_t len, void *_thing)
{
    H5G_node_t *sym       = (H5G_node_t *)_thing; /* Pointer to object */
    uint8_t *   image     = (uint8_t *)_image;    /* Pointer into raw data buffer */
    herr_t      ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(f);
    HDassert(image);
    HDassert(sym);
    HDassert(sym->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_MAGIC);
    HDassert(sym->cache_info.type == H5AC_SNODE);
    HDassert(len == sym->node_size);

    /* magic number */
    H5MM_memcpy(image, H5G_NODE_MAGIC, (size_t)H5_SIZEOF_MAGIC);
    image += H5_SIZEOF_MAGIC;

    /* version number */
    *image++ = H5G_NODE_VERS;

    /* reserved */
    *image++ = 0;

    /* number of symbols */
    UINT16ENCODE(image, sym->nsyms);

    /* entries */
    if (H5G__ent_encode_vec(f, &image, sym->entry, sym->nsyms) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTENCODE, FAIL, "can't serialize")

    /* Clear rest of symbol table node */
    HDmemset(image, 0, len - (size_t)(image - (uint8_t *)_image));

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G__cache_node_serialize() */

/*-------------------------------------------------------------------------
 * Function:    H5G__cache_node_free_icr
 *
 * Purpose:	Destroys a symbol table node in memory.
 *
 * Note:	The metadata cache sets the object's cache_info.magic to
 *		H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC before calling a free_icr
 *		callback (checked in assert).
 *
 * Return:      Success:        SUCCEED
 *              Failure:        FAIL
 *
 * Programmer:  John Mainzer
 *              6/21/14
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5G__cache_node_free_icr(void *_thing)
{
    H5G_node_t *sym       = (H5G_node_t *)_thing; /* Pointer to the object */
    herr_t      ret_value = SUCCEED;              /* Return value */

    FUNC_ENTER_STATIC

    /* Sanity checks */
    HDassert(sym);
    HDassert(sym->cache_info.magic == H5C__H5C_CACHE_ENTRY_T_BAD_MAGIC);
    HDassert(sym->cache_info.type == H5AC_SNODE);

    /* Destroy symbol table node */
    if (H5G__node_free(sym) < 0)
        HGOTO_ERROR(H5E_SYM, H5E_CANTFREE, FAIL, "unable to destroy symbol table node")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5G__cache_node_free_icr() */

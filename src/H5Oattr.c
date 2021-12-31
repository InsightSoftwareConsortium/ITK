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

#define H5A_FRIEND     /*suppress error about including H5Apkg   */
#include "H5Omodule.h" /* This source code file is part of the H5O module */
#define H5S_FRIEND     /*suppress error about including H5Spkg	  */

#include "H5private.h"   /* Generic Functions			*/
#include "H5Apkg.h"      /* Attributes				*/
#include "H5Eprivate.h"  /* Error handling		  	*/
#include "H5MMprivate.h" /* Memory management			*/
#include "H5Opkg.h"      /* Object headers			*/
#include "H5Spkg.h"      /* Dataspaces				*/

/* PRIVATE PROTOTYPES */
static herr_t H5O__attr_encode(H5F_t *f, uint8_t *p, const void *mesg);
static void *H5O__attr_decode(H5F_t *f, H5O_t *open_oh, unsigned mesg_flags, unsigned *ioflags, size_t p_size,
                              const uint8_t *p);
static void *H5O__attr_copy(const void *_mesg, void *_dest);
static size_t H5O__attr_size(const H5F_t *f, const void *_mesg);
static herr_t H5O__attr_free(void *mesg);
static herr_t H5O__attr_pre_copy_file(H5F_t *file_src, const void *mesg_src, hbool_t *deleted,
                                      const H5O_copy_t *cpy_info, void *udata);
static void * H5O__attr_copy_file(H5F_t *file_src, const H5O_msg_class_t *mesg_type, void *native_src,
                                  H5F_t *file_dst, hbool_t *recompute_size, H5O_copy_t *cpy_info, void *udata);
static herr_t H5O__attr_post_copy_file(const H5O_loc_t *src_oloc, const void *mesg_src, H5O_loc_t *dst_oloc,
                                       void *mesg_dst, H5O_copy_t *cpy_info);
static herr_t H5O__attr_get_crt_index(const void *_mesg, H5O_msg_crt_idx_t *crt_idx);
static herr_t H5O__attr_set_crt_index(void *_mesg, H5O_msg_crt_idx_t crt_idx);
static herr_t H5O__attr_debug(H5F_t *f, const void *_mesg, FILE *stream, int indent, int fwidth);

/* Set up & include shared message "interface" info */
#define H5O_SHARED_TYPE                H5O_MSG_ATTR
#define H5O_SHARED_DECODE              H5O__attr_shared_decode
#define H5O_SHARED_DECODE_REAL         H5O__attr_decode
#define H5O_SHARED_ENCODE              H5O__attr_shared_encode
#define H5O_SHARED_ENCODE_REAL         H5O__attr_encode
#define H5O_SHARED_SIZE                H5O__attr_shared_size
#define H5O_SHARED_SIZE_REAL           H5O__attr_size
#define H5O_SHARED_DELETE              H5O__attr_shared_delete
#define H5O_SHARED_DELETE_REAL         H5O__attr_delete
#define H5O_SHARED_LINK                H5O__attr_shared_link
#define H5O_SHARED_LINK_REAL           H5O__attr_link
#define H5O_SHARED_COPY_FILE           H5O__attr_shared_copy_file
#define H5O_SHARED_COPY_FILE_REAL      H5O__attr_copy_file
#define H5O_SHARED_POST_COPY_FILE      H5O__attr_shared_post_copy_file
#define H5O_SHARED_POST_COPY_FILE_REAL H5O__attr_post_copy_file
#undef H5O_SHARED_POST_COPY_FILE_UPD
#define H5O_SHARED_DEBUG      H5O__attr_shared_debug
#define H5O_SHARED_DEBUG_REAL H5O__attr_debug
#include "H5Oshared.h" /* Shared Object Header Message Callbacks */

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_ATTR[1] = {{
    H5O_ATTR_ID,                     /* message id number                */
    "attribute",                     /* message name for debugging       */
    sizeof(H5A_t),                   /* native message size              */
    H5O_SHARE_IS_SHARABLE,           /* messages are sharable?           */
    H5O__attr_shared_decode,         /* decode message                   */
    H5O__attr_shared_encode,         /* encode message                   */
    H5O__attr_copy,                  /* copy the native value            */
    H5O__attr_shared_size,           /* size of raw message              */
    H5O__attr_reset,                 /* reset method                     */
    H5O__attr_free,                  /* free method                      */
    H5O__attr_shared_delete,         /* file delete method               */
    H5O__attr_shared_link,           /* link method                      */
    NULL,                            /* set share method                 */
    NULL,                            /* can share method                 */
    H5O__attr_pre_copy_file,         /* pre copy native value to file    */
    H5O__attr_shared_copy_file,      /* copy native value to file        */
    H5O__attr_shared_post_copy_file, /* post copy native value to file   */
    H5O__attr_get_crt_index,         /* get creation index               */
    H5O__attr_set_crt_index,         /* set creation index               */
    H5O__attr_shared_debug           /* debug the message                */
}};

/* Flags for attribute flag encoding */
#define H5O_ATTR_FLAG_TYPE_SHARED  0x01
#define H5O_ATTR_FLAG_SPACE_SHARED 0x02
#define H5O_ATTR_FLAG_ALL          0x03

/* Declare external the free list for H5S_t's */
H5FL_EXTERN(H5S_t);

/* Declare external the free list for H5S_extent_t's */
H5FL_EXTERN(H5S_extent_t);

/*--------------------------------------------------------------------------
 NAME
    H5O__attr_decode
 PURPOSE
    Decode a attribute message and return a pointer to a memory struct
        with the decoded information
 USAGE
    void *H5O__attr_decode(f, mesg_flags, p)
        H5F_t    *f;            IN: pointer to the HDF5 file struct
        H5O_t    *open_oh;      IN: pointer to the object header
        unsigned mesg_flags;    IN: message flags to influence decoding
        unsigned *ioflags;      IN/OUT: flags for decoding
        size_t   p_size;        IN: size of buffer *p
        const uint8_t *p;       IN: the raw information buffer
 RETURNS
    Pointer to the new message in native order on success, NULL on failure
 DESCRIPTION
        This function decodes the "raw" disk form of a attribute message
    into a struct in memory native format.  The struct is allocated within this
    function using malloc() and is returned to the caller.
--------------------------------------------------------------------------*/
static void *
H5O__attr_decode(H5F_t *f, H5O_t *open_oh, unsigned H5_ATTR_UNUSED mesg_flags, unsigned *ioflags,
                 size_t p_size, const uint8_t *p)
{
    H5A_t *       attr = NULL;
    H5S_extent_t *extent;           /*extent dimensionality information  */
    size_t        name_len;         /*attribute name length */
    size_t        dt_size;          /* Datatype size */
    hssize_t      sds_size;         /* Signed Dataspace size */
    hsize_t       ds_size;          /* Dataspace size */
    unsigned      flags     = 0;    /* Attribute flags */
    H5A_t *       ret_value = NULL; /* Return value */

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(f);
    HDassert(p);

    if (NULL == (attr = H5FL_CALLOC(H5A_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    if (NULL == (attr->shared = H5FL_CALLOC(H5A_shared_t)))
        HGOTO_ERROR(H5E_FILE, H5E_NOSPACE, NULL, "can't allocate shared attr structure")

    /* Version number */
    attr->shared->version = *p++;
    if (attr->shared->version < H5O_ATTR_VERSION_1 || attr->shared->version > H5O_ATTR_VERSION_LATEST)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTLOAD, NULL, "bad version number for attribute message")

    /* Get the flags byte if we have a later version of the attribute */
    if (attr->shared->version >= H5O_ATTR_VERSION_2) {
        flags = *p++;

        /* Check for unknown flag */
        if (flags & (unsigned)~H5O_ATTR_FLAG_ALL)
            HGOTO_ERROR(H5E_ATTR, H5E_CANTLOAD, NULL, "unknown flag for attribute message")
    } /* end if */
    else
        p++; /* Byte is unused when version<2 */

    /*
     * Decode the sizes of the parts of the attribute.  The sizes stored in
     * the file are exact but the parts are aligned on 8-byte boundaries.
     */
    UINT16DECODE(p, name_len); /*including null*/
    UINT16DECODE(p, attr->shared->dt_size);
    UINT16DECODE(p, attr->shared->ds_size);

    /*
     * Decode the character encoding for the name for versions 3 or later,
     * as well as some reserved bytes.
     */
    if (attr->shared->version >= H5O_ATTR_VERSION_3)
        attr->shared->encoding = (H5T_cset_t)*p++;

    /* Decode and store the name */
    if (NULL == (attr->shared->name = H5MM_strdup((const char *)p)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Make an attempt to detect corrupted name or name length - HDFFV-10588 */
    if (name_len != (HDstrlen(attr->shared->name) + 1))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDECODE, NULL, "attribute name has different length than stored length")

    if (attr->shared->version < H5O_ATTR_VERSION_2)
        p += H5O_ALIGN_OLD(name_len); /* advance the memory pointer */
    else
        p += name_len; /* advance the memory pointer */

    /* Decode the attribute's datatype */
    if (NULL == (attr->shared->dt = (H5T_t *)(H5O_MSG_DTYPE->decode)(
                     f, open_oh, ((flags & H5O_ATTR_FLAG_TYPE_SHARED) ? H5O_MSG_FLAG_SHARED : 0), ioflags,
                     attr->shared->dt_size, p)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDECODE, NULL, "can't decode attribute datatype")
    if (attr->shared->version < H5O_ATTR_VERSION_2)
        p += H5O_ALIGN_OLD(attr->shared->dt_size);
    else
        p += attr->shared->dt_size;

    /* decode the attribute dataspace.  It can be shared in versions >= 3
     * What's actually shared, though, is only the extent.
     */
    if (NULL == (attr->shared->ds = H5FL_CALLOC(H5S_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* Decode attribute's dataspace extent */
    if ((extent = (H5S_extent_t *)(H5O_MSG_SDSPACE->decode)(
             f, open_oh, ((flags & H5O_ATTR_FLAG_SPACE_SHARED) ? H5O_MSG_FLAG_SHARED : 0), ioflags,
             attr->shared->ds_size, p)) == NULL)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTDECODE, NULL, "can't decode attribute dataspace")

    /* Copy the extent information to the dataspace */
    H5MM_memcpy(&(attr->shared->ds->extent), extent, sizeof(H5S_extent_t));

    /* Release temporary extent information */
    extent = H5FL_FREE(H5S_extent_t, extent);

    /* Default to entire dataspace being selected */
    if (H5S_select_all(attr->shared->ds, FALSE) < 0)
        HGOTO_ERROR(H5E_DATASPACE, H5E_CANTSET, NULL, "unable to set all selection")

    if (attr->shared->version < H5O_ATTR_VERSION_2)
        p += H5O_ALIGN_OLD(attr->shared->ds_size);
    else
        p += attr->shared->ds_size;

    /* Get the datatype's size */
    if (0 == (dt_size = H5T_get_size(attr->shared->dt)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, NULL, "unable to get datatype size")

    /* Get the datatype & dataspace sizes */
    if (0 == (dt_size = H5T_get_size(attr->shared->dt)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, NULL, "unable to get datatype size")
    if ((sds_size = H5S_GET_EXTENT_NPOINTS(attr->shared->ds)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTGET, NULL, "unable to get dataspace size")
    ds_size = (hsize_t)sds_size;

    /* Compute the size of the data */
    H5_CHECKED_ASSIGN(attr->shared->data_size, size_t, ds_size * (hsize_t)dt_size, hsize_t);

    /* Go get the data */
    if (attr->shared->data_size) {
        /* Ensure that data size doesn't exceed buffer size, in case of
           it's being corrupted in the file */
        if (attr->shared->data_size > p_size)
            HGOTO_ERROR(H5E_RESOURCE, H5E_OVERFLOW, NULL, "data size exceeds buffer size")

        if (NULL == (attr->shared->data = H5FL_BLK_MALLOC(attr_buf, attr->shared->data_size)))
            HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")
        H5MM_memcpy(attr->shared->data, p, attr->shared->data_size);
    } /* end if */

    /* Increment the reference count for this object header message in cache(compact
       storage) or for the object from dense storage. */
    attr->shared->nrefs++;

    /* Set return value */
    ret_value = attr;

done:
    if (NULL == ret_value)
        if (attr) {
            /* Free any dynamically allocated items */
            if (attr->shared)
                if (H5A__shared_free(attr) < 0)
                    HDONE_ERROR(H5E_ATTR, H5E_CANTRELEASE, NULL, "can't release attribute info")

            attr = H5FL_FREE(H5A_t, attr);
        } /* end if */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__attr_decode() */

/*--------------------------------------------------------------------------
 NAME
    H5O__attr_encode
 PURPOSE
    Encode a simple attribute message
 USAGE
    herr_t H5O__attr_encode(f, p, mesg)
        H5F_t *f;         IN: pointer to the HDF5 file struct
        const uint8 *p;         IN: the raw information buffer
        const void *mesg;       IN: Pointer to the simple datatype struct
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
        This function encodes the native memory form of the attribute
    message in the "raw" disk form.
--------------------------------------------------------------------------*/
static herr_t
H5O__attr_encode(H5F_t *f, uint8_t *p, const void *mesg)
{
    const H5A_t *attr = (const H5A_t *)mesg;
    size_t       name_len;        /* Attribute name length */
    htri_t       is_type_shared;  /* Flag to indicate that a shared datatype is used for this attribute */
    htri_t       is_space_shared; /* Flag to indicate that a shared dataspace is used for this attribute */
    unsigned     flags     = 0;   /* Attribute flags */
    herr_t       ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(f);
    HDassert(p);
    HDassert(attr);

    /* Check whether datatype and dataspace are shared */
    if ((is_type_shared = H5O_msg_is_shared(H5O_DTYPE_ID, attr->shared->dt)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "can't determine if datatype is shared")

    if ((is_space_shared = H5O_msg_is_shared(H5O_SDSPACE_ID, attr->shared->ds)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_BADMESG, FAIL, "can't determine if dataspace is shared")

    /* Encode Version */
    *p++ = attr->shared->version;

    /* Set attribute flags if version >1 */
    if (attr->shared->version >= H5O_ATTR_VERSION_2) {
        flags = (is_type_shared ? H5O_ATTR_FLAG_TYPE_SHARED : 0);
        flags |= (is_space_shared ? H5O_ATTR_FLAG_SPACE_SHARED : 0);
        *p++ = (uint8_t)flags; /* Set flags for attribute */
    }                          /* end if */
    else
        *p++ = 0; /* Reserved, for version <2 */

    /*
     * Encode the lengths of the various parts of the attribute message. The
     * encoded lengths are exact but we pad each part except the data to be a
     * multiple of eight bytes (in the first version).
     */
    name_len = HDstrlen(attr->shared->name) + 1;
    UINT16ENCODE(p, name_len);
    UINT16ENCODE(p, attr->shared->dt_size);
    UINT16ENCODE(p, attr->shared->ds_size);

    /* The character encoding for the attribute's name, in later versions */
    if (attr->shared->version >= H5O_ATTR_VERSION_3)
        *p++ = (uint8_t)attr->shared->encoding;

    /* Write the name including null terminator */
    H5MM_memcpy(p, attr->shared->name, name_len);
    if (attr->shared->version < H5O_ATTR_VERSION_2) {
        /* Pad to the correct number of bytes */
        HDmemset(p + name_len, 0, H5O_ALIGN_OLD(name_len) - name_len);
        p += H5O_ALIGN_OLD(name_len);
    } /* end if */
    else
        p += name_len;

    /* encode the attribute datatype */
    if ((H5O_MSG_DTYPE->encode)(f, FALSE, p, attr->shared->dt) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "can't encode attribute datatype")

    if (attr->shared->version < H5O_ATTR_VERSION_2) {
        HDmemset(p + attr->shared->dt_size, 0, H5O_ALIGN_OLD(attr->shared->dt_size) - attr->shared->dt_size);
        p += H5O_ALIGN_OLD(attr->shared->dt_size);
    } /* end if */
    else
        p += attr->shared->dt_size;

    /* encode the attribute dataspace */
    if ((H5O_MSG_SDSPACE->encode)(f, FALSE, p, &(attr->shared->ds->extent)) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTENCODE, FAIL, "can't encode attribute dataspace")

    if (attr->shared->version < H5O_ATTR_VERSION_2) {
        HDmemset(p + attr->shared->ds_size, 0, H5O_ALIGN_OLD(attr->shared->ds_size) - attr->shared->ds_size);
        p += H5O_ALIGN_OLD(attr->shared->ds_size);
    } /* end if */
    else
        p += attr->shared->ds_size;

    /* Store attribute data.  If there's no data, store 0 as fill value. */
    if (attr->shared->data)
        H5MM_memcpy(p, attr->shared->data, attr->shared->data_size);
    else
        HDmemset(p, 0, attr->shared->data_size);

done:
    FUNC_LEAVE_NOAPI(ret_value);
} /* end H5O__attr_encode() */

/*--------------------------------------------------------------------------
 NAME
    H5O__attr_copy
 PURPOSE
    Copies a message from MESG to DEST, allocating DEST if necessary.
 USAGE
    void *H5O__attr_copy(mesg, dest)
        const void *mesg;       IN: Pointer to the source attribute struct
        const void *dest;       IN: Pointer to the destination attribute struct
 RETURNS
    Pointer to DEST on success, NULL on failure
 DESCRIPTION
        This function copies a native (memory) attribute message,
    allocating the destination structure if necessary.
--------------------------------------------------------------------------*/
static void *
H5O__attr_copy(const void *_src, void *_dst)
{
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(_src);

    /* copy */
    if (NULL == (ret_value = (H5A_t *)H5A__copy((H5A_t *)_dst, (const H5A_t *)_src)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "can't copy attribute")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__attr_copy() */

/*--------------------------------------------------------------------------
 NAME
    H5O__attr_size
 PURPOSE
    Return the raw message size in bytes
 USAGE
    size_t H5O__attr_size(f, mesg)
        H5F_t *f;         IN: pointer to the HDF5 file struct
        const void *mesg;     IN: Pointer to the source attribute struct
 RETURNS
    Size of message on success, 0 on failure
 DESCRIPTION
        This function returns the size of the raw attribute message on
    success.  (Not counting the message type or size fields, only the data
    portion of the message).  It doesn't take into account alignment.
--------------------------------------------------------------------------*/
static size_t
H5O__attr_size(const H5F_t H5_ATTR_UNUSED *f, const void *_mesg)
{
    const H5A_t *attr = (const H5A_t *)_mesg;
    size_t       name_len;
    size_t       ret_value = 0;

    FUNC_ENTER_STATIC_NOERR

    HDassert(attr);

    /* Common size information */
    ret_value = 1 + /*version               */
                1 + /*reserved/flags	*/
                2 + /*name size inc. null	*/
                2 + /*type size		*/
                2;  /*space size		*/

    /* Length of attribute name */
    name_len = HDstrlen(attr->shared->name) + 1;

    /* Version-specific size information */
    if (attr->shared->version == H5O_ATTR_VERSION_1)
        ret_value += H5O_ALIGN_OLD(name_len) +              /*attribute name	*/
                     H5O_ALIGN_OLD(attr->shared->dt_size) + /*datatype		*/
                     H5O_ALIGN_OLD(attr->shared->ds_size) + /*dataspace		*/
                     attr->shared->data_size;               /*the data itself	*/
    else if (attr->shared->version == H5O_ATTR_VERSION_2)
        ret_value += name_len +               /*attribute name	*/
                     attr->shared->dt_size +  /*datatype		*/
                     attr->shared->ds_size +  /*dataspace		*/
                     attr->shared->data_size; /*the data itself	*/
    else if (attr->shared->version == H5O_ATTR_VERSION_3)
        ret_value += 1 +                      /*character encoding    */
                     name_len +               /*attribute name	*/
                     attr->shared->dt_size +  /*datatype		*/
                     attr->shared->ds_size +  /*dataspace		*/
                     attr->shared->data_size; /*the data itself	*/
    else
        HDassert(0 && "Bad attribute version");

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__attr_size() */

/*-------------------------------------------------------------------------
 * Function:    H5O__attr_reset
 *
 * Purpose:     Frees resources within a attribute message, but doesn't free
 *              the message itself.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Robb Matzke
 *              Tuesday, December  9, 1997
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O__attr_reset(void H5_ATTR_UNUSED *_mesg)
{
    FUNC_ENTER_PACKAGE_NOERR

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__attr_reset() */

/*-------------------------------------------------------------------------
 * Function:    H5O__attr_free
 *
 * Purpose:     Frees the message
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *              Thursday, November 18, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__attr_free(void *mesg)
{
    H5A_t *attr      = (H5A_t *)mesg;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    HDassert(mesg);

    if (H5A__close(attr) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCLOSEOBJ, FAIL, "unable to close attribute object")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__attr_free() */

/*-------------------------------------------------------------------------
 * Function:    H5O__attr_delete
 *
 * Purpose:     Free file space referenced by message
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Friday, September 26, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O__attr_delete(H5F_t *f, H5O_t *oh, void *_mesg)
{
    H5A_t *attr      = (H5A_t *)_mesg;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(f);
    HDassert(attr);

    /* Decrement reference count on datatype in file */
    if ((H5O_MSG_DTYPE->del)(f, oh, attr->shared->dt) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_LINKCOUNT, FAIL, "unable to adjust datatype link count")

    /* Decrement reference count on dataspace in file */
    if ((H5O_MSG_SDSPACE->del)(f, oh, attr->shared->ds) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_LINKCOUNT, FAIL, "unable to adjust dataspace link count")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__attr_delete() */

/*-------------------------------------------------------------------------
 * Function:    H5O__attr_link
 *
 * Purpose:     Increment reference count on any objects referenced by
 *              message
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Friday, September 26, 2003
 *
 *-------------------------------------------------------------------------
 */
herr_t
H5O__attr_link(H5F_t *f, H5O_t *oh, void *_mesg)
{
    H5A_t *attr      = (H5A_t *)_mesg;
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_PACKAGE

    /* check args */
    HDassert(f);
    HDassert(attr);

    /* Re-share attribute's datatype and dataspace to increment their
     * reference count if they're shared.
     * Otherwise they may be deleted when the attribute
     * message is deleted.
     */
    /* Increment reference count on datatype & dataspace in file */
    if ((H5O_MSG_DTYPE->link)(f, oh, attr->shared->dt) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_LINKCOUNT, FAIL, "unable to adjust datatype link count")
    if ((H5O_MSG_SDSPACE->link)(f, oh, attr->shared->ds) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_LINKCOUNT, FAIL, "unable to adjust dataspace link count")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__attr_link() */

/*-------------------------------------------------------------------------
 * Function:    H5O__attr_pre_copy_file
 *
 * Purpose:     Perform any necessary actions before copying message between
 *              files for attribute messages.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Quincey Koziol
 *              Monday, June 26, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__attr_pre_copy_file(H5F_t H5_ATTR_UNUSED *file_src, const void *native_src, hbool_t *deleted,
                        const H5O_copy_t *cpy_info, void H5_ATTR_UNUSED *udata)
{
    const H5A_t *attr_src  = (const H5A_t *)native_src; /* Source attribute */
    herr_t       ret_value = SUCCEED;                   /* Return value */

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(deleted);
    HDassert(cpy_info);
    HDassert(cpy_info->file_dst);

    /* Check to ensure that the version of the message to be copied does not exceed
     * the message version allowed by the destination file's high bound.
     */
    if (attr_src->shared->version > H5O_attr_ver_bounds[H5F_HIGH_BOUND(cpy_info->file_dst)])
        HGOTO_ERROR(H5E_OHDR, H5E_BADRANGE, FAIL, "attribute message version out of bounds")

    /* If we are not copying attributes into the destination file, indicate
     *  that this message should be deleted.
     */
    if (cpy_info->copy_without_attr)
        *deleted = TRUE;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__attr_pre_copy_file() */

/*-------------------------------------------------------------------------
 * Function:    H5O__attr_copy_file
 *
 * Purpose:     Copies a message from _MESG to _DEST in file
 *
 * Return:      Success:        Ptr to _DEST
 *              Failure:        NULL
 *
 * Programmer:  Quincey Koziol
 *              November 1, 2005
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O__attr_copy_file(H5F_t *file_src, const H5O_msg_class_t H5_ATTR_UNUSED *mesg_type, void *native_src,
                    H5F_t *file_dst, hbool_t *recompute_size, H5O_copy_t *cpy_info,
                    void H5_ATTR_UNUSED *udata)
{
    void *ret_value = NULL; /* Return value */

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(native_src);
    HDassert(file_dst);
    HDassert(cpy_info);
    HDassert(!cpy_info->copy_without_attr);

    /* Mark datatype as being on disk now.  This step used to be done in a lower level
     * by H5O_dtype_decode.  But it has been moved up.  Not an ideal place, but no better
     * place than here. */
    if (H5T_set_loc(((H5A_t *)native_src)->shared->dt, H5F_VOL_OBJ(file_src), H5T_LOC_DISK) < 0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTINIT, NULL, "invalid datatype location")

    if (NULL == (ret_value = H5A__attr_copy_file((H5A_t *)native_src, file_dst, recompute_size, cpy_info)))
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, NULL, "can't copy attribute")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O__attr_copy_file() */

/*-------------------------------------------------------------------------
 * Function:    H5O__attr_post_copy_file
 *
 * Purpose:     Finish copying a message from between files.
 *              We have to copy the values of a reference attribute in the
 *              post copy because H5O_post_copy_file() fails at the case that
 *              an object may have a reference attribute that points to the
 *              object itself.
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:  Peter Cao
 *              March 6, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__attr_post_copy_file(const H5O_loc_t *src_oloc, const void *mesg_src, H5O_loc_t *dst_oloc, void *mesg_dst,
                         H5O_copy_t *cpy_info)
{
    herr_t ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    if (H5A__attr_post_copy_file(src_oloc, (const H5A_t *)mesg_src, dst_oloc, (H5A_t *)mesg_dst, cpy_info) <
        0)
        HGOTO_ERROR(H5E_ATTR, H5E_CANTCOPY, FAIL, "can't copy attribute")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* H5O__attr_post_copy_file() */

/*-------------------------------------------------------------------------
 * Function:    H5O__attr_get_crt_index
 *
 * Purpose:     Get creation index from the message
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 18, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__attr_get_crt_index(const void *_mesg, H5O_msg_crt_idx_t *crt_idx /*out*/)
{
    const H5A_t *attr = (const H5A_t *)_mesg;

    FUNC_ENTER_STATIC_NOERR

    HDassert(attr);
    HDassert(crt_idx);

    /* Get the attribute's creation index */
    *crt_idx = attr->shared->crt_idx;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__attr_get_crt_index() */

/*-------------------------------------------------------------------------
 * Function:    H5O__attr_set_crt_index
 *
 * Purpose:     Set creation index from the message
 *
 * Return:      SUCCEED/FAIL
 *
 * Programmer:	Quincey Koziol
 *              Thursday, January 18, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__attr_set_crt_index(void *_mesg, H5O_msg_crt_idx_t crt_idx)
{
    H5A_t *attr = (H5A_t *)_mesg;

    FUNC_ENTER_STATIC_NOERR

    HDassert(attr);

    /* Set the creation index */
    attr->shared->crt_idx = crt_idx;

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__attr_set_crt_index() */

/*--------------------------------------------------------------------------
 NAME
    H5O__attr_debug
 PURPOSE
    Prints debugging information for an attribute message
 USAGE
    void *H5O__attr_debug(f, mesg, stream, indent, fwidth)
        H5F_t *f;               IN: pointer to the HDF5 file struct
        const void *mesg;       IN: Pointer to the source attribute struct
        FILE *stream;           IN: Pointer to the stream for output data
        int indent;            IN: Amount to indent information by
        int fwidth;            IN: Field width (?)
 RETURNS
    Non-negative on success/Negative on failure
 DESCRIPTION
        This function prints debugging output to the stream passed as a
    parameter.
--------------------------------------------------------------------------*/
static herr_t
H5O__attr_debug(H5F_t *f, const void *_mesg, FILE *stream, int indent, int fwidth)
{
    const H5A_t *mesg = (const H5A_t *)_mesg;
    const char * s;                   /* Temporary string pointer */
    char         buf[128];            /* Temporary string buffer */
    herr_t       ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(f);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s \"%s\"\n", indent, "", fwidth, "Name:", mesg->shared->name);
    switch (mesg->shared->encoding) {
        case H5T_CSET_ASCII:
            s = "ASCII";
            break;

        case H5T_CSET_UTF8:
            s = "UTF-8";
            break;

        case H5T_CSET_RESERVED_2:
        case H5T_CSET_RESERVED_3:
        case H5T_CSET_RESERVED_4:
        case H5T_CSET_RESERVED_5:
        case H5T_CSET_RESERVED_6:
        case H5T_CSET_RESERVED_7:
        case H5T_CSET_RESERVED_8:
        case H5T_CSET_RESERVED_9:
        case H5T_CSET_RESERVED_10:
        case H5T_CSET_RESERVED_11:
        case H5T_CSET_RESERVED_12:
        case H5T_CSET_RESERVED_13:
        case H5T_CSET_RESERVED_14:
        case H5T_CSET_RESERVED_15:
            HDsnprintf(buf, sizeof(buf), "H5T_CSET_RESERVED_%d", (int)(mesg->shared->encoding));
            s = buf;
            break;

        case H5T_CSET_ERROR:
        default:
            HDsnprintf(buf, sizeof(buf), "Unknown character set: %d", (int)(mesg->shared->encoding));
            s = buf;
            break;
    } /* end switch */
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth, "Character Set of Name:", s);
    HDfprintf(stream, "%*s%-*s %s\n", indent, "", fwidth,
              "Object opened:", mesg->obj_opened ? "TRUE" : "FALSE");
    HDfprintf(stream, "%*s%-*s %" PRIuHADDR "\n", indent, "", fwidth, "Object:", mesg->oloc.addr);

    /* Check for attribute creation order index on the attribute */
    if (mesg->shared->crt_idx != H5O_MAX_CRT_ORDER_IDX)
        HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
                  "Creation Index:", (unsigned)mesg->shared->crt_idx);

    HDfprintf(stream, "%*sDatatype...\n", indent, "");
    HDfprintf(stream, "%*s%-*s %lu\n", indent + 3, "", MAX(0, fwidth - 3),
              "Encoded Size:", (unsigned long)(mesg->shared->dt_size));
    if ((H5O_MSG_DTYPE->debug)(f, mesg->shared->dt, stream, indent + 3, MAX(0, fwidth - 3)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to display datatype message info")

    HDfprintf(stream, "%*sDataspace...\n", indent, "");
    HDfprintf(stream, "%*s%-*s %lu\n", indent + 3, "", MAX(0, fwidth - 3),
              "Encoded Size:", (unsigned long)(mesg->shared->ds_size));
    if (H5S_debug(f, mesg->shared->ds, stream, indent + 3, MAX(0, fwidth - 3)) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_WRITEERROR, FAIL, "unable to display dataspace message info")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__attr_debug() */

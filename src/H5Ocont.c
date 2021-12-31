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
 * Created:             H5Ocont.c
 *                      Aug  6 1997
 *                      Robb Matzke
 *
 * Purpose:             The object header continuation message.  This
 *                      message is only generated and read from within
 *                      the H5O package.  Therefore, do not change
 *                      any definitions in this file!
 *
 *-------------------------------------------------------------------------
 */

#include "H5Omodule.h" /* This source code file is part of the H5O module */

#include "H5private.h"   /* Generic Functions			*/
#include "H5Eprivate.h"  /* Error handling		  	*/
#include "H5FLprivate.h" /* Free Lists				*/
#include "H5MFprivate.h" /* File memory management		*/
#include "H5Opkg.h"      /* Object headers			*/

/* PRIVATE PROTOTYPES */
static void *H5O__cont_decode(H5F_t *f, H5O_t *open_oh, unsigned mesg_flags, unsigned *ioflags, size_t p_size,
                              const uint8_t *p);
static herr_t H5O__cont_encode(H5F_t *f, hbool_t disable_shared, uint8_t *p, const void *_mesg);
static size_t H5O__cont_size(const H5F_t *f, hbool_t disable_shared, const void *_mesg);
static herr_t H5O__cont_free(void *mesg);
static herr_t H5O__cont_delete(H5F_t *f, H5O_t *open_oh, void *_mesg);
static herr_t H5O__cont_debug(H5F_t *f, const void *_mesg, FILE *stream, int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_CONT[1] = {{
    H5O_CONT_ID,        /*message id number             */
    "hdr continuation", /*message name for debugging    */
    sizeof(H5O_cont_t), /*native message size           */
    0,                  /* messages are sharable?       */
    H5O__cont_decode,   /*decode message                */
    H5O__cont_encode,   /*encode message                */
    NULL,               /*no copy method                */
    H5O__cont_size,     /*size of header continuation   */
    NULL,               /*reset method			*/
    H5O__cont_free,     /* free method			*/
    H5O__cont_delete,   /* file delete method		*/
    NULL,               /* link method			*/
    NULL,               /*set share method		*/
    NULL,               /*can share method		*/
    NULL,               /* pre copy native value to file */
    NULL,               /* copy native value to file    */
    NULL,               /* post copy native value to file    */
    NULL,               /* get creation index		*/
    NULL,               /* set creation index		*/
    H5O__cont_debug     /*debugging                     */
}};

/* Declare the free list for H5O_cont_t's */
H5FL_DEFINE(H5O_cont_t);

/*-------------------------------------------------------------------------
 * Function:    H5O__cont_decode
 *
 * Purpose:     Decode the raw header continuation message.
 *
 * Return:      Success:        Ptr to the new native message
 *
 *              Failure:        NULL
 *
 * Programmer:  Robb Matzke
 *              Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O__cont_decode(H5F_t *f, H5O_t H5_ATTR_UNUSED *open_oh, unsigned H5_ATTR_UNUSED mesg_flags,
                 unsigned H5_ATTR_UNUSED *ioflags, size_t H5_ATTR_UNUSED p_size, const uint8_t *p)
{
    H5O_cont_t *cont      = NULL;
    void *      ret_value = NULL; /* Return value */

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(f);
    HDassert(p);

    /* Allocate space for the message */
    if (NULL == (cont = H5FL_MALLOC(H5O_cont_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed");

    /* Decode */
    H5F_addr_decode(f, &p, &(cont->addr));
    H5F_DECODE_LENGTH(f, p, cont->size);
    cont->chunkno = 0;

    /* Set return value */
    ret_value = cont;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cont_decode() */

/*-------------------------------------------------------------------------
 * Function:    H5O__cont_encode
 *
 * Purpose:     Encodes a continuation message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              Aug  7 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cont_encode(H5F_t *f, hbool_t H5_ATTR_UNUSED disable_shared, uint8_t *p, const void *_mesg)
{
    const H5O_cont_t *cont = (const H5O_cont_t *)_mesg;

    FUNC_ENTER_STATIC_NOERR

    /* check args */
    HDassert(f);
    HDassert(p);
    HDassert(cont);
    HDassert(H5F_addr_defined(cont->addr));
    HDassert(cont->size > 0);

    /* encode */
    H5F_addr_encode(f, &p, cont->addr);
    H5F_ENCODE_LENGTH(f, p, cont->size);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__cont_encode() */

/*-------------------------------------------------------------------------
 * Function:    H5O__cont_size
 *
 * Purpose:     Returns the size of the raw message in bytes not counting
 *              the message type or size fields, but only the data fields.
 *              This function doesn't take into account alignment.
 *
 * Return:      Success:        Message data size in bytes without alignment.
 *
 *              Failure:        zero
 *
 * Programmer:  Quincey Koziol
 *              Sep  6 2005
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O__cont_size(const H5F_t *f, hbool_t H5_ATTR_UNUSED disable_shared, const void H5_ATTR_UNUSED *_mesg)
{
    size_t ret_value = 0; /* Return value */

    FUNC_ENTER_STATIC_NOERR

    /* Set return value */
    ret_value = (size_t)(H5F_SIZEOF_ADDR(f) + /* Continuation header address */
                         H5F_SIZEOF_SIZE(f)); /* Continuation header length */

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cont_size() */

/*-------------------------------------------------------------------------
 * Function:	H5O__cont_free
 *
 * Purpose:	Frees the message
 *
 * Return:	Non-negative on success/Negative on failure
 *
 * Programmer:	Quincey Koziol
 *              Monday, November 15, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cont_free(void *mesg)
{
    FUNC_ENTER_STATIC_NOERR

    HDassert(mesg);

    mesg = H5FL_FREE(H5O_cont_t, mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__cont_free() */

/*-------------------------------------------------------------------------
 * Function:    H5O__cont_delete
 *
 * Purpose:     Free file space referenced by message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Quincey Koziol
 *              Monday, October 10, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cont_delete(H5F_t *f, H5O_t *open_oh, void *_mesg)
{
    H5O_cont_t *mesg      = (H5O_cont_t *)_mesg;
    herr_t      ret_value = SUCCEED; /* Return value */

    FUNC_ENTER_STATIC

    /* check args */
    HDassert(f);
    HDassert(mesg);

    /* Notify the cache that the chunk has been deleted */
    /* (releases the space for the chunk) */
    if (H5O__chunk_delete(f, open_oh, mesg->chunkno) < 0)
        HGOTO_ERROR(H5E_OHDR, H5E_CANTDELETE, FAIL, "unable to remove chunk from cache")

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O__cont_delete() */

/*-------------------------------------------------------------------------
 * Function:    H5O__cont_debug
 *
 * Purpose:     Prints debugging info.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Robb Matzke
 *              Aug  6 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__cont_debug(H5F_t H5_ATTR_UNUSED *f, const void *_mesg, FILE *stream, int indent, int fwidth)
{
    const H5O_cont_t *cont = (const H5O_cont_t *)_mesg;

    FUNC_ENTER_STATIC_NOERR

    /* check args */
    HDassert(f);
    HDassert(cont);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s %" PRIuHADDR "\n", indent, "", fwidth, "Continuation address:", cont->addr);

    HDfprintf(stream, "%*s%-*s %lu\n", indent, "", fwidth,
              "Continuation size in bytes:", (unsigned long)(cont->size));
    HDfprintf(stream, "%*s%-*s %d\n", indent, "", fwidth, "Points to chunk number:", (int)(cont->chunkno));

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__cont_debug() */

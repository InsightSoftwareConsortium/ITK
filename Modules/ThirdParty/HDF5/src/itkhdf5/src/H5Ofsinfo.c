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
 * Created:             H5Ofsinfo.c
 *                      Feb 2009
 *            Vailin Choi
 *
 * Purpose:             File space info message.
 *
 *-------------------------------------------------------------------------
 */
#define H5F_FRIEND              /*suppress error about including H5Fpkg   */
#include "H5Omodule.h"          /* This source code file is part of the H5O module */


#include "H5private.h"        /* Generic Functions    */
#include "H5Eprivate.h"        /* Error handling    */
#include "H5Fpkg.h"             /* File access          */
#include "H5FLprivate.h"    /* Free lists              */
#include "H5Opkg.h"             /* Object headers    */

/* PRIVATE PROTOTYPES */
static void *H5O_fsinfo_decode(H5F_t *f, H5O_t *open_oh, unsigned mesg_flags,
        unsigned *ioflags, size_t p_size, const uint8_t *p);
static herr_t H5O_fsinfo_encode(H5F_t *f, hbool_t disable_shared, uint8_t *p, const void *_mesg);
static void *H5O_fsinfo_copy(const void *_mesg, void *_dest);
static size_t H5O_fsinfo_size(const H5F_t *f, hbool_t disable_shared, const void *_mesg);
static herr_t H5O__fsinfo_free(void *mesg);
static herr_t H5O__fsinfo_debug(H5F_t *f, const void *_mesg, FILE * stream,
    int indent, int fwidth);

/* This message derives from H5O message class */
const H5O_msg_class_t H5O_MSG_FSINFO[1] = {{
    H5O_FSINFO_ID,                /* message id number                 */
    "fsinfo",                     /* message name for debugging        */
    sizeof(H5O_fsinfo_t),         /* native message size               */
    0,                /* messages are sharable?            */
    H5O_fsinfo_decode,            /* decode message                    */
    H5O_fsinfo_encode,            /* encode message                    */
    H5O_fsinfo_copy,              /* copy the native value             */
    H5O_fsinfo_size,              /* size of free-space manager info message */
    NULL,                       /* default reset method             */
    H5O__fsinfo_free,            /* free method                */
    NULL,                /* file delete method            */
    NULL,            /* link method                */
    NULL,            /* set share method            */
    NULL,                /* can share method            */
    NULL,            /* pre copy native value to file     */
    NULL,            /* copy native value to file        */
    NULL,            /* post copy native value to file    */
    NULL,            /* get creation index            */
    NULL,            /* set creation index            */
    H5O__fsinfo_debug              /* debug the message                */
}};

/* Declare a free list to manage the H5O_fsinfo_t struct */
H5FL_DEFINE_STATIC(H5O_fsinfo_t);


/*-------------------------------------------------------------------------
 * Function:    H5O_fsinfo_decode
 *
 * Purpose:     Decode a message and return a pointer to a newly allocated one.
 *
 * Return:      Success:        Ptr to new message in native form.
 *              Failure:        NULL
 *
 * Programmer:  Vailin Choi; Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_fsinfo_decode(H5F_t *f, H5O_t H5_ATTR_UNUSED *open_oh,
    unsigned H5_ATTR_UNUSED mesg_flags, unsigned H5_ATTR_UNUSED *ioflags,
    size_t H5_ATTR_UNUSED p_size, const uint8_t *p)
{
    H5O_fsinfo_t    *fsinfo = NULL;     /* File space info message */
    H5F_mem_page_t  ptype;              /* Memory type for iteration */
    unsigned        vers;               /* message version */
    void            *ret_value = NULL;  /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(f);
    HDassert(p);

    /* Allocate space for message */
    if(NULL == (fsinfo = H5FL_CALLOC(H5O_fsinfo_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    for(ptype = H5F_MEM_PAGE_SUPER; ptype < H5F_MEM_PAGE_NTYPES; H5_INC_ENUM(H5F_mem_page_t, ptype))
        fsinfo->fs_addr[ptype - 1] = HADDR_UNDEF;

    /* Version of message */
    vers = *p++;

    if(vers == H5O_FSINFO_VERSION_0) {
        H5F_file_space_type_t strategy;     /* Strategy */
        hsize_t threshold;                  /* Threshold */
        H5FD_mem_t type;                    /* Memory type for iteration */

        fsinfo->persist = H5F_FREE_SPACE_PERSIST_DEF;
        fsinfo->threshold = H5F_FREE_SPACE_THRESHOLD_DEF;
        fsinfo->page_size = H5F_FILE_SPACE_PAGE_SIZE_DEF;
        fsinfo->pgend_meta_thres = H5F_FILE_SPACE_PGEND_META_THRES;
        fsinfo->eoa_pre_fsm_fsalloc = HADDR_UNDEF;

        strategy = (H5F_file_space_type_t)*p++; /* File space strategy */
        H5F_DECODE_LENGTH(f, p, threshold);     /* Free-space section threshold */

        /* Map version 0 (deprecated) to version 1 message */
        switch(strategy) {

            case H5F_FILE_SPACE_ALL_PERSIST:
                fsinfo->strategy = H5F_FSPACE_STRATEGY_FSM_AGGR;
                fsinfo->persist = TRUE;
                fsinfo->threshold = threshold;
                if(HADDR_UNDEF == (fsinfo->eoa_pre_fsm_fsalloc = H5F_get_eoa(f, H5FD_MEM_DEFAULT)) )
                    HGOTO_ERROR(H5E_FILE, H5E_CANTGET, NULL, "unable to get file size")
                for(type = H5FD_MEM_SUPER; type < H5FD_MEM_NTYPES; H5_INC_ENUM(H5FD_mem_t, type))
                    H5F_addr_decode(f, &p, &(fsinfo->fs_addr[type-1]));
                break;

            case H5F_FILE_SPACE_ALL:
                fsinfo->strategy = H5F_FSPACE_STRATEGY_FSM_AGGR;
                fsinfo->threshold = threshold;
                break;

            case H5F_FILE_SPACE_AGGR_VFD:
                fsinfo->strategy = H5F_FSPACE_STRATEGY_AGGR;
                break;

            case H5F_FILE_SPACE_VFD:
                fsinfo->strategy = H5F_FSPACE_STRATEGY_NONE;
                break;

            case H5F_FILE_SPACE_NTYPES:
            case H5F_FILE_SPACE_DEFAULT:
            default:
                HGOTO_ERROR(H5E_ARGS, H5E_BADVALUE, NULL, "invalid file space strategy")
        } /* end switch */

        fsinfo->version = H5O_FSINFO_VERSION_1;
        fsinfo->mapped = TRUE;

    } else {
        HDassert(vers >= H5O_FSINFO_VERSION_1);

        fsinfo->version = vers;
        fsinfo->strategy = (H5F_fspace_strategy_t)*p++; /* File space strategy */
        fsinfo->persist = *p++;                         /* Free-space persist or not */
        H5F_DECODE_LENGTH(f, p, fsinfo->threshold);     /* Free-space section threshold */

        H5F_DECODE_LENGTH(f, p, fsinfo->page_size); /* File space page size */
        UINT16DECODE(p, fsinfo->pgend_meta_thres);  /* Page end metdata threshold */
        H5F_addr_decode(f, &p, &(fsinfo->eoa_pre_fsm_fsalloc)); /* EOA before free-space header and section info */

        /* Decode addresses of free space managers, if persisting */
        if(fsinfo->persist) {
            for(ptype = H5F_MEM_PAGE_SUPER; ptype < H5F_MEM_PAGE_NTYPES; H5_INC_ENUM(H5F_mem_page_t, ptype))
                H5F_addr_decode(f, &p, &(fsinfo->fs_addr[ptype - 1]));
        } /* end if */

        fsinfo->mapped = FALSE;
    }

    /* Set return value */
    ret_value = fsinfo;

done:
    if(ret_value == NULL && fsinfo != NULL)
        fsinfo = H5FL_FREE(H5O_fsinfo_t, fsinfo);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_fsinfo_decode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_fsinfo_encode
 *
 * Purpose:     Encodes a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi; Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O_fsinfo_encode(H5F_t *f, hbool_t H5_ATTR_UNUSED disable_shared, uint8_t *p, const void *_mesg)
{
    const H5O_fsinfo_t  *fsinfo = (const H5O_fsinfo_t *)_mesg;
    H5F_mem_page_t     ptype;  /* Memory type for iteration */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    /* check args */
    HDassert(f);
    HDassert(p);
    HDassert(fsinfo);

    *p++ = (uint8_t)fsinfo->version;    /* message version */
    *p++ = fsinfo->strategy;        /* File space strategy */
    *p++ = (unsigned char)fsinfo->persist;    /* Free-space persist or not */
    H5F_ENCODE_LENGTH(f, p, fsinfo->threshold); /* Free-space section size threshold */

    H5F_ENCODE_LENGTH(f, p, fsinfo->page_size);    /* File space page size */
    UINT16ENCODE(p, fsinfo->pgend_meta_thres);    /* Page end metadata threshold */
    H5F_addr_encode(f, &p, fsinfo->eoa_pre_fsm_fsalloc);    /* EOA before free-space header and section info */

    /* Store addresses of free-space managers, if persisting */
    if(fsinfo->persist) {
        /* Addresses of free-space managers */
        for(ptype = H5F_MEM_PAGE_SUPER; ptype < H5F_MEM_PAGE_NTYPES; H5_INC_ENUM(H5F_mem_page_t, ptype))
            H5F_addr_encode(f, &p, fsinfo->fs_addr[ptype - 1]);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O_fsinfo_encode() */


/*-------------------------------------------------------------------------
 * Function:    H5O_fsinfo_copy
 *
 * Purpose:     Copies a message from _MESG to _DEST, allocating _DEST if
 *              necessary.
 *
 * Return:      Success:        Ptr to _DEST
 *              Failure:        NULL
 *
 * Programmer:  Vailin Choi; Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static void *
H5O_fsinfo_copy(const void *_mesg, void *_dest)
{
    const H5O_fsinfo_t  *fsinfo = (const H5O_fsinfo_t *)_mesg;
    H5O_fsinfo_t        *dest = (H5O_fsinfo_t *) _dest;
    void                *ret_value = NULL;      /* Return value */

    FUNC_ENTER_NOAPI_NOINIT

    /* check args */
    HDassert(fsinfo);
    if(!dest && NULL == (dest = H5FL_CALLOC(H5O_fsinfo_t)))
        HGOTO_ERROR(H5E_RESOURCE, H5E_NOSPACE, NULL, "memory allocation failed")

    /* copy */
    *dest = *fsinfo;

    /* Set return value */
    ret_value = dest;

done:
    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_fsinfo_copy() */


/*-------------------------------------------------------------------------
 * Function:    H5O_fsinfo_size
 *
 * Purpose:     Returns the size of the raw message in bytes not counting
 *              the message type or size fields, but only the data fields.
 *              This function doesn't take into account alignment.
 *
 * Return:      Success:        Message data size in bytes without alignment.
 *              Failure:        zero
 *
 * Programmer:  Vailin Choi; Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static size_t
H5O_fsinfo_size(const H5F_t *f, hbool_t H5_ATTR_UNUSED disable_shared, const void *_mesg)
{
    const H5O_fsinfo_t   *fsinfo = (const H5O_fsinfo_t *)_mesg;
    size_t ret_value = 0;       /* Return value */

    FUNC_ENTER_NOAPI_NOINIT_NOERR

    ret_value = 3                       /* Version, strategy & persist */
        + (size_t)H5F_SIZEOF_SIZE(f)    /* Free-space section threshold */
        + (size_t)H5F_SIZEOF_SIZE(f)    /* File space page size */
        + 2                             /* Page end meta threshold */
        + (size_t)H5F_SIZEOF_ADDR(f);

    /* Free-space manager addresses */
    if(fsinfo->persist)
        ret_value += (H5F_MEM_PAGE_NTYPES - 1) * (size_t)H5F_SIZEOF_ADDR(f);

    FUNC_LEAVE_NOAPI(ret_value)
} /* end H5O_fsinfo_size() */


/*-------------------------------------------------------------------------
 * Function:    H5O__fsinfo_free
 *
 * Purpose:     Frees the message
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi; Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__fsinfo_free(void *mesg)
{
    FUNC_ENTER_STATIC_NOERR

    HDassert(mesg);

    mesg = H5FL_FREE(H5O_fsinfo_t, mesg);

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__fsinfo_free() */


/*-------------------------------------------------------------------------
 * Function:    H5O__fsinfo_debug
 *
 * Purpose:     Prints debugging info for a message.
 *
 * Return:      Non-negative on success/Negative on failure
 *
 * Programmer:  Vailin Choi; Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
H5O__fsinfo_debug(H5F_t H5_ATTR_UNUSED *f, const void *_mesg, FILE * stream,
    int indent, int fwidth)
{
    const H5O_fsinfo_t    *fsinfo = (const H5O_fsinfo_t *) _mesg;
    H5F_mem_page_t  ptype;      /* Free-space types for iteration */

    FUNC_ENTER_STATIC_NOERR

    /* check args */
    HDassert(f);
    HDassert(fsinfo);
    HDassert(stream);
    HDassert(indent >= 0);
    HDassert(fwidth >= 0);

    HDfprintf(stream, "%*s%-*s ", indent, "", fwidth, "File space strategy:");
    switch(fsinfo->strategy) {
        case H5F_FSPACE_STRATEGY_FSM_AGGR:
            HDfprintf(stream, "%s\n", "H5F_FSPACE_STRATEGY_FSM_AGGR");
            break;

        case H5F_FSPACE_STRATEGY_PAGE:
            HDfprintf(stream, "%s\n", "H5F_FSPACE_STRATEGY_PAGE");
            break;

        case H5F_FSPACE_STRATEGY_AGGR:
            HDfprintf(stream, "%s\n", "H5F_FSPACE_STRATEGY_AGGR");
            break;

        case H5F_FSPACE_STRATEGY_NONE:
            HDfprintf(stream, "%s\n", "H5F_FSPACE_STRATEGY_NONE");
            break;

        case H5F_FSPACE_STRATEGY_NTYPES:
        default:
            HDfprintf(stream, "%s\n", "unknown");
    } /* end switch */

    HDfprintf(stream, "%*s%-*s %t\n", indent, "", fwidth,
              "Free-space persist:", fsinfo->persist);

    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
              "Free-space section threshold:", fsinfo->threshold);

    HDfprintf(stream, "%*s%-*s %Hu\n", indent, "", fwidth,
              "File space page size:", fsinfo->page_size);

    HDfprintf(stream, "%*s%-*s %u\n", indent, "", fwidth,
              "Page end metadata threshold:", fsinfo->pgend_meta_thres);

    HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
              "eoa_pre_fsm_fsalloc:", fsinfo->eoa_pre_fsm_fsalloc);

    if(fsinfo->persist) {
        for(ptype = H5F_MEM_PAGE_SUPER; ptype < H5F_MEM_PAGE_NTYPES; H5_INC_ENUM(H5F_mem_page_t, ptype))
            HDfprintf(stream, "%*s%-*s %a\n", indent, "", fwidth,
                "Free space manager address:", fsinfo->fs_addr[ptype-1]);
    } /* end if */

    FUNC_LEAVE_NOAPI(SUCCEED)
} /* end H5O__fsinfo_debug() */


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
 * This file contains private information about the H5R module
 */
#ifndef _H5Rprivate_H
#define _H5Rprivate_H

#include "H5Rpublic.h"

/* Private headers needed by this file */
#include "H5Fprivate.h"         /* Files                                    */
#include "H5Gprivate.h"         /* Groups                                   */
#include "H5Oprivate.h"         /* Object headers                           */
#include "H5Sprivate.h"         /* Dataspaces                               */


/**************************/
/* Library Private Macros */
/**************************/


/****************************/
/* Library Private Typedefs */
/****************************/


/*****************************/
/* Library Private Variables */
/*****************************/


/******************************/
/* Library Private Prototypes */
/******************************/

H5_DLL herr_t H5R_create(void *ref, H5G_loc_t *loc, const char *name,
    H5R_type_t ref_type, H5S_t *space, hid_t dxpl_id);
H5_DLL H5S_t * H5R_get_region(H5F_t *file, hid_t dxpl_id, const void *_ref);
H5_DLL ssize_t H5R_get_name(H5F_t *file, hid_t lapl_id, hid_t dxpl_id, hid_t id,
    H5R_type_t ref_type, const void *_ref, char *name, size_t size);
H5_DLL herr_t H5R_get_obj_type(H5F_t *file, hid_t dxpl_id, H5R_type_t ref_type,
    const void *_ref, H5O_type_t *obj_type);
H5_DLL hid_t H5R_dereference(H5F_t *file, hid_t dapl_id, hid_t dxpl_id, H5R_type_t ref_type,
    const void *_ref, hbool_t app_ref);

#endif  /* _H5Rprivate_H */


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
 * Created:		H5MPprivate.h
 *			May  2 2005
 *			Quincey Koziol
 *
 * Purpose:		Private header for memory pool routines.
 *
 *-------------------------------------------------------------------------
 */

#ifndef H5MPprivate_H
#define H5MPprivate_H

/* Include package's public header (not yet) */
/* #include "H5MPpublic.h" */

/* Private headers needed by this file */

/**************************/
/* Library Private Macros */
/**************************/

/* Pool creation flags */
/* Default settings */
#define H5MP_FLG_DEFAULT       0
#define H5MP_PAGE_SIZE_DEFAULT 4096 /* (bytes) */

/****************************/
/* Library Private Typedefs */
/****************************/

/* Memory pool header (defined in H5MPpkg.c) */
typedef struct H5MP_pool_t H5MP_pool_t;

/***************************************/
/* Library-private Function Prototypes */
/***************************************/
H5_DLL H5MP_pool_t *H5MP_create(size_t page_size, unsigned flags);
H5_DLL void *       H5MP_malloc(H5MP_pool_t *mp, size_t request);
H5_DLL void *       H5MP_free(H5MP_pool_t *mp, void *spc);
H5_DLL herr_t       H5MP_close(H5MP_pool_t *mp);

#endif /* H5MPprivate_H */

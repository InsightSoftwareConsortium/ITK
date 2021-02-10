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

/* Generated automatically by bin/make_vers -- do not edit */
/* Add new versioned symbols to H5vers.txt file */


#ifndef _H5version_H
#define _H5version_H

/* Issue error if contradicting macros have been defined. */
#if (defined(H5_USE_16_API) || defined(H5_USE_18_API) || defined(H5_USE_110_API)) && defined(H5_NO_DEPRECATED_SYMBOLS)
  #error "Can't choose old API versions when deprecated APIs are disabled"
#endif /* (defined(H5_USE_16_API) || defined(H5_USE_18_API) || defined(H5_USE_110_API)) && defined(H5_NO_DEPRECATED_SYMBOLS) */


/* If a particular "global" version of the library's interfaces is chosen,
 *      set the versions for the API symbols affected.
 *
 * Note: If an application has already chosen a particular version for an
 *      API symbol, the individual API version macro takes priority.
 */
#if defined(H5_USE_16_API_DEFAULT) && !defined(H5_USE_16_API)
  #define H5_USE_16_API 1
#endif /* H5_USE_16_API_DEFAULT && !H5_USE_16_API */

#if defined(H5_USE_18_API_DEFAULT) && !defined(H5_USE_18_API)
  #define H5_USE_18_API 1
#endif /* H5_USE_18_API_DEFAULT && !H5_USE_18_API */

#if defined(H5_USE_110_API_DEFAULT) && !defined(H5_USE_110_API)
  #define H5_USE_110_API 1
#endif /* H5_USE_110_API_DEFAULT && !H5_USE_110_API */


#ifdef H5_USE_16_API

/*************/
/* Functions */
/*************/

#if !defined(H5Acreate_vers)
  #define H5Acreate_vers 1
#endif /* !defined(H5Acreate_vers) */

#if !defined(H5Aiterate_vers)
  #define H5Aiterate_vers 1
#endif /* !defined(H5Aiterate_vers) */

#if !defined(H5Dcreate_vers)
  #define H5Dcreate_vers 1
#endif /* !defined(H5Dcreate_vers) */

#if !defined(H5Dopen_vers)
  #define H5Dopen_vers 1
#endif /* !defined(H5Dopen_vers) */

#if !defined(H5Eclear_vers)
  #define H5Eclear_vers 1
#endif /* !defined(H5Eclear_vers) */

#if !defined(H5Eget_auto_vers)
  #define H5Eget_auto_vers 1
#endif /* !defined(H5Eget_auto_vers) */

#if !defined(H5Eprint_vers)
  #define H5Eprint_vers 1
#endif /* !defined(H5Eprint_vers) */

#if !defined(H5Epush_vers)
  #define H5Epush_vers 1
#endif /* !defined(H5Epush_vers) */

#if !defined(H5Eset_auto_vers)
  #define H5Eset_auto_vers 1
#endif /* !defined(H5Eset_auto_vers) */

#if !defined(H5Ewalk_vers)
  #define H5Ewalk_vers 1
#endif /* !defined(H5Ewalk_vers) */

#if !defined(H5Gcreate_vers)
  #define H5Gcreate_vers 1
#endif /* !defined(H5Gcreate_vers) */

#if !defined(H5Gopen_vers)
  #define H5Gopen_vers 1
#endif /* !defined(H5Gopen_vers) */

#if !defined(H5Pget_filter_vers)
  #define H5Pget_filter_vers 1
#endif /* !defined(H5Pget_filter_vers) */

#if !defined(H5Pget_filter_by_id_vers)
  #define H5Pget_filter_by_id_vers 1
#endif /* !defined(H5Pget_filter_by_id_vers) */

#if !defined(H5Pinsert_vers)
  #define H5Pinsert_vers 1
#endif /* !defined(H5Pinsert_vers) */

#if !defined(H5Pregister_vers)
  #define H5Pregister_vers 1
#endif /* !defined(H5Pregister_vers) */

#if !defined(H5Rdereference_vers)
  #define H5Rdereference_vers 1
#endif /* !defined(H5Rdereference_vers) */

#if !defined(H5Rget_obj_type_vers)
  #define H5Rget_obj_type_vers 1
#endif /* !defined(H5Rget_obj_type_vers) */

#if !defined(H5Tarray_create_vers)
  #define H5Tarray_create_vers 1
#endif /* !defined(H5Tarray_create_vers) */

#if !defined(H5Tcommit_vers)
  #define H5Tcommit_vers 1
#endif /* !defined(H5Tcommit_vers) */

#if !defined(H5Tget_array_dims_vers)
  #define H5Tget_array_dims_vers 1
#endif /* !defined(H5Tget_array_dims_vers) */

#if !defined(H5Topen_vers)
  #define H5Topen_vers 1
#endif /* !defined(H5Topen_vers) */

/************/
/* Typedefs */
/************/

#if !defined(H5E_auto_t_vers)
  #define H5E_auto_t_vers 1
#endif /* !defined(H5E_auto_t_vers) */

#if !defined(H5Z_class_t_vers)
  #define H5Z_class_t_vers 1
#endif /* !defined(H5Z_class_t_vers) */

#endif /* H5_USE_16_API */

#ifdef H5_USE_18_API

/*************/
/* Functions */
/*************/

#if !defined(H5Acreate_vers)
  #define H5Acreate_vers 2
#endif /* !defined(H5Acreate_vers) */

#if !defined(H5Aiterate_vers)
  #define H5Aiterate_vers 2
#endif /* !defined(H5Aiterate_vers) */

#if !defined(H5Dcreate_vers)
  #define H5Dcreate_vers 2
#endif /* !defined(H5Dcreate_vers) */

#if !defined(H5Dopen_vers)
  #define H5Dopen_vers 2
#endif /* !defined(H5Dopen_vers) */

#if !defined(H5Eclear_vers)
  #define H5Eclear_vers 2
#endif /* !defined(H5Eclear_vers) */

#if !defined(H5Eget_auto_vers)
  #define H5Eget_auto_vers 2
#endif /* !defined(H5Eget_auto_vers) */

#if !defined(H5Eprint_vers)
  #define H5Eprint_vers 2
#endif /* !defined(H5Eprint_vers) */

#if !defined(H5Epush_vers)
  #define H5Epush_vers 2
#endif /* !defined(H5Epush_vers) */

#if !defined(H5Eset_auto_vers)
  #define H5Eset_auto_vers 2
#endif /* !defined(H5Eset_auto_vers) */

#if !defined(H5Ewalk_vers)
  #define H5Ewalk_vers 2
#endif /* !defined(H5Ewalk_vers) */

#if !defined(H5Fget_info_vers)
  #define H5Fget_info_vers 1
#endif /* !defined(H5Fget_info_vers) */

#if !defined(H5Gcreate_vers)
  #define H5Gcreate_vers 2
#endif /* !defined(H5Gcreate_vers) */

#if !defined(H5Gopen_vers)
  #define H5Gopen_vers 2
#endif /* !defined(H5Gopen_vers) */

#if !defined(H5Pget_filter_vers)
  #define H5Pget_filter_vers 2
#endif /* !defined(H5Pget_filter_vers) */

#if !defined(H5Pget_filter_by_id_vers)
  #define H5Pget_filter_by_id_vers 2
#endif /* !defined(H5Pget_filter_by_id_vers) */

#if !defined(H5Pinsert_vers)
  #define H5Pinsert_vers 2
#endif /* !defined(H5Pinsert_vers) */

#if !defined(H5Pregister_vers)
  #define H5Pregister_vers 2
#endif /* !defined(H5Pregister_vers) */

#if !defined(H5Rdereference_vers)
  #define H5Rdereference_vers 1
#endif /* !defined(H5Rdereference_vers) */

#if !defined(H5Rget_obj_type_vers)
  #define H5Rget_obj_type_vers 2
#endif /* !defined(H5Rget_obj_type_vers) */

#if !defined(H5Tarray_create_vers)
  #define H5Tarray_create_vers 2
#endif /* !defined(H5Tarray_create_vers) */

#if !defined(H5Tcommit_vers)
  #define H5Tcommit_vers 2
#endif /* !defined(H5Tcommit_vers) */

#if !defined(H5Tget_array_dims_vers)
  #define H5Tget_array_dims_vers 2
#endif /* !defined(H5Tget_array_dims_vers) */

#if !defined(H5Topen_vers)
  #define H5Topen_vers 2
#endif /* !defined(H5Topen_vers) */

/************/
/* Typedefs */
/************/

#if !defined(H5E_auto_t_vers)
  #define H5E_auto_t_vers 2
#endif /* !defined(H5E_auto_t_vers) */

#if !defined(H5Z_class_t_vers)
  #define H5Z_class_t_vers 2
#endif /* !defined(H5Z_class_t_vers) */

#endif /* H5_USE_18_API */

#ifdef H5_USE_110_API

/*************/
/* Functions */
/*************/

#if !defined(H5Acreate_vers)
  #define H5Acreate_vers 2
#endif /* !defined(H5Acreate_vers) */

#if !defined(H5Aiterate_vers)
  #define H5Aiterate_vers 2
#endif /* !defined(H5Aiterate_vers) */

#if !defined(H5Dcreate_vers)
  #define H5Dcreate_vers 2
#endif /* !defined(H5Dcreate_vers) */

#if !defined(H5Dopen_vers)
  #define H5Dopen_vers 2
#endif /* !defined(H5Dopen_vers) */

#if !defined(H5Eclear_vers)
  #define H5Eclear_vers 2
#endif /* !defined(H5Eclear_vers) */

#if !defined(H5Eget_auto_vers)
  #define H5Eget_auto_vers 2
#endif /* !defined(H5Eget_auto_vers) */

#if !defined(H5Eprint_vers)
  #define H5Eprint_vers 2
#endif /* !defined(H5Eprint_vers) */

#if !defined(H5Epush_vers)
  #define H5Epush_vers 2
#endif /* !defined(H5Epush_vers) */

#if !defined(H5Eset_auto_vers)
  #define H5Eset_auto_vers 2
#endif /* !defined(H5Eset_auto_vers) */

#if !defined(H5Ewalk_vers)
  #define H5Ewalk_vers 2
#endif /* !defined(H5Ewalk_vers) */

#if !defined(H5Fget_info_vers)
  #define H5Fget_info_vers 2
#endif /* !defined(H5Fget_info_vers) */

#if !defined(H5Gcreate_vers)
  #define H5Gcreate_vers 2
#endif /* !defined(H5Gcreate_vers) */

#if !defined(H5Gopen_vers)
  #define H5Gopen_vers 2
#endif /* !defined(H5Gopen_vers) */

#if !defined(H5Pget_filter_vers)
  #define H5Pget_filter_vers 2
#endif /* !defined(H5Pget_filter_vers) */

#if !defined(H5Pget_filter_by_id_vers)
  #define H5Pget_filter_by_id_vers 2
#endif /* !defined(H5Pget_filter_by_id_vers) */

#if !defined(H5Pinsert_vers)
  #define H5Pinsert_vers 2
#endif /* !defined(H5Pinsert_vers) */

#if !defined(H5Pregister_vers)
  #define H5Pregister_vers 2
#endif /* !defined(H5Pregister_vers) */

#if !defined(H5Rdereference_vers)
  #define H5Rdereference_vers 2
#endif /* !defined(H5Rdereference_vers) */

#if !defined(H5Rget_obj_type_vers)
  #define H5Rget_obj_type_vers 2
#endif /* !defined(H5Rget_obj_type_vers) */

#if !defined(H5Tarray_create_vers)
  #define H5Tarray_create_vers 2
#endif /* !defined(H5Tarray_create_vers) */

#if !defined(H5Tcommit_vers)
  #define H5Tcommit_vers 2
#endif /* !defined(H5Tcommit_vers) */

#if !defined(H5Tget_array_dims_vers)
  #define H5Tget_array_dims_vers 2
#endif /* !defined(H5Tget_array_dims_vers) */

#if !defined(H5Topen_vers)
  #define H5Topen_vers 2
#endif /* !defined(H5Topen_vers) */

/************/
/* Typedefs */
/************/

#if !defined(H5E_auto_t_vers)
  #define H5E_auto_t_vers 2
#endif /* !defined(H5E_auto_t_vers) */

#if !defined(H5Z_class_t_vers)
  #define H5Z_class_t_vers 2
#endif /* !defined(H5Z_class_t_vers) */

#endif /* H5_USE_110_API */


/* Choose the correct version of each API symbol, defaulting to the latest
 *      version of each.  The "best" name for API parameters/data structures
 *      that have changed definitions is also set.  An error is issued for
 *      specifying an invalid API version.
 */

/*************/
/* Functions */
/*************/

#if !defined(H5Acreate_vers) || H5Acreate_vers == 2
  #ifndef H5Acreate_vers
    #define H5Acreate_vers 2
  #endif /* H5Acreate_vers */
  #define H5Acreate H5Acreate2
#elif H5Acreate_vers == 1
  #define H5Acreate H5Acreate1
#else /* H5Acreate_vers */
  #error "H5Acreate_vers set to invalid value"
#endif /* H5Acreate_vers */

#if !defined(H5Aiterate_vers) || H5Aiterate_vers == 2
  #ifndef H5Aiterate_vers
    #define H5Aiterate_vers 2
  #endif /* H5Aiterate_vers */
  #define H5Aiterate H5Aiterate2
  #define H5A_operator_t H5A_operator2_t
#elif H5Aiterate_vers == 1
  #define H5Aiterate H5Aiterate1
  #define H5A_operator_t H5A_operator1_t
#else /* H5Aiterate_vers */
  #error "H5Aiterate_vers set to invalid value"
#endif /* H5Aiterate_vers */

#if !defined(H5Dcreate_vers) || H5Dcreate_vers == 2
  #ifndef H5Dcreate_vers
    #define H5Dcreate_vers 2
  #endif /* H5Dcreate_vers */
  #define H5Dcreate H5Dcreate2
#elif H5Dcreate_vers == 1
  #define H5Dcreate H5Dcreate1
#else /* H5Dcreate_vers */
  #error "H5Dcreate_vers set to invalid value"
#endif /* H5Dcreate_vers */

#if !defined(H5Dopen_vers) || H5Dopen_vers == 2
  #ifndef H5Dopen_vers
    #define H5Dopen_vers 2
  #endif /* H5Dopen_vers */
  #define H5Dopen H5Dopen2
#elif H5Dopen_vers == 1
  #define H5Dopen H5Dopen1
#else /* H5Dopen_vers */
  #error "H5Dopen_vers set to invalid value"
#endif /* H5Dopen_vers */

#if !defined(H5Eclear_vers) || H5Eclear_vers == 2
  #ifndef H5Eclear_vers
    #define H5Eclear_vers 2
  #endif /* H5Eclear_vers */
  #define H5Eclear H5Eclear2
#elif H5Eclear_vers == 1
  #define H5Eclear H5Eclear1
#else /* H5Eclear_vers */
  #error "H5Eclear_vers set to invalid value"
#endif /* H5Eclear_vers */

#if !defined(H5Eget_auto_vers) || H5Eget_auto_vers == 2
  #ifndef H5Eget_auto_vers
    #define H5Eget_auto_vers 2
  #endif /* H5Eget_auto_vers */
  #define H5Eget_auto H5Eget_auto2
#elif H5Eget_auto_vers == 1
  #define H5Eget_auto H5Eget_auto1
#else /* H5Eget_auto_vers */
  #error "H5Eget_auto_vers set to invalid value"
#endif /* H5Eget_auto_vers */

#if !defined(H5Eprint_vers) || H5Eprint_vers == 2
  #ifndef H5Eprint_vers
    #define H5Eprint_vers 2
  #endif /* H5Eprint_vers */
  #define H5Eprint H5Eprint2
#elif H5Eprint_vers == 1
  #define H5Eprint H5Eprint1
#else /* H5Eprint_vers */
  #error "H5Eprint_vers set to invalid value"
#endif /* H5Eprint_vers */

#if !defined(H5Epush_vers) || H5Epush_vers == 2
  #ifndef H5Epush_vers
    #define H5Epush_vers 2
  #endif /* H5Epush_vers */
  #define H5Epush H5Epush2
#elif H5Epush_vers == 1
  #define H5Epush H5Epush1
#else /* H5Epush_vers */
  #error "H5Epush_vers set to invalid value"
#endif /* H5Epush_vers */

#if !defined(H5Eset_auto_vers) || H5Eset_auto_vers == 2
  #ifndef H5Eset_auto_vers
    #define H5Eset_auto_vers 2
  #endif /* H5Eset_auto_vers */
  #define H5Eset_auto H5Eset_auto2
#elif H5Eset_auto_vers == 1
  #define H5Eset_auto H5Eset_auto1
#else /* H5Eset_auto_vers */
  #error "H5Eset_auto_vers set to invalid value"
#endif /* H5Eset_auto_vers */

#if !defined(H5Ewalk_vers) || H5Ewalk_vers == 2
  #ifndef H5Ewalk_vers
    #define H5Ewalk_vers 2
  #endif /* H5Ewalk_vers */
  #define H5Ewalk H5Ewalk2
  #define H5E_error_t H5E_error2_t
  #define H5E_walk_t H5E_walk2_t
#elif H5Ewalk_vers == 1
  #define H5Ewalk H5Ewalk1
  #define H5E_error_t H5E_error1_t
  #define H5E_walk_t H5E_walk1_t
#else /* H5Ewalk_vers */
  #error "H5Ewalk_vers set to invalid value"
#endif /* H5Ewalk_vers */

#if !defined(H5Fget_info_vers) || H5Fget_info_vers == 2
  #ifndef H5Fget_info_vers
    #define H5Fget_info_vers 2
  #endif /* H5Fget_info_vers */
  #define H5Fget_info H5Fget_info2
  #define H5F_info_t H5F_info2_t
#elif H5Fget_info_vers == 1
  #define H5Fget_info H5Fget_info1
  #define H5F_info_t H5F_info1_t
#else /* H5Fget_info_vers */
  #error "H5Fget_info_vers set to invalid value"
#endif /* H5Fget_info_vers */

#if !defined(H5Gcreate_vers) || H5Gcreate_vers == 2
  #ifndef H5Gcreate_vers
    #define H5Gcreate_vers 2
  #endif /* H5Gcreate_vers */
  #define H5Gcreate H5Gcreate2
#elif H5Gcreate_vers == 1
  #define H5Gcreate H5Gcreate1
#else /* H5Gcreate_vers */
  #error "H5Gcreate_vers set to invalid value"
#endif /* H5Gcreate_vers */

#if !defined(H5Gopen_vers) || H5Gopen_vers == 2
  #ifndef H5Gopen_vers
    #define H5Gopen_vers 2
  #endif /* H5Gopen_vers */
  #define H5Gopen H5Gopen2
#elif H5Gopen_vers == 1
  #define H5Gopen H5Gopen1
#else /* H5Gopen_vers */
  #error "H5Gopen_vers set to invalid value"
#endif /* H5Gopen_vers */

#if !defined(H5Pget_filter_vers) || H5Pget_filter_vers == 2
  #ifndef H5Pget_filter_vers
    #define H5Pget_filter_vers 2
  #endif /* H5Pget_filter_vers */
  #define H5Pget_filter H5Pget_filter2
#elif H5Pget_filter_vers == 1
  #define H5Pget_filter H5Pget_filter1
#else /* H5Pget_filter_vers */
  #error "H5Pget_filter_vers set to invalid value"
#endif /* H5Pget_filter_vers */

#if !defined(H5Pget_filter_by_id_vers) || H5Pget_filter_by_id_vers == 2
  #ifndef H5Pget_filter_by_id_vers
    #define H5Pget_filter_by_id_vers 2
  #endif /* H5Pget_filter_by_id_vers */
  #define H5Pget_filter_by_id H5Pget_filter_by_id2
#elif H5Pget_filter_by_id_vers == 1
  #define H5Pget_filter_by_id H5Pget_filter_by_id1
#else /* H5Pget_filter_by_id_vers */
  #error "H5Pget_filter_by_id_vers set to invalid value"
#endif /* H5Pget_filter_by_id_vers */

#if !defined(H5Pinsert_vers) || H5Pinsert_vers == 2
  #ifndef H5Pinsert_vers
    #define H5Pinsert_vers 2
  #endif /* H5Pinsert_vers */
  #define H5Pinsert H5Pinsert2
#elif H5Pinsert_vers == 1
  #define H5Pinsert H5Pinsert1
#else /* H5Pinsert_vers */
  #error "H5Pinsert_vers set to invalid value"
#endif /* H5Pinsert_vers */

#if !defined(H5Pregister_vers) || H5Pregister_vers == 2
  #ifndef H5Pregister_vers
    #define H5Pregister_vers 2
  #endif /* H5Pregister_vers */
  #define H5Pregister H5Pregister2
#elif H5Pregister_vers == 1
  #define H5Pregister H5Pregister1
#else /* H5Pregister_vers */
  #error "H5Pregister_vers set to invalid value"
#endif /* H5Pregister_vers */

#if !defined(H5Rdereference_vers) || H5Rdereference_vers == 2
  #ifndef H5Rdereference_vers
    #define H5Rdereference_vers 2
  #endif /* H5Rdereference_vers */
  #define H5Rdereference H5Rdereference2
#elif H5Rdereference_vers == 1
  #define H5Rdereference H5Rdereference1
#else /* H5Rdereference_vers */
  #error "H5Rdereference_vers set to invalid value"
#endif /* H5Rdereference_vers */

#if !defined(H5Rget_obj_type_vers) || H5Rget_obj_type_vers == 2
  #ifndef H5Rget_obj_type_vers
    #define H5Rget_obj_type_vers 2
  #endif /* H5Rget_obj_type_vers */
  #define H5Rget_obj_type H5Rget_obj_type2
#elif H5Rget_obj_type_vers == 1
  #define H5Rget_obj_type H5Rget_obj_type1
#else /* H5Rget_obj_type_vers */
  #error "H5Rget_obj_type_vers set to invalid value"
#endif /* H5Rget_obj_type_vers */

#if !defined(H5Tarray_create_vers) || H5Tarray_create_vers == 2
  #ifndef H5Tarray_create_vers
    #define H5Tarray_create_vers 2
  #endif /* H5Tarray_create_vers */
  #define H5Tarray_create H5Tarray_create2
#elif H5Tarray_create_vers == 1
  #define H5Tarray_create H5Tarray_create1
#else /* H5Tarray_create_vers */
  #error "H5Tarray_create_vers set to invalid value"
#endif /* H5Tarray_create_vers */

#if !defined(H5Tcommit_vers) || H5Tcommit_vers == 2
  #ifndef H5Tcommit_vers
    #define H5Tcommit_vers 2
  #endif /* H5Tcommit_vers */
  #define H5Tcommit H5Tcommit2
#elif H5Tcommit_vers == 1
  #define H5Tcommit H5Tcommit1
#else /* H5Tcommit_vers */
  #error "H5Tcommit_vers set to invalid value"
#endif /* H5Tcommit_vers */

#if !defined(H5Tget_array_dims_vers) || H5Tget_array_dims_vers == 2
  #ifndef H5Tget_array_dims_vers
    #define H5Tget_array_dims_vers 2
  #endif /* H5Tget_array_dims_vers */
  #define H5Tget_array_dims H5Tget_array_dims2
#elif H5Tget_array_dims_vers == 1
  #define H5Tget_array_dims H5Tget_array_dims1
#else /* H5Tget_array_dims_vers */
  #error "H5Tget_array_dims_vers set to invalid value"
#endif /* H5Tget_array_dims_vers */

#if !defined(H5Topen_vers) || H5Topen_vers == 2
  #ifndef H5Topen_vers
    #define H5Topen_vers 2
  #endif /* H5Topen_vers */
  #define H5Topen H5Topen2
#elif H5Topen_vers == 1
  #define H5Topen H5Topen1
#else /* H5Topen_vers */
  #error "H5Topen_vers set to invalid value"
#endif /* H5Topen_vers */

/************/
/* Typedefs */
/************/

#if !defined(H5E_auto_t_vers) || H5E_auto_t_vers == 2
  #ifndef H5E_auto_t_vers
    #define H5E_auto_t_vers 2
  #endif /* H5E_auto_t_vers */
  #define H5E_auto_t H5E_auto2_t
#elif H5E_auto_t_vers == 1
  #define H5E_auto_t H5E_auto1_t
#else /* H5E_auto_t_vers */
  #error "H5E_auto_t_vers set to invalid value"
#endif /* H5E_auto_t_vers */


#if !defined(H5Z_class_t_vers) || H5Z_class_t_vers == 2
  #ifndef H5Z_class_t_vers
    #define H5Z_class_t_vers 2
  #endif /* H5Z_class_t_vers */
  #define H5Z_class_t H5Z_class2_t
#elif H5Z_class_t_vers == 1
  #define H5Z_class_t H5Z_class1_t
#else /* H5Z_class_t_vers */
  #error "H5Z_class_t_vers set to invalid value"
#endif /* H5Z_class_t_vers */

#endif /* H5version_H */


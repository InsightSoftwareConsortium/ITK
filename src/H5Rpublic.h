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

/*
 * This file contains public declarations for the H5R module.
 */
#ifndef H5Rpublic_H
#define H5Rpublic_H

/* Public headers needed by this file */
#include "H5public.h"
#include "H5Gpublic.h"
#include "H5Ipublic.h"

/*****************/
/* Public Macros */
/*****************/

/* Note! Be careful with the sizes of the references because they should really
 * depend on the run-time values in the file.  Unfortunately, the arrays need
 * to be defined at compile-time, so we have to go with the worst case sizes
 * for them.  -QAK
 */
#define H5R_OBJ_REF_BUF_SIZE sizeof(haddr_t)

/* 4 is used instead of sizeof(int) to permit portability between the Crays
 * and other machines (the heap ID is always encoded as an int32 anyway).
 */
#define H5R_DSET_REG_REF_BUF_SIZE (sizeof(haddr_t) + 4)

/*******************/
/* Public Typedefs */
/*******************/

//! <!-- [H5R_type_t_snip] -->
/**
 * Reference types allowed.
 *
 * \internal DO NOT CHANGE THE ORDER or VALUES as reference type values are
 *           encoded into the datatype message header.
 */
typedef enum {
    H5R_BADTYPE = (-1), /**< Invalid reference type               */
    H5R_OBJECT,         /**< Object reference                     */
    H5R_DATASET_REGION, /**< Dataset Region Reference             */
    H5R_MAXTYPE         /**< Highest type (invalid)               */
} H5R_type_t;
//! <!-- [H5R_type_t_snip] -->

//! <!-- [hobj_ref_t_snip] -->
/**
 * Object reference structure for user's code
 * This needs to be large enough to store largest haddr_t on a worst case
 * machine (8 bytes currently).
 */
typedef haddr_t hobj_ref_t;
//! <!-- [hobj_ref_t_snip] -->

//! <!-- [hdset_reg_ref_t_snip] -->
/**
 * Dataset Region reference structure for user's code
 * (Buffer to store heap ID and index)
 * This needs to be large enough to store largest haddr_t in a worst case
 * machine (8 bytes currently) plus an int
 */
typedef unsigned char hdset_reg_ref_t[H5R_DSET_REG_REF_BUF_SIZE];
//! <!-- [hdset_reg_ref_t_snip] -->

/********************/
/* Public Variables */
/********************/

/*********************/
/* Public Prototypes */
/*********************/

#ifdef __cplusplus
extern "C" {
#endif

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Creates a reference
 *
 * \param[out] ref Reference created by the function call
 * \param[in] loc_id Location identifier used to locate the object being pointed to
 * \param[in] name Name of object at location \p loc_id
 * \param[in] ref_type Type of reference
 * \param[in] space_id Dataspace identifier with selection. Used only for
 *                     dataset region references; pass as -1 if reference is
 *                     an object reference, i.e., of type #H5R_OBJECT
 *
 * \return \herr_t
 *
 * \details H5Rcreate() creates the reference, \p ref, of the type specified in
 *          \p ref_type, pointing to the object \p name located at \p loc_id.
 *
 *          The HDF5 library maps the void type specified above for \p ref to
 *          the type specified in \p ref_type, which will be one of the following:
 *          \snippet this H5R_type_t_snip
 *
 *          The parameters \p loc_id and \p name are used to locate the object.
 *
 *          The parameter \p space_id identifies the dataset region that a
 *          dataset region reference points to. This parameter is used only with
 *          dataset region references and should be set to -1 if the reference
 *          is an object reference, #H5R_OBJECT.
 *
 * \since 1.8.0
 */
H5_DLL herr_t H5Rcreate(void *ref, hid_t loc_id, const char *name, H5R_type_t ref_type, hid_t space_id);
/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Opens the HDF5 object referenced
 *
 * \obj_id
 * \oapl_id
 * \param[in] ref_type The reference type of \p ref
 * \param[in] ref Reference to open
 *
 * \return Returns identifier of referenced object if successful; otherwise
 *         returns a negative value.
 *
 * \details Given a reference, \p ref, to an object or a region in an object,
 *          H5Rdereference2() opens that object and returns an identifier.
 *
 *          The parameter \p obj_id must be a valid identifier for the HDF5 file
 *          containing the referenced object or for any object in that HDF5
 *          file.
 *
 *          The parameter \p oapl_id is an object access property list
 *          identifier for the referenced object. The access property list must
 *          be of the same type as the object being referenced, that is a group,
 *          dataset, or datatype property list.
 *
 *          The parameter \p ref_type specifies the reference type of the
 *          reference \p ref. \p ref_type may contain either of the following
 *          values:
 *          - #H5R_OBJECT
 *          - #H5R_DATASET_REGION
 *
 *          The object opened with this function should be closed when it is no
 *          longer needed so that resource leaks will not develop. Use the
 *          appropriate close function such as H5Oclose() or H5Dclose() for
 *          datasets.
 *
 * \since 1.10.0
 *
 */
H5_DLL hid_t H5Rdereference2(hid_t obj_id, hid_t oapl_id, H5R_type_t ref_type, const void *ref);
/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Sets up a dataspace and selection as specified by a region reference
 *
 * \param[in] dataset File identifier or identifier for any object in the file
 *                    containing the referenced region
 * \param[in] ref_type Reference type of \p ref, which must be #H5R_DATASET_REGION
 * \param[in] ref Region reference to open
 *
 * \return Returns a valid dataspace identifier if successful; otherwise returns
 *         a negative value.
 *
 * \details H5Rget_region() creates a copy of the dataspace of the dataset
 *          pointed to by a region reference, \p ref, and defines a selection
 *          matching the selection pointed to by ref within the dataspace copy.
 *
 *          \p dataset is used to identify the file containing the referenced
 *          region; it can be a file identifier or an identifier for any object
 *          in the file.
 *
 *          The parameter \p ref_type specifies the reference type of \p ref and
 *          must contain the value #H5R_DATASET_REGION.
 *
 *          Use H5Sclose() to release the dataspace identifier returned by this
 *          function when the identifier is no longer needed.
 *
 */
H5_DLL hid_t H5Rget_region(hid_t dataset, H5R_type_t ref_type, const void *ref);
/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves the type of object that an object reference points to
 *
 * \param[in] id The dataset containing the reference object or the group
 *            containing that dataset
 * \param[in] ref_type Type of reference to query
 * \param[in] ref Reference to query
 * \param[out] obj_type Type of referenced object
 *
 * \return \herr_t
 *
 * \details Given an object reference, \p ref, H5Rget_obj_type2() returns the
 *          type of the referenced object in \p obj_type.
 *
 *          A \Emph{reference type} is the type of reference, either an object
 *          reference or a dataset region reference. An \Emph{object reference}
 *          points to an HDF5 object while a \Emph{dataset region reference}
 *          points to a defined region within a dataset.
 *
 *          The \Emph{referenced object} is the object the reference points
 *          to. The \Emph{referenced object type}, or the type of the referenced
 *          object, is the type of the object that the reference points to.
 *
 *          The location identifier, \p id, is the identifier for either the
 *          dataset containing the object reference or the group containing that
 *          dataset.
 *
 *          Valid reference types, to pass in as \p ref_type, include the
 *          following:
 *          \snippet this H5R_type_t_snip
 *
 *          If the application does not already know the object reference type,
 *          that can be determined with three preliminary calls:
 *
 *          \li Call H5Dget_type() on the dataset containing the reference to
 *              get a datatype identifier for the dataset’s datatype.
 *          \li Using that datatype identifier, H5Tget_class() returns a datatype
 *              class.\n If the datatype class is #H5T_REFERENCE, H5Tequal() can
 *              then be used to determine whether the reference’s datatype is
 *              #H5T_STD_REF_OBJ or #H5T_STD_REF_DSETREG:
 *              - If the datatype is #H5T_STD_REF_OBJ, the reference object type
 *                is #H5R_OBJECT.
 *              - If the datatype is #H5T_STD_REF_DSETREG, the reference object
 *                type is #H5R_DATASET_REGION.
 *
 *          When the function completes successfully, it returns one of the
 *          following valid object type values (defined in H5Opublic.h):
 *          \snippet H5Opublic.h H5O_type_t_snip
 *
 * \since 1.8.0
 *
 */
H5_DLL herr_t H5Rget_obj_type2(hid_t id, H5R_type_t ref_type, const void *_ref, H5O_type_t *obj_type);
/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves a name for a referenced object
 *
 * \param[in] loc_id Identifier for the file containing the reference or for
 *                   any object in that file
 * \param[in] ref_type Type of reference
 * \param[in] ref An object or dataset region reference
 * \param[out] name A buffer to place the name of the referenced object or
 *                  dataset region. If \c NULL, then this call will return the
 *                  size in bytes of the name.
 * \param[in] size The size of the \p name buffer. When the size is passed in,
 *                 the \c NULL terminator needs to be included.
 *
 * \return Returns the length of the name if successful, returning 0 (zero) if
 *         no name is associated with the identifier. Otherwise returns a
 *         negative value.
 *
 * \details H5Rget_name() retrieves a name for the object identified by \p ref.\n
 *          \p loc_id is used to identify the file containing the reference. It
 *          can be the file identifier for the file containing the reference or
 *          an identifier for any object in that file.
 *
 *          \ref H5R_type_t is the reference type of \p ref. Valid values
 *          include the following:
 *          \snippet this H5R_type_t_snip
 *
 *          \p ref is the reference for which the target object’s name is
 *          sought.
 *
 *          If \p ref is an object reference, \p name will be returned with a
 *          name for the referenced object. If \p ref is a dataset region
 *          reference, \p name will contain a name for the object containing the
 *          referenced region.
 *
 *          Up to \p size characters of the name are returned in \p name;
 *          additional characters, if any, are not returned to the user
 *          application.
 *
 *          If the length of the name, which determines the required value of \p
 *          size, is unknown, a preliminary H5Rget_name() call can be made. The
 *          return value of this call will be the size of the object name. That
 *          value can then be assigned to \p size for a second H5Rget_name()
 *          call, which will retrieve the actual name.
 *
 *          If there is no name associated with the object identifier or if the
 *          \p name is \c NULL, H5Rget_name() returns the size of the \p name
 *          buffer (the size does not include the \p NULL terminator).
 *
 *          Note that an object in an HDF5 file may have multiple paths if there
 *          are multiple links pointing to it. This function may return any one
 *          of these paths.
 *
 * \since 1.8.0
 */
H5_DLL ssize_t H5Rget_name(hid_t loc_id, H5R_type_t ref_type, const void *ref, char *name /*out*/,
                           size_t size);

/* Symbols defined for compatibility with previous versions of the HDF5 API.
 *
 * Use of these symbols is deprecated.
 */
#ifndef H5_NO_DEPRECATED_SYMBOLS

/* Function prototypes */
/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Retrieves the type of object that an object reference points to
 *
 * \param[in] id The dataset containing the reference object or the group
 *            containing that dataset
 * \param[in] ref_type Type of reference to query
 * \param[in] ref Reference to query
 *
 * \return Returns a valid object type if successful; otherwise returns a
 *         negative value (#H5G_UNKNOWN).
 *
 * \deprecated This function has been renamed from H5Rget_obj_type() and is
 *             deprecated in favor of the macro H5Rget_obj_type() or the
 *             function H5Rget_obj_type2().
 *
 * \details Given an object reference, \p ref, H5Rget_obj_type1() returns the
 *          type of the referenced object.
 *
 *          A \Emph{reference type} is the type of reference, either an object
 *          reference or a dataset region reference. An \Emph{object reference}
 *          points to an HDF5 object while a \Emph{dataset region reference}
 *          points to a defined region within a dataset.
 *
 *          The \Emph{referenced object} is the object the reference points
 *          to. The \Emph{referenced object type}, or the type of the referenced
 *          object, is the type of the object that the reference points to.
 *
 *          The location identifier, \p id, is the identifier for either the
 *          dataset containing the object reference or the group containing that
 *          dataset.
 *
 *          Valid reference types, to pass in as \p ref_type, include the
 *          following:
 *          \snippet this H5R_type_t_snip
 *
 *          If the application does not already know the object reference type,
 *          that can be determined with three preliminary calls:
 *
 *          \li Call H5Dget_type() on the dataset containing the reference to
 *              get a datatype identifier for the dataset’s datatype.
 *          \li Using that datatype identifier, H5Tget_class() returns a datatype
 *              class.\n If the datatype class is #H5T_REFERENCE, H5Tequal() can
 *              then be used to determine whether the reference’s datatype is
 *              #H5T_STD_REF_OBJ or #H5T_STD_REF_DSETREG:
 *              - If the datatype is #H5T_STD_REF_OBJ, the reference object type
 *                is #H5R_OBJECT.
 *              - If the datatype is #H5T_STD_REF_DSETREG, the reference object
 *                type is #H5R_DATASET_REGION.
 *
 *          When the function completes successfully, it returns one of the
 *          following valid object type values (defined in H5Gpublic.h):
 *          \snippet H5Gpublic.h H5G_obj_t_snip
 *
 * \version 1.8.0 Function H5Rget_obj_type() renamed to H5Rget_obj_type1() and
 *                deprecated in this release.
 * \since 1.6.0
 *
 */
H5_DLL H5G_obj_t H5Rget_obj_type1(hid_t id, H5R_type_t ref_type, const void *ref);

/**
 * --------------------------------------------------------------------------
 * \ingroup H5R
 *
 * \brief Opens the HDF5 object referenced
 *
 * \obj_id
 * \param[in] ref_type The reference type of \p ref
 * \param[in] ref Reference to open
 *
 * \return Returns identifier of referenced object if successful; otherwise
 *         returns a negative value.
 *
 * \deprecated This function has been renamed from H5Rdereference() and is
 *             deprecated in favor of the macro H5Rdereference() or the function
 *             H5Rdereference2().
 *
 * \details Given a reference, \p ref, to an object or a region in an object,
 *          H5Rdereference1() opens that object and returns an identifier.
 *
 *          The parameter \p obj_id must be a valid identifier for an object in
 *          the HDF5 file containing the referenced object, including the file
 *          identifier.
 *
 *          The parameter \p ref_type specifies the reference type of the
 *          reference \p ref. \p ref_type may contain either of the following
 *          values:
 *          - #H5R_OBJECT
 *          - #H5R_DATASET_REGION
 *
 *          The object opened with this function should be closed when it is no
 *          longer needed so that resource leaks will not develop. Use the
 *          appropriate close function such as H5Oclose() or H5Dclose() for
 *          datasets.
 *
 * \version 1.10.0 Function H5Rdereference() renamed to H5Rdereference1() and
 *                 deprecated in this release.
 * \since 1.8.0
 *
 */
H5_DLL hid_t H5Rdereference1(hid_t obj_id, H5R_type_t ref_type, const void *ref);

#endif /* H5_NO_DEPRECATED_SYMBOLS */

#ifdef __cplusplus
}
#endif

#endif /* H5Rpublic_H */

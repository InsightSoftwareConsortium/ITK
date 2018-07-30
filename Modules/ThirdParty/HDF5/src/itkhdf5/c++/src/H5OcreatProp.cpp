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

#include <string>

#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5OcreatProp.h"

namespace H5 {

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// This DOXYGEN_SHOULD_SKIP_THIS block is a work-around approach to control
// the order of creation and deletion of the global constants.  See Design Notes
// in "H5PredType.cpp" for information.

// Initialize a pointer for the constant
ObjCreatPropList* ObjCreatPropList::DEFAULT_ = 0;

//--------------------------------------------------------------------------
// Function:    ObjCreatPropList::getConstant
//              Creates a ObjCreatPropList object representing the HDF5 constant
//              H5P_FILE_ACCESS, pointed to by ObjCreatPropList::DEFAULT_
// exception    H5::PropListIException
// Description
//              If ObjCreatPropList::DEFAULT_ already points to an allocated
//              object, throw a PropListIException.  This scenario should not
//              happen.
// Programmer   Binh-Minh Ribler - 2015
//--------------------------------------------------------------------------
ObjCreatPropList* ObjCreatPropList::getConstant()
{
    // Tell the C library not to clean up, H5Library::termH5cpp will call
    // H5close - more dependency if use H5Library::dontAtExit()
    if (!IdComponent::H5dontAtexit_called)
    {
        (void) H5dont_atexit();
        IdComponent::H5dontAtexit_called = true;
    }

    // If the constant pointer is not allocated, allocate it. Otherwise,
    // throw because it shouldn't be.
    if (DEFAULT_ == 0)
        DEFAULT_ = new ObjCreatPropList(H5P_OBJECT_CREATE);
    else
        throw PropListIException("ObjCreatPropList::getConstant", "ObjCreatPropList::getConstant is being invoked on an allocated DEFAULT_");
    return(DEFAULT_);
}

//--------------------------------------------------------------------------
// Function:    ObjCreatPropList::deleteConstants
// Purpose:     Deletes the constant object that ObjCreatPropList::DEFAULT_
//              points to.
// exception    H5::PropListIException
// Programmer   Binh-Minh Ribler - 2015
//--------------------------------------------------------------------------
void ObjCreatPropList::deleteConstants()
{
    if (DEFAULT_ != 0)
        delete DEFAULT_;
}

//--------------------------------------------------------------------------
// Purpose:     Constant for default property
//--------------------------------------------------------------------------
const ObjCreatPropList& ObjCreatPropList::DEFAULT = *getConstant();

#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:    Default Constructor
///\brief       Creates a file access property list
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
ObjCreatPropList::ObjCreatPropList() : PropList(H5P_OBJECT_CREATE) {}

//--------------------------------------------------------------------------
// Function:    ObjCreatPropList copy constructor
///\brief       Copy constructor: same HDF5 object as \a original
///\param       original - IN: ObjCreatPropList instance to copy
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
ObjCreatPropList::ObjCreatPropList(const ObjCreatPropList& original) : PropList(original) {}

//--------------------------------------------------------------------------
// Function:    ObjCreatPropList overloaded constructor
///\brief       Creates a file access property list using the id of an
///             existing one.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
ObjCreatPropList::ObjCreatPropList(const hid_t plist_id) : PropList(plist_id) {}

//--------------------------------------------------------------------------
// Function:    ObjCreatPropList::setAttrPhaseChange
///\brief       Sets attribute storage phase change thresholds.
///\param       max_compact - IN: Maximum number of attributes to be stored in
///                               compact storage.  Default to 8
///\param       min_dense   - IN: Minimum number of attributes to be stored in
///                               dense storage.  Default to 6
///\exception   H5::PropListIException
///\par Description
///             If \c max_compact is set to 0, dense storage will be used.
///             For more detail about on attribute storage, please refer to the
///             H5Pset_attr_phase_change API in the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - September 2015
//--------------------------------------------------------------------------
void ObjCreatPropList::setAttrPhaseChange(unsigned max_compact, unsigned min_dense) const
{
    herr_t ret_value = H5Pset_attr_phase_change(id, max_compact, min_dense);
    if (ret_value < 0)
    {
        throw PropListIException("ObjCreatPropList::setAttrPhaseChange", "H5Pset_attr_phase_change failed");
    }
}

//--------------------------------------------------------------------------
// Function:    ObjCreatPropList::getAttrPhaseChange
///\brief       Gets attribute storage phase change thresholds.
///\param       max_compact - OUT: Maximum number of attributes to be stored in
///                               compact storage.
///\param       min_dense   - OUT: Minimum number of attributes to be stored in
///                               dense storage.
///\exception   H5::PropListIException
///\par Description
///             If \c max_compact is set to 0, dense storage will be used.
///             For more detail about on attribute storage, please refer to the
///             H5Pget_attr_phase_change API in the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - September 2015
//--------------------------------------------------------------------------
void ObjCreatPropList::getAttrPhaseChange(unsigned& max_compact, unsigned& min_dense) const
{
    herr_t ret_value;
    ret_value = H5Pget_attr_phase_change(id, &max_compact, &min_dense);
    if (ret_value < 0)
    {
        throw PropListIException("ObjCreatPropList::getAttrPhaseChange", "H5Pget_attr_phase_change failed");
    }
}

//--------------------------------------------------------------------------
// Function:    ObjCreatPropList::setAttrCrtOrder
///\brief       Set the flags for creation order of attributes on an object
///\param       crt_order_flags  - IN: Flags specifying whether to track and
///                     index attribute creation order.  Default: No flag set
///\exception   H5::PropListIException
///\par Description
///             Valid flags are:
///             \li \c H5P_CRT_ORDER_TRACKED - Attribute creation order is tracked
///             \li \c H5P_CRT_ORDER_INDEXED - Attribute creation order is
///                              indexed (requires H5P_CRT_ORDER_TRACKED).
///             When no flag is set, attribute creation order is neither
///             tracked not indexed.  Note that HDF5 currently provides no
///             mechanism to turn on attribute creation order tracking at object
///             creation time and to build the index later.
///             For detail, please refer to the H5Pset_attr_creation_order API
///             in the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - September 2015
//--------------------------------------------------------------------------
void ObjCreatPropList::setAttrCrtOrder(unsigned crt_order_flags) const
{
    herr_t ret_value = H5Pset_attr_creation_order(id, crt_order_flags);
    if (ret_value < 0)
    {
        throw PropListIException("ObjCreatPropList::setAttrCrtOrder", "H5Pset_attr_creation_order failed");
    }
}

//--------------------------------------------------------------------------
// Function:    ObjCreatPropList::getAttrCrtOrder
///\brief       Returns the flags indicating creation order is tracked/indexed
///             for attributes on an object.
///\return      The flags
///\exception   H5::PropListIException
///\par Description
///             When no flag is set, i.e. crt_order_flags = 0, attribute
///             creation order is neither tracked not indexed.
///             For detail, please refer to the H5Pget_attr_creation_order API
///             in the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - September 2015
//--------------------------------------------------------------------------
unsigned ObjCreatPropList::getAttrCrtOrder() const
{
    herr_t ret_value;
    unsigned crt_order_flags = 0;
    ret_value = H5Pget_attr_creation_order(id, &crt_order_flags);
    if (ret_value < 0)
    {
        throw PropListIException("ObjCreatPropList::getAttrCrtOrder", "H5Pget_attr_creation_order failed");
    }
    return(crt_order_flags);
}

//--------------------------------------------------------------------------
// Function:    ObjCreatPropList destructor
///\brief       Noop destructor
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
ObjCreatPropList::~ObjCreatPropList() {}

} // end namespace

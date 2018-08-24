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
#include "H5LaccProp.h"

namespace H5 {

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// This DOXYGEN_SHOULD_SKIP_THIS block is a work-around approach to control
// the order of creation and deletion of the global constants.  See Design Notes
// in "H5PredType.cpp" for information.

// Initialize a pointer for the constant
LinkAccPropList* LinkAccPropList::DEFAULT_ = 0;

//--------------------------------------------------------------------------
// Function:    LinkAccPropList::getConstant
//              Creates a LinkAccPropList object representing the HDF5 constant
//              H5P_LINK_ACCESS, pointed to by LinkAccPropList::DEFAULT_
// exception    H5::PropListIException
// Description
//              If LinkAccPropList::DEFAULT_ already points to an allocated
//              object, throw a PropListIException.  This scenario should not
//              happen.
// Programmer   Binh-Minh Ribler - December, 2016
//--------------------------------------------------------------------------
LinkAccPropList* LinkAccPropList::getConstant()
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
        DEFAULT_ = new LinkAccPropList(H5P_LINK_ACCESS);
    else
        throw PropListIException("LinkAccPropList::getConstant", "LinkAccPropList::getConstant is being invoked on an allocated DEFAULT_");
    return(DEFAULT_);
}

//--------------------------------------------------------------------------
// Function:    LinkAccPropList::deleteConstants
// Purpose:     Deletes the constant object that LinkAccPropList::DEFAULT_
//              points to.
// exception    H5::PropListIException
// Programmer   Binh-Minh Ribler - December, 2016
//--------------------------------------------------------------------------
void LinkAccPropList::deleteConstants()
{
    if (DEFAULT_ != 0)
        delete DEFAULT_;
}

//--------------------------------------------------------------------------
// Purpose:     Constant for default property
//--------------------------------------------------------------------------
const LinkAccPropList& LinkAccPropList::DEFAULT = *getConstant();

#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:    Default Constructor
///\brief       Creates a file access property list
// Programmer   Binh-Minh Ribler - December, 2016
//--------------------------------------------------------------------------
LinkAccPropList::LinkAccPropList() : PropList(H5P_LINK_ACCESS) {}

//--------------------------------------------------------------------------
// Function:    LinkAccPropList copy constructor
///\brief       Copy Constructor: same HDF5 object as \a original
///\param       original - IN: LinkAccPropList instance to copy
// Programmer   Binh-Minh Ribler - December, 2016
//--------------------------------------------------------------------------
LinkAccPropList::LinkAccPropList(const LinkAccPropList& original) : PropList(original) {}

//--------------------------------------------------------------------------
// Function:    LinkAccPropList overloaded constructor
///\brief       Creates a file access property list using the id of an
///             existing one.
// Programmer   Binh-Minh Ribler - December, 2016
//--------------------------------------------------------------------------
LinkAccPropList::LinkAccPropList(const hid_t plist_id) : PropList(plist_id) {}

//--------------------------------------------------------------------------
// Function:    LinkAccPropList::setNumLinks
///\brief       Set the number of soft or user-defined link traversals allowed
///             before the library assumes it has found a cycle and aborts the
///             traversal.
///
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - March 1, 2017
//--------------------------------------------------------------------------
void LinkAccPropList::setNumLinks(size_t nlinks) const
{
    herr_t ret_value = H5Pset_nlinks(id, nlinks);
    // Throw exception if H5Pset_nlinks returns failure
    if (ret_value < 0)
    {
        throw PropListIException("setNumLinks", "H5Pset_nlinks failed");
    }
}

//--------------------------------------------------------------------------
// Function:    LinkAccPropList::getNumLinks
///\brief       Gets the number of soft or user-defined links that can be
///             traversed before a failure occurs.
///
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - March 1, 2017
//--------------------------------------------------------------------------
size_t LinkAccPropList::getNumLinks() const
{
    size_t nlinks = 0;
    herr_t ret_value = H5Pget_nlinks(id, &nlinks);
    // Throw exception if H5Pget_nlinks returns failure
    if (ret_value < 0)
    {
        throw PropListIException("getNumLinks", "H5Pget_nlinks failed");
    }
    return(nlinks);
}

//--------------------------------------------------------------------------
// Function:    LinkAccPropList destructor
///\brief       Noop destructor
// Programmer   Binh-Minh Ribler - December, 2016
//--------------------------------------------------------------------------
LinkAccPropList::~LinkAccPropList() {}

} // end namespace

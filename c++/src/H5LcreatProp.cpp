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
#include "H5LcreatProp.h"

namespace H5 {

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// This DOXYGEN_SHOULD_SKIP_THIS block is a work-around approach to control
// the order of creation and deletion of the global constants.  See Design Notes
// in "H5PredType.cpp" for information.

// Initialize a pointer for the constant
LinkCreatPropList* LinkCreatPropList::DEFAULT_ = 0;

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList::getConstant
//              Creates a LinkCreatPropList object representing the HDF5 constant
//              H5P_LINK_CREATE, pointed to by LinkCreatPropList::DEFAULT_
// exception    H5::PropListIException
// Description
//              If LinkCreatPropList::DEFAULT_ already points to an allocated
//              object, throw a PropListIException.  This scenario should not
//              happen.
// December, 2016
//--------------------------------------------------------------------------
LinkCreatPropList* LinkCreatPropList::getConstant()
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
        DEFAULT_ = new LinkCreatPropList(H5P_LINK_CREATE);
    else
        throw PropListIException("LinkCreatPropList::getConstant", "LinkCreatPropList::getConstant is being invoked on an allocated DEFAULT_");
    return(DEFAULT_);
}

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList::deleteConstants
// Purpose:     Deletes the constant object that LinkCreatPropList::DEFAULT_
//              points to.
// exception    H5::PropListIException
// December, 2016
//--------------------------------------------------------------------------
void LinkCreatPropList::deleteConstants()
{
    if (DEFAULT_ != 0)
        delete DEFAULT_;
}

//--------------------------------------------------------------------------
// Purpose:     Constant for default property
//--------------------------------------------------------------------------
const LinkCreatPropList& LinkCreatPropList::DEFAULT = *getConstant();

#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:    Default Constructor
///\brief       Creates a file access property list
// December, 2016
//--------------------------------------------------------------------------
LinkCreatPropList::LinkCreatPropList() : PropList(H5P_LINK_CREATE) {}

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList copy constructor
///\brief       Copy constructor: same HDF5 object as \a original
///\param       original - IN: LinkCreatPropList instance to copy
// December, 2016
//--------------------------------------------------------------------------
LinkCreatPropList::LinkCreatPropList(const LinkCreatPropList& original) : PropList(original) {}

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList overloaded constructor
///\brief       Creates a file access property list using the id of an
///             existing one.
// December, 2016
//--------------------------------------------------------------------------
LinkCreatPropList::LinkCreatPropList(const hid_t plist_id) : PropList(plist_id) {}

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList::setCreateIntermediateGroup
///\brief       Specifies in property list whether to create missing
///             intermediate groups.
///\param       crt_intmd_group - IN: Flag specifying whether to create
///                               intermediate groups upon the creation of an object
///\exception   H5::PropListIException
// April, 2019
//--------------------------------------------------------------------------
void LinkCreatPropList::setCreateIntermediateGroup(bool crt_intmd_group) const
{
    herr_t ret_value = H5Pset_create_intermediate_group(id, (unsigned)crt_intmd_group);
    // Throw exception if H5Pset_create_intermediate_group returns failure
    if (ret_value < 0)
    {
        throw PropListIException("setCreateIntermediateGroup", "H5Pset_create_intermediate_group failed");
    }
}

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList::getCreateIntermediateGroup
///\brief       Determines whether property is set to enable creating missing
///             intermediate groups.
///\return      true if creating intermediate groups is enabled, and false, otherwise
///\exception   H5::PropListIException
// April, 2019
//--------------------------------------------------------------------------
bool LinkCreatPropList::getCreateIntermediateGroup() const
{
    unsigned crt_intmd_group;
    herr_t ret_value = H5Pget_create_intermediate_group(id, &crt_intmd_group);
    // Throw exception if H5Pget_create_intermediate_group returns failure
    if (ret_value < 0)
    {
        throw PropListIException("getCreateIntermediateGroup", "H5Pget_create_intermediate_group failed");
    }

    return((bool)crt_intmd_group);
}

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList::setCharEncoding
///\brief       Sets the character encoding of the string.
///
///\exception   H5::PropListIException
// March, 2018
//--------------------------------------------------------------------------
void LinkCreatPropList::setCharEncoding(H5T_cset_t encoding) const
{
    herr_t ret_value = H5Pset_char_encoding(id, encoding);
    // Throw exception if H5Pset_char_encoding returns failure
    if (ret_value < 0)
    {
        throw PropListIException("setCharEncoding", "H5Pset_char_encoding failed");
    }
}

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList::getCharEncoding
///\brief       Gets the character encoding of the string.
///\return      The character encoding
///\exception   H5::PropListIException
// March, 2018
//--------------------------------------------------------------------------
H5T_cset_t LinkCreatPropList::getCharEncoding() const
{
    H5T_cset_t encoding;
    herr_t ret_value = H5Pget_char_encoding(id, &encoding);
    // Throw exception if H5Pget_char_encoding returns failure
    if (ret_value < 0)
    {
        throw PropListIException("getCharEncoding", "H5Pget_char_encoding failed");
    }
    return(encoding);
}

//--------------------------------------------------------------------------
// Function:    LinkCreatPropList destructor
///\brief       Noop destructor
// December, 2016
//--------------------------------------------------------------------------
LinkCreatPropList::~LinkCreatPropList() {}

} // end namespace

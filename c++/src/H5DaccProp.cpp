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
#include "H5DataSpace.h"
#include "H5PropList.h"
#include "H5LaccProp.h"
#include "H5DaccProp.h"

namespace H5 {

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// This DOXYGEN_SHOULD_SKIP_THIS block is a work-around approach to control
// the order of creation and deletion of the global constants.  See Design Notes
// in "H5PredType.cpp" for information.

// Initialize a pointer for the constant
DSetAccPropList* DSetAccPropList::DEFAULT_ = 0;

//--------------------------------------------------------------------------
// Function:    DSetAccPropList::getConstant
// Purpose:     Creates a DSetAccPropList object representing the HDF5
//              constant H5P_DATASET_ACCESS, pointed to by
//              DSetAccPropList::DEFAULT_
// exception    H5::PropListIException
// Description
//              If DSetAccPropList::DEFAULT_ already points to an allocated
//              object, throw a PropListIException.  This scenario should
//              not happen.
// Programmer   Binh-Minh Ribler - 2015
//--------------------------------------------------------------------------
DSetAccPropList* DSetAccPropList::getConstant()
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
        DEFAULT_ = new DSetAccPropList(H5P_DATASET_ACCESS);
    else
        throw PropListIException("DSetAccPropList::getConstant", "DSetAccPropList::getConstant is being invoked on an allocated DEFAULT_");
    return(DEFAULT_);
}

//--------------------------------------------------------------------------
// Function:    DSetAccPropList::deleteConstants
// Purpose:     Deletes the constant object that DSetAccPropList::DEFAULT_
//              points to.
// Programmer   Binh-Minh Ribler - 2015
//--------------------------------------------------------------------------
void DSetAccPropList::deleteConstants()
{
    if (DEFAULT_ != 0)
        delete DEFAULT_;
}

//--------------------------------------------------------------------------
// Purpose      Constant for dataset creation default property
//--------------------------------------------------------------------------
const DSetAccPropList& DSetAccPropList::DEFAULT = *getConstant();

#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:    DSetAccPropList default constructor
///\brief       Default constructor: creates a stub dataset creation property list
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DSetAccPropList::DSetAccPropList() : LinkAccPropList(H5P_DATASET_ACCESS) {}

//--------------------------------------------------------------------------
// Function:    DSetAccPropList copy constructor
///\brief       Copy constructor: same HDF5 object as \a original
///             DSetAccPropList object
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DSetAccPropList::DSetAccPropList(const DSetAccPropList& orig) : LinkAccPropList(orig) {}

//--------------------------------------------------------------------------
// Function:    DSetAccPropList overloaded constructor
///\brief       Creates a DSetAccPropList object using the id of an
///             existing dataset creation property list.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DSetAccPropList::DSetAccPropList(const hid_t plist_id) : LinkAccPropList(plist_id) {}

//--------------------------------------------------------------------------
// Function:    DSetAccPropList::setChunkCache
///\brief       Sets the raw data chunk cache parameters.
///\param       rdcc_nslots - IN: Number of chunk slots in the raw data chunk cache
///\param       rdcc_nbytes - IN: Total size of the raw data chunk cache
///\param       rdcc_w0     - IN: The chunk preemption policy for this dataset
///\exception   H5::PropListIException
///\par Description
///             The raw data chunk cache parameters includes the number of
///             objects in the meta data cache and the maximum number of
///             chunks and bytes in the raw data chunk cache.  Once set,
///             these values will override the values in the file access
///             property list.
///
///             For information, please refer to the H5Pset_chunk_cache API in
///             the HDF5 C Reference Manual.
// July 2018
//--------------------------------------------------------------------------
void DSetAccPropList::setChunkCache(size_t rdcc_nslots, size_t rdcc_nbytes, double rdcc_w0) const
{
    herr_t ret_value = H5Pset_chunk_cache(id, rdcc_nslots, rdcc_nbytes, rdcc_w0);
    if (ret_value < 0)
    {
        throw PropListIException("DSetAccPropList::setChunkCache", "H5Pset_chunk_cache failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetAccPropList::getChunkCache
///\brief       Retrieves the raw data chunk cache parameters.
///\param       rdcc_nslots - OUT: Number of chunk slots in the raw data chunk cache
///\param       rdcc_nbytes - OUT: Total size of the raw data chunk cache
///\param       rdcc_w0     - OUT: The chunk preemption policy for this dataset
///\exception   H5::PropListIException
///\par Description
///             For information, please refer to the H5Pget_chunk_cache API in
///             the HDF5 C Reference Manual.
// July 2018
//--------------------------------------------------------------------------
void  DSetAccPropList::getChunkCache(size_t &rdcc_nslots, size_t &rdcc_nbytes, double &rdcc_w0) const
{
    herr_t ret_value = H5Pget_chunk_cache(id, &rdcc_nslots, &rdcc_nbytes, &rdcc_w0);
    if (ret_value < 0)
    {
        throw PropListIException("DSetAccPropList::getChunkCache", "H5Pget_chunk_cache failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetAccPropList destructor
///\brief       Noop destructor.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DSetAccPropList::~DSetAccPropList() {}

} // end namespace

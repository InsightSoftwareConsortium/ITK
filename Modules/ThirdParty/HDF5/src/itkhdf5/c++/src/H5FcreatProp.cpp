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
#include "H5FcreatProp.h"

namespace H5 {

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// This DOXYGEN_SHOULD_SKIP_THIS block is a work-around approach to control
// the order of creation and deletion of the global constants.  See Design Notes
// in "H5PredType.cpp" for information.

// Initialize a pointer for the constant
FileCreatPropList* FileCreatPropList::DEFAULT_ = 0;

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::getConstant
// Purpose      Creates a FileCreatPropList object representing the HDF5
//              constant H5P_FILE_ACCESS, pointed to by FileCreatPropList::DEFAULT_
// exception    H5::PropListIException
// Description
//              If FileCreatPropList::DEFAULT_ already points to an allocated
//              object, throw a PropListIException.  This scenario should not happen.
// Programmer   Binh-Minh Ribler - 2015
//--------------------------------------------------------------------------
FileCreatPropList* FileCreatPropList::getConstant()
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
        DEFAULT_ = new FileCreatPropList(H5P_FILE_CREATE);
    else
        throw PropListIException("FileCreatPropList::getConstant", "FileCreatPropList::getConstant is being invoked on an allocated DEFAULT_");
    return(DEFAULT_);
}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::deleteConstants
// Purpose      Deletes the constant object that FileCreatPropList::DEFAULT_
//              points to.
// Programmer   Binh-Minh Ribler - 2015
//--------------------------------------------------------------------------
void FileCreatPropList::deleteConstants()
{
    if (DEFAULT_ != 0)
        delete DEFAULT_;
}

//--------------------------------------------------------------------------
// Purpose      Constant for default property
//--------------------------------------------------------------------------
const FileCreatPropList& FileCreatPropList::DEFAULT = *getConstant();

#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:    FileCreatPropList default constructor
///\brief       Default constructor: Creates a file create property list
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
FileCreatPropList::FileCreatPropList() : PropList(H5P_FILE_CREATE) {}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList copy constructor
///\brief       Copy constructor: same HDF5 object as \a original
///             FileCreatPropList object.
///\param       original - IN: FileCreatPropList instance to copy
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
FileCreatPropList::FileCreatPropList(const FileCreatPropList& original) : PropList( original ) {}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList overloaded constructor
///\brief       Creates a file creation property list using the id of an
///             existing one.
///\param       plist_id - IN: FileCreatPropList id to use
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
FileCreatPropList::FileCreatPropList(const hid_t plist_id) : PropList(plist_id) {}

#ifndef H5_NO_DEPRECATED_SYMBOLS
//--------------------------------------------------------------------------
// Function:    FileCreatPropList::getVersion
///\brief       Retrieves version information for various parts of a file.
///\param       super    - OUT: The file super block.
///\param       freelist - OUT: The global free list.
///\param       stab     - OUT: The root symbol table entry.
///\param       shhdr    - OUT: Shared object headers.
///\exception   H5::PropListIException
///\par Description
///             Any (or even all) of the output arguments can be null pointers.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void FileCreatPropList::getVersion(unsigned& super, unsigned& freelist, unsigned& stab, unsigned& shhdr) const
{
    herr_t ret_value = H5Pget_version(id, &super, &freelist, &stab, &shhdr);
    if (ret_value < 0)
    {
        throw PropListIException("FileCreatPropList::getVersion",
            "H5Pget_version failed");
    }
}
#endif /* H5_NO_DEPRECATED_SYMBOLS */

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::setUserblock
///\brief       Sets the user block size field of this file creation property list.
///\param       size - IN: User block size to be set, in bytes
///\exception   H5::PropListIException
///\par Description
///             The default user block size is 0; it may be set to any power
///             of 2 equal to 512 or greater (512, 1024, 2048, etc.)
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void FileCreatPropList::setUserblock(hsize_t size) const
{
    herr_t ret_value = H5Pset_userblock(id, size);
    if (ret_value < 0)
    {
        throw PropListIException("FileCreatPropList::setUserblock",
            "H5Pset_userblock failed");
    }
}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::getUserblock
///\brief       Returns the user block size of this file creation property list.
///\return      User block size
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
hsize_t FileCreatPropList::getUserblock() const
{
    hsize_t userblock_size;
    herr_t ret_value = H5Pget_userblock(id, &userblock_size);
    if (ret_value < 0)
    {
        throw PropListIException("FileCreatPropList::getUserblock",
            "H5Pget_userblock failed");
    }
    return(userblock_size);
}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::setSizes
///\brief       Sets the byte size of the offsets and lengths used to
///             address objects in an HDF5 file.
///\param       sizeof_addr - IN: Size of an object offset in bytes
///\param       sizeof_size - IN: Size of an object length in bytes.
///\exception   H5::PropListIException
///\par Description
///             For information, please refer to the H5Pset_sizes API in
///             the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void FileCreatPropList::setSizes(size_t sizeof_addr, size_t sizeof_size) const
{
    herr_t ret_value = H5Pset_sizes(id, sizeof_addr, sizeof_size);
    if (ret_value < 0)
    {
        throw PropListIException("FileCreatPropList::setSizes",
            "H5Pset_sizes failed");
    }
}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::getSizes
///\brief       Retrieves the size of the offsets and lengths used in an
///             HDF5 file.
///
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void FileCreatPropList::getSizes(size_t& sizeof_addr, size_t& sizeof_size) const
{
    herr_t ret_value = H5Pget_sizes(id, &sizeof_addr, &sizeof_size);
    if (ret_value < 0)
    {
        throw PropListIException("FileCreatPropList::getSizes",
            "H5Pget_sizes failed");
    }
}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::setSymk
///\brief       Sets the size of parameters used to control the symbol table
///             nodes.
///\param       ik - IN: Symbol table tree rank
///\param       lk - IN: Symbol table node size
///\exception   H5::PropListIException
///\par Description
///             For information, please refer to the H5Pset_sym_k API in
///             the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void FileCreatPropList::setSymk(unsigned ik, unsigned lk) const
{
    herr_t ret_value = H5Pset_sym_k(id, ik, lk);
    if (ret_value < 0)
    {
        throw PropListIException("FileCreatPropList::setSymk",
            "H5Pset_sym_k failed");
    }
}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::getSymk
///\brief       Retrieves the size of the symbol table B-tree 1/2 rank and
///             the symbol table leaf node 1/2 size.
///
///\exception   H5::PropListIException
///\par Description
///             For information, please refer to the H5Pget_sym_k API in
///             the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void FileCreatPropList::getSymk(unsigned& ik, unsigned& lk) const
{
    herr_t ret_value = H5Pget_sym_k(id, &ik, &lk);
    if (ret_value < 0)
    {
        throw PropListIException("FileCreatPropList::getSymk",
            "H5Pget_sym_k failed");
    }
}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::setIstorek
///\brief       Sets the size of the parameter used to control the B-trees
///             for indexing chunked datasets.
///\param       ik - IN: 1/2 rank of chunked storage B-tree
///\exception   H5::PropListIException
///\par Description
///             For information, please refer to the H5Pset_istore_k API in
///             the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void FileCreatPropList::setIstorek(unsigned ik) const
{
    herr_t ret_value = H5Pset_istore_k(id, ik);
    if (ret_value < 0)
    {
        throw PropListIException("FileCreatPropList::setIstorek",
            "H5Pset_istore_k failed");
    }
}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::getIstorek
///\brief       Returns the 1/2 rank of an indexed storage B-tree.
///\return      1/2 rank of chunked storage B-tree
///\exception   H5::PropListIException
///\par Description
///             For information, please refer to the H5Pget_istore_k API in
///             the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
unsigned FileCreatPropList::getIstorek() const
{
    unsigned ik;
    herr_t ret_value = H5Pget_istore_k(id, &ik);
    if (ret_value < 0)
    {
        throw PropListIException("FileCreatPropList::getIstorek",
            "H5Pget_istore_k failed");
    }
    return(ik);
}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::setFileSpaceStrategy
///\brief       Sets the strategy and the threshold value that the library
///             will employ in managing file space.
///\param       strategy  - IN: Strategy for file space management
///\param       persist   - IN: Whether to persist free-space
///\param       threshold - IN: Free-space section threshold. The library
///             default is 1, which is to track all free-space sections.
///\exception   H5::PropListIException
///\par Description
///             If the given strategy is zero, the property will not be
///             changed and the existing strategy will be retained.
///             If the given threshold value is zero, the property will not be
///             changed and the existing threshold will be retained.
///             For information, please refer to the H5Pset_file_space_strategy
///             API in the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - Feb, 2017
//--------------------------------------------------------------------------
void FileCreatPropList::setFileSpaceStrategy(H5F_fspace_strategy_t strategy, hbool_t persist, hsize_t threshold) const
{
    herr_t ret_value = H5Pset_file_space_strategy(id, strategy, persist, threshold);
    if (ret_value < 0)
    {
        throw PropListIException("FileCreatPropList::setFileSpaceStrategy",
            "H5Pset_file_space_strategy failed");
    }
}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::getFileSpaceStrategy
///\brief       Retrieves the strategy, persist, and threshold that the library
///             uses in managing file space.
///\param       strategy  - OUT: Strategy for file space management
///\param       persist   - OUT: Whether to persist free-space
///\param       threshold - OUT: Free-space section threshold
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - Feb, 2017
//--------------------------------------------------------------------------
void FileCreatPropList::getFileSpaceStrategy(H5F_fspace_strategy_t& strategy, hbool_t& persist, hsize_t& threshold) const
{
    herr_t ret_value = H5Pget_file_space_strategy(id, &strategy, &persist, &threshold);
    if (ret_value < 0)
    {
        throw PropListIException("FileCreatPropList::getFileSpaceStrategy",
            "H5Pget_file_space_strategy failed");
    }
}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::setFileSpacePagesize
///\brief       Sets the file space page size for paged aggregation.
///\param       fsp_psize - IN: Filespace's page size
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - Feb, 2017
//--------------------------------------------------------------------------
void FileCreatPropList::setFileSpacePagesize(hsize_t fsp_psize) const
{
    herr_t ret_value = H5Pset_file_space_page_size(id, fsp_psize);
    if (ret_value < 0)
    {
        throw PropListIException("FileCreatPropList::setFileSpacePagesize",
            "H5Pset_file_space_page_size failed");
    }
}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList::getFileSpacePagesize
///\brief       Returns the file space page size for aggregating small
///             metadata or raw data.
///\return      File space page size
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - Feb, 2017
//--------------------------------------------------------------------------
hsize_t FileCreatPropList::getFileSpacePagesize() const
{
    hsize_t fsp_psize = 0;
    herr_t ret_value = H5Pget_file_space_page_size(id, &fsp_psize);
    if (ret_value < 0)
    {
        throw PropListIException("FileCreatPropList::getFileSpacePagesize",
            "H5Pget_file_space_page_size failed");
    }
    return(fsp_psize);
}

//--------------------------------------------------------------------------
// Function:    FileCreatPropList destructor
///\brief       Noop destructor.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
FileCreatPropList::~FileCreatPropList() {}

} // end namespace

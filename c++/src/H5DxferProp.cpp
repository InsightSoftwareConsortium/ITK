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

#include <string>

#include "H5private.h" // for HDmemset
#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5DxferProp.h"

namespace H5 {

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// This DOXYGEN_SHOULD_SKIP_THIS block is a work-around approach to control
// the order of creation and deletion of the global constants.  See Design Notes
// in "H5PredType.cpp" for information.

// Initialize a pointer for the constant
DSetMemXferPropList *DSetMemXferPropList::DEFAULT_ = 0;

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getConstant
//              Creates a DSetMemXferPropList object representing the HDF5
//              constant H5P_DATASET_XFER, pointed to by
//              DSetMemXferPropList::DEFAULT_
// exception    H5::PropListIException
// Description
//              If DSetMemXferPropList::DEFAULT_ already points to an allocated
//              object, throw a PropListIException.  This scenario should not
//              happen.
// Programmer   Binh-Minh Ribler - 2015
//--------------------------------------------------------------------------
DSetMemXferPropList *
DSetMemXferPropList::getConstant()
{
    // Tell the C library not to clean up, H5Library::termH5cpp will call
    // H5close - more dependency if use H5Library::dontAtExit()
    if (!IdComponent::H5dontAtexit_called) {
        (void)H5dont_atexit();
        IdComponent::H5dontAtexit_called = true;
    }

    // If the constant pointer is not allocated, allocate it. Otherwise,
    // throw because it shouldn't be.
    if (DEFAULT_ == 0)
        DEFAULT_ = new DSetMemXferPropList(H5P_DATASET_XFER);
    else
        throw PropListIException(
            "DSetMemXferPropList::getConstant",
            "DSetMemXferPropList::getConstant is being invoked on an allocated DEFAULT_");
    return (DEFAULT_);
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::deleteConstants
// Purpose:     Deletes the constant object that DSetMemXferPropList::DEFAULT_
//              points to.
// Programmer   Binh-Minh Ribler - 2015
//--------------------------------------------------------------------------
void
DSetMemXferPropList::deleteConstants()
{
    if (DEFAULT_ != 0)
        delete DEFAULT_;
}

//--------------------------------------------------------------------------
// Purpose      Constant for default dataset memory and transfer property list.
//--------------------------------------------------------------------------
const DSetMemXferPropList &DSetMemXferPropList::DEFAULT = *getConstant();

#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function     DSetMemXferPropList default constructor
///\brief       Default constructor: creates a stub dataset memory and
///             transfer property list object.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DSetMemXferPropList::DSetMemXferPropList() : PropList(H5P_DATASET_XFER)
{
}

//--------------------------------------------------------------------------
// Function     DSetMemXferPropList constructor
///\brief       Creates a dataset transfer property list with transform
///             expression.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DSetMemXferPropList::DSetMemXferPropList(const char *exp) : PropList(H5P_DATASET_XFER)
{
    setDataTransform(exp);
}

//--------------------------------------------------------------------------
// Function     DSetMemXferPropList copy constructor
///\brief       Copy constructor: same HDF5 object as \a original
///             DSetMemXferPropList object
///\param       original - IN: Original dataset memory and transfer property
///                            list object to copy
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DSetMemXferPropList::DSetMemXferPropList(const DSetMemXferPropList &original) : PropList(original)
{
}

//--------------------------------------------------------------------------
// Function     DSetMemXferPropList overloaded constructor
///\brief       Creates a DSetMemXferPropList object using the id of an
///             existing DSetMemXferPropList.
///\param       plist_id - IN: Id of an existing dataset memory and transfer
///                            property list
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DSetMemXferPropList::DSetMemXferPropList(const hid_t plist_id) : PropList(plist_id)
{
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setBuffer
///\brief       Sets type conversion and background buffers.
///\param       size  - IN: Size, in bytes, of the type conversion and background buffers
///\param       tconv - IN: Pointer to application-allocated type conversion buffer
///\param       bkg   - IN: Pointer to application-allocated background buffer
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void
DSetMemXferPropList::setBuffer(size_t size, void *tconv, void *bkg) const
{
    herr_t ret_value = H5Pset_buffer(id, size, tconv, bkg);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::setBuffer", "H5Pset_buffer failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getBuffer
///\brief       Reads buffer settings.
///\param       tconv - OUT: Pointer to application-allocated type conversion buf
///\param       bkg   - OUT: Pointer to application-allocated background buffer
///\return      Buffer size, in bytes
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
size_t
DSetMemXferPropList::getBuffer(void **tconv, void **bkg) const
{
    size_t buffer_size = H5Pget_buffer(id, tconv, bkg);
    if (buffer_size == 0) {
        throw PropListIException("DSetMemXferPropList::getBuffer",
                                 "H5Pget_buffer returned 0 for buffer size - failure");
    }
    return (buffer_size);
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setPreserve
///\brief       Sets the dataset transfer property list status to true or false.
///\param       status - IN: Status to set, true or false
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void
DSetMemXferPropList::setPreserve(bool status) const
{
    herr_t ret_value = H5Pset_preserve(id, (hbool_t)status);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::setPreserve", "H5Pset_preserve failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getPreserve
///\brief       Checks status of the dataset transfer property list.
///\return      Status of the dataset transfer property list
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
bool
DSetMemXferPropList::getPreserve() const
{
    int ret_value = H5Pget_preserve(id);
    if (ret_value > 0)
        return true;
    else if (ret_value == 0)
        return false;
    else {
        throw PropListIException("DSetMemXferPropList::getPreserve",
                                 "H5Pget_preserve returned negative value for status");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setBtreeRatios
///\brief       Sets B-tree split ratios for a dataset transfer property list.
///\param       left   - IN: B-tree split ratio for left-most nodes
///\param       middle - IN: B-tree split ratio for right-most nodes and lone nodes
///\param       right  - IN: B-tree split ratio for all other nodes
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void
DSetMemXferPropList::setBtreeRatios(double left, double middle, double right) const
{
    herr_t ret_value = H5Pset_btree_ratios(id, left, middle, right);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::setBtreeRatios", "H5Pset_btree_ratios failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getBtreeRatios
///\brief       Gets B-tree split ratios for a dataset transfer property list.
///\param       left   - OUT: B-tree split ratio for left-most nodes
///\param       middle - OUT: B-tree split ratio for right-most nodes and lone nodes
///\param       right  - OUT: B-tree split ratio for all other nodes
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void
DSetMemXferPropList::getBtreeRatios(double &left, double &middle, double &right) const
{
    herr_t ret_value = H5Pget_btree_ratios(id, &left, &middle, &right);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::getBtreeRatios", "H5Pget_btree_ratios failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setDataTransform
///\brief       Sets data transform expression.
///\param       expression - IN: null-terminated data transform expression (char*)
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - Mar, 2014
//--------------------------------------------------------------------------
void
DSetMemXferPropList::setDataTransform(const char *expression) const
{
    herr_t ret_value = H5Pset_data_transform(id, expression);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::setDataTransform", "H5Pset_data_transform failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setDataTransform
///\brief       This is an overloaded member function, provided for convenience.
///             It takes a reference to a \c H5std_string for the expression.
///\param       expression - IN: H5std_string data transform expression
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - Mar, 2014
//--------------------------------------------------------------------------
void
DSetMemXferPropList::setDataTransform(const H5std_string &expression) const
{
    setDataTransform(expression.c_str());
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getDataTransform
///\brief       Sets data transform expression.
///\param       exp - OUT: buffer for data transform expression (char*)
///\param       buf_size   - IN: size of buffer for expression, including the
///                              null terminator
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - Mar, 2014
//--------------------------------------------------------------------------
ssize_t
DSetMemXferPropList::getDataTransform(char *exp, size_t buf_size) const
{
    // H5Pget_data_transform will get buf_size characters of the expression
    // including the null terminator
    ssize_t exp_len;
    exp_len = H5Pget_data_transform(id, exp, buf_size);

    // H5Pget_data_transform returns a negative value, raise an exception
    if (exp_len < 0) {
        throw PropListIException("DSetMemXferPropList::getDataTransform", "H5Pget_data_transform failed");
    }

    // H5Pget_data_transform will put a null terminator at the end of the
    // expression or at [buf_size-1] if the expression is at least the size
    // of the buffer.

    // Return the expression length, which might be different from buf_size
    return (exp_len);
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getDataTransform
///\brief       This is an overloaded member function, provided for convenience.
///             It takes no parameter and returns a \c H5std_string for the expression.
///
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - Mar, 2014
//--------------------------------------------------------------------------
H5std_string
DSetMemXferPropList::getDataTransform() const
{
    // Initialize string to "", so that if there is no expression, the returned
    // string will be empty
    H5std_string expression;

    // Preliminary call to get the expression's length
    ssize_t exp_len = H5Pget_data_transform(id, NULL, (size_t)0);

    // If H5Pget_data_transform returns a negative value, raise an exception
    if (exp_len < 0) {
        throw PropListIException("DSetMemXferPropList::getDataTransform", "H5Pget_data_transform failed");
    }

    // If expression exists, calls C routine again to get it
    else if (exp_len > 0) {
        // Temporary buffer for char* expression
        char *exp_C = new char[exp_len + 1];
        HDmemset(exp_C, 0, exp_len + 1); // clear buffer

        // Used overloaded function
        exp_len = getDataTransform(exp_C, exp_len + 1);

        // Convert the C expression to return
        expression = exp_C;

        // Clean up resource
        delete[] exp_C;
    }
    // Return the string expression
    return (expression);
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getTypeConvCB
///\brief       Sets an exception handling callback for datatype conversion
///             for a dataset transfer property list.
///\param       op        - IN: User's function
///\param       user_data - IN: User's data
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void
DSetMemXferPropList::setTypeConvCB(H5T_conv_except_func_t op, void *user_data) const
{
    herr_t ret_value = H5Pset_type_conv_cb(id, op, user_data);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::setTypeConvCB", "H5Pset_type_conv_cb failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getTypeConvCB
///\brief       Gets the exception handling callback function and data.
///\param       op        - IN: Retrieved user function
///\param       user_data - IN: Retrieved user data
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void
DSetMemXferPropList::getTypeConvCB(H5T_conv_except_func_t *op, void **user_data) const
{
    herr_t ret_value = H5Pget_type_conv_cb(id, op, user_data);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::getTypeConvCB", "H5Pget_type_conv_cb failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setVlenMemManager
///\brief       Sets the memory manager for variable-length datatype allocation.
///\param       alloc_func - IN: User's allocate routine
///\param       alloc_info - IN: User's allocation parameters
///\param       free_func  - IN: User's free routine
///\param       free_info  - IN: User's free parameters
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void
DSetMemXferPropList::setVlenMemManager(H5MM_allocate_t alloc_func, void *alloc_info, H5MM_free_t free_func,
                                       void *free_info) const
{
    herr_t ret_value = H5Pset_vlen_mem_manager(id, alloc_func, alloc_info, free_func, free_info);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::setVlenMemManager", "H5Pset_vlen_mem_manager failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setVlenMemManager
///\brief       Sets the memory manager for variable-length datatype
///             allocation - system \c malloc and \c free will be used.
///
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void
DSetMemXferPropList::setVlenMemManager() const
{
    setVlenMemManager(NULL, NULL, NULL, NULL);
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getVlenMemManager
///\brief       Gets the memory manager for variable-length datatype allocation
///\param       alloc_func - OUT: User's allocate routine
///\param       alloc_info - OUT: User's allocation parameters
///\param       free_func  - OUT: User's free routine
///\param       free_info  - OUT: User's free parameters
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void
DSetMemXferPropList::getVlenMemManager(H5MM_allocate_t &alloc_func, void **alloc_info, H5MM_free_t &free_func,
                                       void **free_info) const
{
    herr_t ret_value = H5Pget_vlen_mem_manager(id, &alloc_func, alloc_info, &free_func, free_info);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::getVlenMemManager", "H5Pget_vlen_mem_manager failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setSmallDataBlockSize
///\brief       Sets the size of a contiguous block reserved for small data.
///\param       size - IN: Maximum size, in bytes, of the small data block.
///\exception   H5::PropListIException
///\par Description
///             For detail, please refer to the H5Pset_small_data_block_size
///             API in the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void
DSetMemXferPropList::setSmallDataBlockSize(hsize_t size) const
{
    herr_t ret_value = H5Pset_small_data_block_size(id, size);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::setSmallDataBlockSize",
                                 "H5Pset_small_data_block_size failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getSmallDataBlockSize
///\brief       Returns the current small data block size setting.
///\return      Size of the small data block, in bytes
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
hsize_t
DSetMemXferPropList::getSmallDataBlockSize() const
{
    hsize_t size;
    herr_t  ret_value = H5Pget_small_data_block_size(id, &size);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::getSmallDataBlockSize",
                                 "H5Pget_small_data_block_size failed");
    }
    return (size);
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setHyperVectorSize
///\brief       Sets number of I/O vectors to be read/written in hyperslab I/O.
///
///\exception   H5::PropListIException
///\par Description
///             For detail, please refer to the H5Pset_hyper_vector_size
///             API in the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void
DSetMemXferPropList::setHyperVectorSize(size_t vector_size) const
{
    herr_t ret_value = H5Pset_hyper_vector_size(id, vector_size);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::setHyperVectorSize",
                                 "H5Pset_hyper_vector_size failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getHyperVectorSize
///\brief       Returns the number of I/O vectors to be read/written in
///             hyperslab I/O.
///\return      Number of I/O vectors
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
size_t
DSetMemXferPropList::getHyperVectorSize() const
{
    size_t vector_size;
    herr_t ret_value = H5Pget_hyper_vector_size(id, &vector_size);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::getHyperVectorSize",
                                 "H5Pget_hyper_vector_size failed");
    }
    return (vector_size);
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::setEDCCheck
///\brief       Enables or disables error-detecting for a dataset reading
///             process.
///\param       check - IN: Specifies whether error detection is enabled or
///                         disabled
///\exception   H5::PropListIException
///\par Description
///             The error detection algorithm used is the algorithm previously
///             specified in the corresponding dataset creation property
///             list.  This function does not affect the use of error
///             detection in the writing process.
///\par
///             Valid values are as follows:
///             \li \c H5Z_ENABLE_EDC   (default)
///             \li \c H5Z_DISABLE_EDC
// Programmer   Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
void
DSetMemXferPropList::setEDCCheck(H5Z_EDC_t check) const
{
    herr_t ret_value = H5Pset_edc_check(id, check);
    if (ret_value < 0) {
        throw PropListIException("DSetMemXferPropList::setEDCCheck", "H5Pset_edc_check failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList::getEDCCheck
///\brief       Determines whether error-detection is enabled for dataset reads.
///\return      \c H5Z_ENABLE_EDC or \c H5Z_DISABLE_EDC
///\exception   H5::PropListIException
// Programmer   Binh-Minh Ribler - April, 2004
//--------------------------------------------------------------------------
H5Z_EDC_t
DSetMemXferPropList::getEDCCheck() const
{
    H5Z_EDC_t check = H5Pget_edc_check(id);
    if (check < 0) {
        throw PropListIException("DSetMemXferPropList::getEDCCheck", "H5Pget_edc_check failed");
    }
    return (check);
}

//--------------------------------------------------------------------------
// Function:    DSetMemXferPropList destructor
///\brief       Noop destructor.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DSetMemXferPropList::~DSetMemXferPropList()
{
}

} // namespace H5

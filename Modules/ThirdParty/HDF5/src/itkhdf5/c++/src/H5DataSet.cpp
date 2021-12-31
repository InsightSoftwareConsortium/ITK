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

#include <iostream>
#include <string>

#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5PropList.h"
#include "H5FaccProp.h"
#include "H5FcreatProp.h"
#include "H5OcreatProp.h"
#include "H5DxferProp.h"
#include "H5DcreatProp.h"
#include "H5LcreatProp.h"
#include "H5LaccProp.h"
#include "H5DaccProp.h"
#include "H5Location.h"
#include "H5Object.h"
#include "H5DataType.h"
#include "H5DataSpace.h"
#include "H5AbstractDs.h"
#include "H5Attribute.h"
#include "H5DataSet.h"

namespace H5 {
using std::cerr;
using std::endl;

//--------------------------------------------------------------------------
// Function:    DataSet default constructor
///\brief       Default constructor: creates a stub DataSet.
//--------------------------------------------------------------------------
DataSet::DataSet() : H5Object(), AbstractDs(), id(H5I_INVALID_HID)
{
}

//--------------------------------------------------------------------------
// Function:    DataSet overloaded constructor
///\brief       Creates an DataSet object using the id of an existing dataset.
///\param       existing_id - IN: Id of an existing dataset
// Description
//              incRefCount() is needed here to prevent the id from being closed
//              prematurely.  That is, when application uses the id of an
//              existing DataSet object to create another DataSet object.  So,
//              when one of those objects is deleted, the id will be closed if
//              the reference counter is only 1.
//--------------------------------------------------------------------------
DataSet::DataSet(const hid_t existing_id) : H5Object(), AbstractDs(), id(existing_id)
{
    incRefCount(); // increment number of references to this id
}

//--------------------------------------------------------------------------
// Function:    DataSet copy constructor
///\brief       Copy constructor: same HDF5 object as \a original
///\param       original - IN: DataSet instance to copy
//--------------------------------------------------------------------------
DataSet::DataSet(const DataSet &original) : H5Object(), AbstractDs(), id(original.id)
{
    incRefCount(); // increment number of references to this id
}

//--------------------------------------------------------------------------
// Function:    DataSet assignment operator
///\brief       Assignment operator: same HDF5 object as \a original
///\param       original - IN: DataSet instance to copy
//--------------------------------------------------------------------------
DataSet &
DataSet::operator=(const DataSet &original)
{
    if (this != &original) {
        setId(original.id);
    }
    return (*this);
}

//--------------------------------------------------------------------------
// Function:    DataSet overload constructor - dereference
///\brief       Given a reference, ref, to an hdf5 location, creates a
///             DataSet object
///\param       loc - IN: Dataset reference object is in or location of
///                       object that the dataset is located within.
///\param       ref - IN: Reference pointer
///\param       ref_type - IN: Reference type - default to H5R_OBJECT
///\param       plist - IN: Property list - default to PropList::DEFAULT
///\exception   H5::DataSetIException
///\par Description
///             \c loc can be DataSet, Group, H5File, or named DataType, that
///             is a datatype that has been named by DataType::commit.
//--------------------------------------------------------------------------
DataSet::DataSet(const H5Location &loc, const void *ref, H5R_type_t ref_type, const PropList &plist)
    : H5Object(), AbstractDs(), id(H5I_INVALID_HID)
{
    id = H5Location::p_dereference(loc.getId(), ref, ref_type, plist, "constructor - by dereferenced");
}

//--------------------------------------------------------------------------
// Function:    DataSet overload constructor - dereference
///\brief       Given a reference, ref, to an hdf5 attribute, creates a
///             DataSet object
///\param       attr - IN: Specifying location where the referenced object is in
///\param       ref - IN: Reference pointer
///\param       ref_type - IN: Reference type - default to H5R_OBJECT
///\param       plist - IN: Property list - default to PropList::DEFAULT
///\exception   H5::ReferenceException
//--------------------------------------------------------------------------
DataSet::DataSet(const Attribute &attr, const void *ref, H5R_type_t ref_type, const PropList &plist)
    : H5Object(), AbstractDs(), id(H5I_INVALID_HID)
{
    id = H5Location::p_dereference(attr.getId(), ref, ref_type, plist, "constructor - by dereference");
}

//--------------------------------------------------------------------------
// Function:    DataSet::getSpace
///\brief       Gets a copy of the dataspace of this dataset.
///\return      DataSpace instance
///\exception   H5::DataSetIException
//--------------------------------------------------------------------------
DataSpace
DataSet::getSpace() const
{
    // Calls C function H5Dget_space to get the id of the dataspace
    hid_t dataspace_id = H5Dget_space(id);

    // If the dataspace id is invalid, throw an exception
    if (dataspace_id < 0) {
        throw DataSetIException("DataSet::getSpace", "H5Dget_space failed");
    }
    // create dataspace object using the existing id then return the object
    DataSpace data_space;
    f_DataSpace_setId(&data_space, dataspace_id);
    return (data_space);
}

// This private member function calls the C API to get the identifier
// of the datatype that is used by this dataset.  It is used
// by the various AbstractDs functions to get the specific datatype.
hid_t
DataSet::p_get_type() const
{
    hid_t type_id = H5Dget_type(id);
    if (type_id > 0)
        return (type_id);
    else {
        throw DataSetIException("", "H5Dget_type failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DataSet::getCreatePlist
///\brief       Gets the dataset creation property list.
///\return      DSetCreatPropList instance
///\exception   H5::DataSetIException
//--------------------------------------------------------------------------
DSetCreatPropList
DataSet::getCreatePlist() const
{
    hid_t create_plist_id = H5Dget_create_plist(id);
    if (create_plist_id < 0) {
        throw DataSetIException("DataSet::getCreatePlist", "H5Dget_create_plist failed");
    }

    // create and return the DSetCreatPropList object
    DSetCreatPropList create_plist;
    f_PropList_setId(&create_plist, create_plist_id);
    return (create_plist);
}

//--------------------------------------------------------------------------
// Function:    DataSet::getAccessPlist
///\brief       Gets the dataset access property list.
///\return      DSetAccPropList instance
///\exception   H5::DataSetIException
// July 2018
//--------------------------------------------------------------------------
DSetAccPropList
DataSet::getAccessPlist() const
{
    hid_t access_plist_id = H5Dget_access_plist(id);
    if (access_plist_id < 0) {
        throw DataSetIException("DataSet::getAccessPlist", "H5Dget_access_plist failed");
    }

    // create and return the DSetCreatPropList object
    DSetAccPropList access_plist;
    f_PropList_setId(&access_plist, access_plist_id);
    return (access_plist);
}

//--------------------------------------------------------------------------
// Function:    DataSet::getStorageSize
///\brief       Returns the amount of storage required for a dataset.
///\return      Size of the storage or 0, for no data
///\exception   H5::DataSetIException
// Note:        H5Dget_storage_size returns 0 when there is no data.  This
//              function should have no failure. (from SLU)
//--------------------------------------------------------------------------
hsize_t
DataSet::getStorageSize() const
{
    hsize_t storage_size = H5Dget_storage_size(id);
    return (storage_size);
}

//--------------------------------------------------------------------------
// Function:    DataSet::getInMemDataSize
///\brief       Gets the size in memory of the dataset's data.
///\return      Size of data (in memory)
///\exception   H5::DataSetIException
//--------------------------------------------------------------------------
size_t
DataSet::getInMemDataSize() const
{
    const char *func = "DataSet::getInMemDataSize";

    // Get the data type of this dataset
    hid_t mem_type_id = H5Dget_type(id);
    if (mem_type_id < 0) {
        throw DataSetIException(func, "H5Dget_type failed");
    }

    // Get the data type's size by first getting its native type then getting
    // the native type's size.
    hid_t native_type = H5Tget_native_type(mem_type_id, H5T_DIR_DEFAULT);
    if (native_type < 0) {
        throw DataSetIException(func, "H5Tget_native_type failed");
    }
    size_t type_size = H5Tget_size(native_type);
    if (type_size == 0) {
        throw DataSetIException(func, "H5Tget_size failed");
    }

    // Close the native type and the datatype of this dataset.
    if (H5Tclose(native_type) < 0) {
        throw DataSetIException(func, "H5Tclose(native_type) failed");
    }
    if (H5Tclose(mem_type_id) < 0) {
        throw DataSetIException(func, "H5Tclose(mem_type_id) failed");
    }

    // Get number of elements of the dataset by first getting its dataspace,
    // then getting the number of elements in the dataspace
    hid_t space_id = H5Dget_space(id);
    if (space_id < 0) {
        throw DataSetIException(func, "H5Dget_space failed");
    }
    hssize_t num_elements = H5Sget_simple_extent_npoints(space_id);
    if (num_elements < 0) {
        throw DataSetIException(func, "H5Sget_simple_extent_npoints failed");
    }

    // Close the dataspace
    if (H5Sclose(space_id) < 0) {
        throw DataSetIException(func, "H5Sclose failed");
    }

    // Calculate and return the size of the data
    size_t data_size = type_size * num_elements;
    return (data_size);
}

//--------------------------------------------------------------------------
// Function:    DataSet::getOffset
///\brief       Returns the address of this dataset in the file.
///\return      Address of dataset
///\exception   H5::DataSetIException
//--------------------------------------------------------------------------
haddr_t
DataSet::getOffset() const
{
    haddr_t ds_addr; // for address of dataset

    ds_addr = H5Dget_offset(id);
    if (ds_addr == HADDR_UNDEF) {
        throw DataSetIException("DataSet::getOffset", "H5Dget_offset returned HADDR_UNDEF");
    }
    return (ds_addr);
}

//--------------------------------------------------------------------------
// Function:    DataSet::getSpaceStatus
///\brief       Determines whether space has been allocated for a dataset.
///\param       status - OUT: Space allocation status
///\exception   H5::DataSetIException
//--------------------------------------------------------------------------
void
DataSet::getSpaceStatus(H5D_space_status_t &status) const
{
    herr_t ret_value = H5Dget_space_status(id, &status);
    if (ret_value < 0) {
        throw DataSetIException("DataSet::getSpaceStatus", "H5Dget_space_status failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DataSet::getVlenBufSize
///\brief       Returns the number of bytes required to store VL data.
///\param       type - IN: Datatype, which is the datatype for the buffer
///\param       space - IN: Selection for the memory buffer
///\return      Amount of storage
///\exception   H5::DataSetIException
//--------------------------------------------------------------------------
hsize_t
DataSet::getVlenBufSize(const DataType &type, const DataSpace &space) const
{
    // Obtain identifiers for C API
    hid_t type_id  = type.getId();
    hid_t space_id = space.getId();

    hsize_t size; // for amount of storage

    herr_t ret_value = H5Dvlen_get_buf_size(id, type_id, space_id, &size);
    if (ret_value < 0) {
        throw DataSetIException("DataSet::getVlenBufSize", "H5Dvlen_get_buf_size failed");
    }
    return (size);
}

//--------------------------------------------------------------------------
// Function:    DataSet::getVlenBufSize
// Purpose      This is an overloaded member function, kept for backward
//              compatibility.  It differs from the above function in that it
//              misses const's.  This wrapper will be removed in future release.
// Return       Amount of storage
// Exception    H5::DataSetIException
// Modification
//              Modified to call its replacement. -BMR, 2014/04/16
//              Removed from documentation. -BMR, 2016/03/07 1.8.17 and 1.10.0
//              Removed from code. -BMR, 2016/08/11 1.8.18 and 1.10.1
//--------------------------------------------------------------------------
// hsize_t DataSet::getVlenBufSize(DataType& type, DataSpace& space) const
//{
//    return(getVlenBufSize(type, space));
//}

//--------------------------------------------------------------------------
// Function:    DataSet::vlenReclaim
///\brief       Reclaims VL datatype memory buffers.
///\param       type - IN: Datatype, which is the datatype stored in the buffer
///\param       space - IN: Selection for the memory buffer to free the
///             VL datatypes within
///\param       xfer_plist - IN: Property list used to create the buffer
///\param       buf - IN: Pointer to the buffer to be reclaimed
///\exception   H5::DataSetIException
//--------------------------------------------------------------------------
void
DataSet::vlenReclaim(const DataType &type, const DataSpace &space, const DSetMemXferPropList &xfer_plist,
                     void *buf)
{
    // Obtain identifiers for C API
    hid_t type_id       = type.getId();
    hid_t space_id      = space.getId();
    hid_t xfer_plist_id = xfer_plist.getId();

    herr_t ret_value = H5Treclaim(type_id, space_id, xfer_plist_id, buf);
    if (ret_value < 0) {
        throw DataSetIException("DataSet::vlenReclaim", "H5Treclaim failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DataSet::vlenReclaim
///\brief       Reclaims VL datatype memory buffers.
///\param       type - IN: Datatype, which is the datatype stored in the buffer
///\param       space - IN: Selection for the memory buffer to free the
///             VL datatypes within
///\param       xfer_plist - IN: Property list used to create the buffer
///\param       buf - IN: Pointer to the buffer to be reclaimed
///\exception   H5::DataSetIException
//\parDescription
//              This function has better prototype for the users than the
//              other, which might be removed at some point. BMR - 2006/12/20
//--------------------------------------------------------------------------
void
DataSet::vlenReclaim(void *buf, const DataType &type, const DataSpace &space,
                     const DSetMemXferPropList &xfer_plist)
{
    // Obtain identifiers for C API
    hid_t type_id       = type.getId();
    hid_t space_id      = space.getId();
    hid_t xfer_plist_id = xfer_plist.getId();

    herr_t ret_value = H5Treclaim(type_id, space_id, xfer_plist_id, buf);
    if (ret_value < 0) {
        throw DataSetIException("DataSet::vlenReclaim", "H5Treclaim failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DataSet::read
///\brief       Reads raw data from the specified dataset.
///\param       buf - IN: Buffer for read data
///\param       mem_type - IN: Memory datatype
///\param       mem_space - IN: Memory dataspace
///\param       file_space - IN: Dataset's dataspace in the file
///\param       xfer_plist - IN: Transfer property list for this I/O operation
///\exception   H5::DataSetIException
///\par Description
///             This function reads raw data from this dataset into the
///             buffer \a buf, converting from file datatype and dataspace
///             to memory datatype \a mem_type and dataspace \a mem_space.
//--------------------------------------------------------------------------
void
DataSet::read(void *buf, const DataType &mem_type, const DataSpace &mem_space, const DataSpace &file_space,
              const DSetMemXferPropList &xfer_plist) const
{
    // Obtain identifiers for C API
    hid_t mem_type_id   = mem_type.getId();
    hid_t mem_space_id  = mem_space.getId();
    hid_t file_space_id = file_space.getId();
    hid_t xfer_plist_id = xfer_plist.getId();

    herr_t ret_value = H5Dread(id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf);
    if (ret_value < 0) {
        throw DataSetIException("DataSet::read", "H5Dread failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DataSet::read
///\brief       This is an overloaded member function, provided for convenience.
///             It takes a reference to a \c H5std_string for the buffer.
///\param       strg - IN: Buffer for read data string
///\param       mem_type - IN: Memory datatype
///\param       mem_space - IN: Memory dataspace
///\param       file_space - IN: Dataset's dataspace in the file
///\param       xfer_plist - IN: Transfer property list for this I/O operation
///\exception   H5::DataSetIException
// Modification
//      Jul 2009
//              Follow the change to Attribute::read and use the following
//              private functions to read datasets with fixed- and
//              variable-length string:
//                  DataSet::p_read_fixed_len and
//                  DataSet::p_read_variable_len
//--------------------------------------------------------------------------
void
DataSet::read(H5std_string &strg, const DataType &mem_type, const DataSpace &mem_space,
              const DataSpace &file_space, const DSetMemXferPropList &xfer_plist) const
{
    // Check if this dataset has variable-len string or fixed-len string and
    // proceed appropriately.
    htri_t is_variable_len = H5Tis_variable_str(mem_type.getId());
    if (is_variable_len < 0) {
        throw DataSetIException("DataSet::read", "H5Tis_variable_str failed");
    }

    // Obtain identifiers for C API
    hid_t mem_type_id   = mem_type.getId();
    hid_t mem_space_id  = mem_space.getId();
    hid_t file_space_id = file_space.getId();
    hid_t xfer_plist_id = xfer_plist.getId();

    if (!is_variable_len) // only allocate for fixed-len string
    {
        p_read_fixed_len(mem_type_id, mem_space_id, file_space_id, xfer_plist_id, strg);
    }
    else {
        p_read_variable_len(mem_type_id, mem_space_id, file_space_id, xfer_plist_id, strg);
    }
}

//--------------------------------------------------------------------------
// Function:    DataSet::write
///\brief       Writes raw data from an application buffer to a dataset.
///\param       buf - IN: Buffer containing data to be written
///\param       mem_type - IN: Memory datatype
///\param       mem_space - IN: Memory dataspace
///\param       file_space - IN: Dataset's dataspace in the file
///\param       xfer_plist - IN: Transfer property list for this I/O operation
///\exception   H5::DataSetIException
///\par Description
///             This function writes raw data from an application buffer
///             \a buf to a dataset, converting from memory datatype
///             \a mem_type and dataspace \a mem_space to file datatype
///             and dataspace.
//--------------------------------------------------------------------------
void
DataSet::write(const void *buf, const DataType &mem_type, const DataSpace &mem_space,
               const DataSpace &file_space, const DSetMemXferPropList &xfer_plist) const
{
    // Obtain identifiers for C API
    hid_t mem_type_id   = mem_type.getId();
    hid_t mem_space_id  = mem_space.getId();
    hid_t file_space_id = file_space.getId();
    hid_t xfer_plist_id = xfer_plist.getId();

    herr_t ret_value = H5Dwrite(id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, buf);
    if (ret_value < 0) {
        throw DataSetIException("DataSet::write", "H5Dwrite failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DataSet::write
///\brief       This is an overloaded member function, provided for convenience.
///             It takes a reference to a \c H5std_string for the buffer.
// Modification
//      Jul 2009
//              Modified to pass the buffer into H5Dwrite properly depending
//              whether the dataset has variable- or fixed-length string.
//--------------------------------------------------------------------------
void
DataSet::write(const H5std_string &strg, const DataType &mem_type, const DataSpace &mem_space,
               const DataSpace &file_space, const DSetMemXferPropList &xfer_plist) const
{
    // Check if this attribute has variable-len string or fixed-len string and
    // proceed appropriately.
    htri_t is_variable_len = H5Tis_variable_str(mem_type.getId());
    if (is_variable_len < 0) {
        throw DataSetIException("DataSet::write", "H5Tis_variable_str failed");
    }

    // Obtain identifiers for C API
    hid_t mem_type_id   = mem_type.getId();
    hid_t mem_space_id  = mem_space.getId();
    hid_t file_space_id = file_space.getId();
    hid_t xfer_plist_id = xfer_plist.getId();

    // Convert string to C-string
    const char *strg_C;
    strg_C           = strg.c_str(); // strg_C refers to the contents of strg as a C-str
    herr_t ret_value = 0;

    // Pass string in differently depends on variable or fixed length
    if (!is_variable_len) {
        ret_value = H5Dwrite(id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, strg_C);
    }
    else {
        // passing string argument by address
        ret_value = H5Dwrite(id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, &strg_C);
    }
    if (ret_value < 0) {
        throw DataSetIException("DataSet::write", "H5Dwrite failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DataSet::iterateElems
///\brief       Iterates over all selected elements in a dataspace.
///\param       buf - IN/OUT: Pointer to the buffer in memory containing the
///             elements to iterate over
///\param       type - IN: Datatype for the elements stored in \a buf
///\param       space - IN: Dataspace for \a buf. Also contains the selection
///             to iterate over.
///\param       op - IN: Function pointer to the routine to be called for
///             each element in \a buf iterated over
///\param       op_data - IN/OUT: Pointer to any user-defined data associated
///             with the operation
///\exception   H5::DataSetIException
///\note        This function may not work correctly yet - it's still
///             under development.
//--------------------------------------------------------------------------
int
DataSet::iterateElems(void *buf, const DataType &type, const DataSpace &space, H5D_operator_t op,
                      void *op_data)
{
    // Obtain identifiers for C API
    hid_t  type_id   = type.getId();
    hid_t  space_id  = space.getId();
    herr_t ret_value = H5Diterate(buf, type_id, space_id, op, op_data);
    if (ret_value >= 0)
        return (ret_value);
    else // raise exception when H5Diterate returns a negative value
    {
        throw DataSetIException("DataSet::iterateElems", "H5Diterate failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DataSet::extend
///\brief       Extends a dataset with unlimited dimension.
///\param       size - IN: Array containing the new magnitude of each dimension
///\exception   H5::DataSetIException
///\par Description
///             For information, please refer to the H5Dset_extent API in
///             the HDF5 C Reference Manual.
//--------------------------------------------------------------------------
void
DataSet::extend(const hsize_t *size) const
{
    herr_t ret_value = H5Dset_extent(id, size);
    if (ret_value < 0) // raise exception when H5Dset_extent returns a neg value
        throw DataSetIException("DataSet::extend", "H5Dset_extent failed");
}

//--------------------------------------------------------------------------
// Function:    DataSet::fillMemBuf
///\brief       Fills a selection in memory with a value.
///\param       fill - IN: Pointer to fill value to use - default NULL
///\param       fill_type - IN: Datatype of the fill value
///\param       buf - IN/OUT: Memory buffer to fill selection within
///\param       buf_type - IN: Datatype of the elements in buffer
///\param       space - IN: Dataspace describing memory buffer & containing selection to use
///\exception   H5::DataSetIException
//--------------------------------------------------------------------------
void
DataSet::fillMemBuf(const void *fill, const DataType &fill_type, void *buf, const DataType &buf_type,
                    const DataSpace &space) const
{
    hid_t  fill_type_id = fill_type.getId();
    hid_t  buf_type_id  = buf_type.getId();
    hid_t  space_id     = space.getId();
    herr_t ret_value    = H5Dfill(fill, fill_type_id, buf, buf_type_id, space_id);
    if (ret_value < 0) {
        throw DataSetIException("DataSet::fillMemBuf", "H5Dfill failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DataSet::fillMemBuf
// Purpose      This is an overloaded member function, kept for backward
//              compatibility.  It differs from the above function in that it
//              misses const's.  This wrapper will be removed in future release.
// Param        fill - IN: Pointer to fill value to use - default NULL
// Param        fill_type - IN: Datatype of the fill value
// Param        buf - IN/OUT: Memory buffer to fill selection within
// Param        buf_type - IN: Datatype of the elements in buffer
// Param        space - IN: Dataspace describing memory buffer & containing selection to use
// Exception    H5::DataSetIException
// Modification
//              Modified to call its replacement. -BMR, 2014/04/16
//              Removed from documentation. -BMR, 2016/03/07 1.8.17 and 1.10.0
//              Removed from code. -BMR, 2016/08/11 1.8.18 and 1.10.1
//--------------------------------------------------------------------------
// void DataSet::fillMemBuf(const void *fill, DataType& fill_type, void *buf, DataType& buf_type, DataSpace&
// space)
//{
//    fillMemBuf(fill, (const DataType)fill_type, buf, (const DataType)buf_type, (const DataSpace)space);
//}

//--------------------------------------------------------------------------
// Function:    DataSet::fillMemBuf
///\brief       Fills a selection in memory with 0.
///\param       buf - IN/OUT: Memory buffer to fill selection within
///\param       buf_type - IN: Datatype of the elements in buffer
///\param       space - IN: Dataspace describing memory buffer & containing selection to use
///\exception   H5::DataSetIException
//--------------------------------------------------------------------------
void
DataSet::fillMemBuf(void *buf, const DataType &buf_type, const DataSpace &space) const
{
    hid_t  buf_type_id = buf_type.getId();
    hid_t  space_id    = space.getId();
    herr_t ret_value   = H5Dfill(NULL, buf_type_id, buf, buf_type_id, space_id);
    if (ret_value < 0) {
        throw DataSetIException("DataSet::fillMemBuf", "H5Dfill failed");
    }
}

//--------------------------------------------------------------------------
// Function:    DataSet::fillMemBuf
// Purpose      This is an overloaded member function, kept for backward
//              compatibility.  It differs from the above function in that it
//              misses const's.  This wrapper will be removed in future release.
// Param        buf - IN/OUT: Memory buffer to fill selection within
// Param        buf_type - IN: Datatype of the elements in buffer
// Param        space - IN: Dataspace describing memory buffer & containing selection to use
// Exception    H5::DataSetIException
// Modification
//              Modified to call its replacement. -BMR, 2014/04/16
//              Removed from documentation. -BMR, 2016/03/07 1.8.17 and 1.10.0
//              Removed from code. -BMR, 2016/08/11 1.8.18 and 1.10.1
//--------------------------------------------------------------------------
// void DataSet::fillMemBuf(void *buf, DataType& buf_type, DataSpace& space)
//{
//    fillMemBuf(buf, (const DataType)buf_type, (const DataSpace)space);
//}

//--------------------------------------------------------------------------
// Function:    DataSet::getId
///\brief       Get the id of this dataset.
///\return      DataSet identifier
// Description:
//              Class hierarchy is revised to address bugzilla 1068.  Class
//              AbstractDs and Attribute are moved out of H5Object.  In
//              addition, member IdComponent::id is moved into subclasses, and
//              IdComponent::getId now becomes pure virtual function.
//--------------------------------------------------------------------------
hid_t
DataSet::getId() const
{
    return (id);
}

//--------------------------------------------------------------------------
// Function:    DataSet::p_read_fixed_len (private)
// brief        Reads a fixed length \a H5std_string from a dataset.
// param        mem_type  - IN: DataSet datatype (in memory)
// param        strg      - IN: Buffer for read string
// exceptio n   H5::DataSetIException
// Modification
//      Jul 2009
//              Added in follow to the change in Attribute::read
//--------------------------------------------------------------------------
void
DataSet::p_read_fixed_len(const hid_t mem_type_id, const hid_t mem_space_id, const hid_t file_space_id,
                          const hid_t xfer_plist_id, H5std_string &strg) const
{
    // Only allocate for fixed-len string.

    // Get the size of the dataset's data
    size_t data_size = getInMemDataSize();

    // If there is data, allocate buffer and read it.
    if (data_size > 0) {
        // Create buffer for C string
        char *strg_C = new char[data_size + 1]();

        herr_t ret_value = H5Dread(id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, strg_C);

        if (ret_value < 0) {
            delete[] strg_C; // de-allocate for fixed-len string
            throw DataSetIException("DataSet::read", "H5Dread failed for fixed length string");
        }

        // Get string from the C char* and release resource allocated locally
        strg = strg_C;
        delete[] strg_C;
    }
}

//--------------------------------------------------------------------------
// Function:    DataSet::p_read_variable_len (private)
// brief        Reads a variable length \a H5std_string from an dataset.
// param        mem_type  - IN: DataSet datatype (in memory)
// param        strg      - IN: Buffer for read string
// exception    H5::DataSetIException
// Modification
//      Jul 2009
//              Added in follow to the change in Attribute::read
//--------------------------------------------------------------------------
void
DataSet::p_read_variable_len(const hid_t mem_type_id, const hid_t mem_space_id, const hid_t file_space_id,
                             const hid_t xfer_plist_id, H5std_string &strg) const
{
    // Prepare and call C API to read dataset.
    char *strg_C;

    // Read dataset, no allocation for variable-len string; C library will
    herr_t ret_value = H5Dread(id, mem_type_id, mem_space_id, file_space_id, xfer_plist_id, &strg_C);

    if (ret_value < 0) {
        throw DataSetIException("DataSet::read", "H5Dread failed for variable length string");
    }

    // Get string from the C char* and release resource allocated by C API
    strg = strg_C;
    free(strg_C);
}

#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------------------
// Function:    DataSet::p_setId (protected)
///\brief       Sets the identifier of this dataset to a new value.
///
///\exception   H5::IdComponentException when the attempt to close the HDF5
///             object fails
// Description:
//              The underlaying reference counting in the C library ensures
//              that the current valid id of this object is properly closed.
//              Then the object's id is reset to the new id.
//--------------------------------------------------------------------------
void
DataSet::p_setId(const hid_t new_id)
{
    // handling references to this old id
    try {
        close();
    }
    catch (Exception &close_error) {
        throw DataSetIException(inMemFunc("p_setId"), close_error.getDetailMsg());
    }
    // reset object's id to the given id
    id = new_id;
}

//--------------------------------------------------------------------------
// Function:    f_PropList_setId - friend
// Purpose:     This function is friend to class H5::PropList so that it
//              can set PropList::id in order to work around a problem
//              described in the JIRA issue HDFFV-7947.
//              Applications shouldn't need to use it.
// param        dset   - IN/OUT: DataSet object to be changed
// param        new_id - IN: New id to set
//--------------------------------------------------------------------------
void
f_PropList_setId(PropList *plist, hid_t new_id)
{
    plist->p_setId(new_id);
}

#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:    DataSet::close
///\brief       Closes this dataset.
///
///\exception   H5::DataSetIException
//--------------------------------------------------------------------------
void
DataSet::close()
{
    if (p_valid_id(id)) {
        herr_t ret_value = H5Dclose(id);
        if (ret_value < 0) {
            throw DataSetIException("DataSet::close", "H5Dclose failed");
        }
        // reset the id
        id = H5I_INVALID_HID;
    }
}

//--------------------------------------------------------------------------
// Function:    DataSet destructor
///\brief       Properly terminates access to this dataset.
// Modification
//              - Replaced resetIdComponent() with decRefCount() to use C
//              library ID reference counting mechanism - BMR, Jun 1, 2004
//              - Replaced decRefCount with close() to let the C library
//              handle the reference counting - BMR, Jun 1, 2006
//--------------------------------------------------------------------------
DataSet::~DataSet()
{
    try {
        close();
    }
    catch (Exception &close_error) {
        cerr << "DataSet::~DataSet - " << close_error.getDetailMsg() << endl;
    }
}

} // namespace H5

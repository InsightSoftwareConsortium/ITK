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
#include "H5CommonFG.h"
#include "H5Group.h"
#include "H5AbstractDs.h"
#include "H5DataSpace.h"
#include "H5DataSet.h"
#include "H5File.h"
#include "H5Alltypes.h"

namespace H5 {
using std::cerr;
using std::endl;

//--------------------------------------------------------------------------
// Function     H5File default constructor
///\brief       Default constructor: creates a stub H5File object.
// December 2000
//--------------------------------------------------------------------------
H5File::H5File() : Group(), id(H5I_INVALID_HID)
{
}

//--------------------------------------------------------------------------
// Function:    H5File overloaded constructor
///\brief       Creates or opens an HDF5 file depending on the parameter flags.
///\param       name         - IN: Name of the file
///\param       flags        - IN: File access flags
///\param       create_plist - IN: File creation property list, used when
///             modifying default file meta-data.  Default to
///             FileCreatPropList::DEFAULT
///\param       access_plist - IN: File access property list.  Default to
///             FileAccPropList::DEFAULT
///\par Description
///             Valid values of \a flags include:
///             \li \c H5F_ACC_TRUNC - Truncate file, if it already exists,
///                             erasing all data previously stored in
///                             the file.
///             \li \c H5F_ACC_EXCL - Fail if file already exists.
///                 \c H5F_ACC_TRUNC and \c H5F_ACC_EXCL are mutually exclusive
///             \li \c H5F_ACC_RDONLY - Open file as read-only, if it already
///                             exists, and fail, otherwise
///             \li \c H5F_ACC_RDWR - Open file for read/write, if it already
///                             exists, and fail, otherwise
///\par
///             For info on file creation in the case of an already-open file,
///             please refer to the \b Special \b case section of the H5Fcreate
///             API in the C Reference Manual.
// Notes        With a PGI compiler (~2012-2013,) the exception thrown by
//              p_get_file could not be caught in the applications.  Added try
//              block here to catch then re-throw it. -BMR 2013/03/21
// December 2000
//--------------------------------------------------------------------------
H5File::H5File(const char *name, unsigned int flags, const FileCreatPropList &create_plist,
               const FileAccPropList &access_plist)
    : Group(), id(H5I_INVALID_HID)
{
    try {
        p_get_file(name, flags, create_plist, access_plist);
    }
    catch (FileIException &open_file) {
        throw open_file;
    }
}

//--------------------------------------------------------------------------
// Function:    H5File overloaded constructor
///\brief       This is another overloaded constructor.  It differs from the
///             above constructor only in the type of the \a name argument.
///\param       name - IN: Name of the file - \c H5std_string
///\param       flags - IN: File access flags
///\param       create_plist - IN: File creation property list, used when
///             modifying default file meta-data.  Default to
///             FileCreatPropList::DEFAULT
///\param       access_plist - IN: File access property list.  Default to
///             FileAccPropList::DEFAULT
// Notes        With a PGI compiler (~2012-2013,) the exception thrown by
//              p_get_file could not be caught in the applications.  Added try
//              block here to catch then re-throw it. -BMR 2013/03/21
// December 2000
//--------------------------------------------------------------------------
H5File::H5File(const H5std_string &name, unsigned int flags, const FileCreatPropList &create_plist,
               const FileAccPropList &access_plist)
    : Group(), id(H5I_INVALID_HID)
{
    try {
        p_get_file(name.c_str(), flags, create_plist, access_plist);
    }
    catch (FileIException &open_file) {
        throw open_file;
    }
}

#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------------------
// This function is private and contains common code between the
// constructors taking a string or a char*
// Modification
//              - removed H5F_ACC_CREAT because H5Fcreate will fail with
//              H5F_ACC_CREAT. - BMR, Sep 17, 2014
//--------------------------------------------------------------------------
void
H5File::p_get_file(const char *name, unsigned int flags, const FileCreatPropList &create_plist,
                   const FileAccPropList &access_plist)
{
    // These bits only set for creation, so if any of them are set,
    // create the file.
    if (flags & (H5F_ACC_EXCL | H5F_ACC_TRUNC)) {
        hid_t create_plist_id = create_plist.getId();
        hid_t access_plist_id = access_plist.getId();
        id                    = H5Fcreate(name, flags, create_plist_id, access_plist_id);
        if (id < 0) // throw an exception when open/create fail
        {
            throw FileIException("H5File constructor", "H5Fcreate failed");
        }
    }
    // Open the file if none of the bits above are set.
    else {
        hid_t access_plist_id = access_plist.getId();
        id                    = H5Fopen(name, flags, access_plist_id);
        if (id < 0) // throw an exception when open/create fail
        {
            throw FileIException("H5File constructor", "H5Fopen failed");
        }
    }
}

//--------------------------------------------------------------------------
// Function:    H5File overloaded constructor
///\brief       Creates an H5File object using an existing file id.
///\param       existing_id - IN: Id of an existing file
// Description
//      Mar 29, 2015
//              Added in responding to a request from user Jason Newton.
//              However, it is not recommended to use the private member "id"
//              in applications.  Unlike other situations, where similar
//              constructor is needed by the library in order to return
//              an object, H5File doesn't need it. -BMR (HDFFV-8766 partially)
//--------------------------------------------------------------------------
H5File::H5File(hid_t existing_id) : Group(), id{existing_id}
{
    incRefCount(); // increment number of references to this id
}

#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:    H5File copy constructor
///\brief       Copy constructor: same HDF5 object as \a original
///             H5File object.
///\param       original - IN: H5File instance to copy
// December 2000
//--------------------------------------------------------------------------
H5File::H5File(const H5File &original) : Group(original), id{original.getId()}
{
    incRefCount(); // increment number of references to this id
}

//--------------------------------------------------------------------------
// Function:    H5File::isHdf5 (static)
///\brief       Determines whether a file in HDF5 format. (Static)
///\param       name - IN: Name of the file
///\return      true if the file is in HDF5 format, and false, otherwise
///\exception   H5::FileIException
// December 2000
//--------------------------------------------------------------------------
bool
H5File::isHdf5(const char *name)
{
    // Calls C routine H5Fis_accessible to determine whether the file is in
    // HDF5 format.  It returns positive value, 0, or negative value
    htri_t ret_value = H5Fis_accessible(name, H5P_DEFAULT);

    if (ret_value > 0)
        return true;
    else if (ret_value == 0)
        return false;
    else // Raise exception when H5Fis_accessible returns a negative value
    {
        throw FileIException("H5File::isHdf5", "H5Fis_accessible returned negative value");
    }
}

//--------------------------------------------------------------------------
// Function:    H5File::isHdf5 (static)
///\brief       This is an overloaded member function, provided for convenience.
///             It takes an \c H5std_string for \a name. (Static)
///\param       name - IN: Name of the file - \c H5std_string
// December 2000
//--------------------------------------------------------------------------
bool
H5File::isHdf5(const H5std_string &name)
{
    return (isHdf5(name.c_str()));
}

//--------------------------------------------------------------------------
// Function:    H5File::isAccessible (static)
///\brief       Determines whether a file can be accessed as HDF5. (Static)
///\param       name - IN: Name of the file
///\param       access_plist - IN: File access property list.  Default to
///             FileAccPropList::DEFAULT
///\return      true if the file can be accessed as HDF5, and false, otherwise
///\exception   H5::FileIException
// September 2018
//--------------------------------------------------------------------------
bool
H5File::isAccessible(const char *name, const FileAccPropList &access_plist)
{
    // Calls C routine H5Fis_accessible to determine whether the file is in
    // HDF5 format.  It returns positive value, 0, or negative value
    hid_t  access_plist_id = access_plist.getId();
    htri_t ret_value       = H5Fis_accessible(name, access_plist_id);
    if (ret_value > 0)
        return true;
    else if (ret_value == 0)
        return false;
    else // Raise exception when H5Fis_accessible returns a negative value
    {
        throw FileIException("H5File::isAccessible", "H5Fis_accessible returned negative value");
    }
}

//--------------------------------------------------------------------------
// Function:    H5File::isAccessible (static)
///\brief       This is an overloaded member function, provided for convenience.
///             It takes an \c H5std_string for \a name. (Static)
///\param       name - IN: Name of the file - \c H5std_string
///\param       access_plist - IN: File access property list.  Default to
///             FileAccPropList::DEFAULT
// September 2018
//--------------------------------------------------------------------------
bool
H5File::isAccessible(const H5std_string &name, const FileAccPropList &access_plist)
{
    return (isAccessible(name.c_str(), access_plist));
}

//--------------------------------------------------------------------------
// Function:    openFile
///\brief       Opens an HDF5 file
///\param       name         - IN: Name of the file
///\param       flags        - IN: File access flags
///\param       access_plist - IN: File access property list.  Default to
///             FileAccPropList::DEFAULT
///\par Description
///             Valid values of \a flags include:
///             H5F_ACC_RDWR:   Open with read/write access. If the file is
///                     currently open for read-only access then it
///                     will be reopened. Absence of this flag
///                     implies read-only access.
///
///             H5F_ACC_RDONLY: Open with read only access. - default
///
// October 2005
//--------------------------------------------------------------------------
void
H5File::openFile(const char *name, unsigned int flags, const FileAccPropList &access_plist)
{
    try {
        close();
    }
    catch (Exception &close_error) {
        throw FileIException("H5File::openFile", close_error.getDetailMsg());
    }

    hid_t access_plist_id = access_plist.getId();
    id                    = H5Fopen(name, flags, access_plist_id);
    if (id < 0) // throw an exception when open fails
    {
        throw FileIException("H5File::openFile", "H5Fopen failed");
    }
}

//--------------------------------------------------------------------------
// Function:    H5File::openFile
///\brief       This is an overloaded member function, provided for convenience.
///             It takes an \c H5std_string for \a name.
///\param       name         - IN: Name of the file - \c H5std_string
///\param       flags        - IN: File access flags
///\param       access_plist - IN: File access property list.  Default to
///             FileAccPropList::DEFAULT
// December 2000
//--------------------------------------------------------------------------
void
H5File::openFile(const H5std_string &name, unsigned int flags, const FileAccPropList &access_plist)
{
    openFile(name.c_str(), flags, access_plist);
}

//--------------------------------------------------------------------------
// Function:    H5File::reOpen
///\brief       Reopens this file.
///
///\exception   H5::FileIException
// Description
//              If this object has represented another HDF5 file, the previous
//              HDF5 file need to be closed first.
// Note:        This wrapper doesn't seem right regarding the 'id' and should
//              be investigated.  BMR - 2/20/2005
// Modification
//              - Replaced resetIdComponent() with decRefCount() to use C
//              library ID reference counting mechanism - BMR, Feb 20, 2005
//              - Replaced decRefCount with close() to let the C library
//              handle the reference counting - BMR, Jun 1, 2006
//--------------------------------------------------------------------------
void
H5File::reOpen()
{
    try {
        close();
    }
    catch (Exception &close_error) {
        throw FileIException("H5File::reOpen", close_error.getDetailMsg());
    }

    // call C routine to reopen the file - Note: not sure about this,
    // which id to be the parameter when closing?
    id = H5Freopen(id);
    if (id < 0) // Raise exception when H5Freopen returns a neg value
        throw FileIException("H5File::reOpen", "H5Freopen failed");
}

//--------------------------------------------------------------------------
// Function:    H5File::getCreatePlist
///\brief       Returns a copy of the creation property list of this file
///\return      FileCreatPropList object
///\exception   H5::FileIException
// December 2000
//--------------------------------------------------------------------------
FileCreatPropList
H5File::getCreatePlist() const
{
    hid_t create_plist_id = H5Fget_create_plist(id);

    // if H5Fget_create_plist returns a valid id, create and return
    // the FileCreatPropList object for this property list
    if (create_plist_id > 0) {
        FileCreatPropList create_plist(create_plist_id);
        return (create_plist);
    }
    else {
        throw FileIException("H5File::getCreatePlist", "H5Fget_create_plist failed");
    }
}

//--------------------------------------------------------------------------
// Function:    H5File::getAccessPlist
///\brief       Returns a copy of the access property list of this file
///\return      FileAccPropList object
///\exception   H5::FileIException
// December 2000
//--------------------------------------------------------------------------
FileAccPropList
H5File::getAccessPlist() const
{
    hid_t access_plist_id = H5Fget_access_plist(id);

    // if H5Fget_access_plist returns a valid id, create and return
    // the FileAccPropList object for this property list
    if (access_plist_id > 0) {
        FileAccPropList access_plist(access_plist_id);
        return access_plist;
    }
    else // Raise an exception
    {
        throw FileIException("H5File::getAccessPlist", "H5Fget_access_plist failed");
    }
}

//--------------------------------------------------------------------------
// Function:    H5File::getFileInfo
///\brief       Retrieves the general information of this file.
///
///\exception   H5::FileIException
///\par Description
///             The retrieved information may include information about
///             superblock extension, free space management, and shared object
// February 2017
//--------------------------------------------------------------------------
void
H5File::getFileInfo(H5F_info2_t &file_info) const
{
    herr_t ret_value = H5Fget_info2(id, &file_info);
    if (ret_value < 0) {
        throw FileIException("H5File::getFileInfo", "H5Fget_info2 failed");
    }
}

//--------------------------------------------------------------------------
// Function:    H5File::getFreeSpace
///\brief       Returns the amount of free space in the file.
///\return      Amount of free space
///\exception   H5::FileIException
// May 2004
//--------------------------------------------------------------------------
hssize_t
H5File::getFreeSpace() const
{
    hssize_t free_space = H5Fget_freespace(id);
    if (free_space < 0) {
        throw FileIException("H5File::getFreeSpace", "H5Fget_freespace failed");
    }
    return (free_space);
}

//--------------------------------------------------------------------------
// Function:    H5File::getObjCount
///\brief       Returns the number of opened object IDs (files, datasets,
///             groups and datatypes) in the same file.
///\param       types - Type of object to retrieve the count
///\return      Number of opened object IDs
///\exception   H5::FileIException
///\par Description
///             The valid values for \a types include:
///             \li \c H5F_OBJ_FILE     - Files only
///             \li \c H5F_OBJ_DATASET  - Datasets only
///             \li \c H5F_OBJ_GROUP    - Groups only
///             \li \c H5F_OBJ_DATATYPE - Named datatypes only
///             \li \c H5F_OBJ_ATTR     - Attributes only
///             \li \c H5F_OBJ_ALL    - All of the above, i.e., \c H5F_OBJ_FILE
///                         | \c H5F_OBJ_DATASET | \c H5F_OBJ_GROUP
///                         | \c H5F_OBJ_DATATYPE | \c H5F_OBJ_ATTR
///\par
/// Multiple object types can be combined with the logical OR operator (|).
// May 2004
//--------------------------------------------------------------------------
ssize_t
H5File::getObjCount(unsigned types) const
{
    ssize_t num_objs = H5Fget_obj_count(id, types);
    if (num_objs < 0) {
        throw FileIException("H5File::getObjCount", "H5Fget_obj_count failed");
    }
    return (num_objs);
}

//--------------------------------------------------------------------------
// Function:    H5File::getObjIDs
///\brief       Retrieves a list of opened object IDs (files, datasets,
///             groups and datatypes) in the same file.
///\param       types    - Type of object to retrieve the count
///\param       max_objs - Maximum number of object identifiers to place
///                        into obj_id_list.
///\param       oid_list - List of open object identifiers
///\exception   H5::FileIException
///\par Description
///             The valid values for \a types include:
///             \li \c H5F_OBJ_FILE - Files only
///             \li \c H5F_OBJ_DATASET - Datasets only
///             \li \c H5F_OBJ_GROUP - Groups only
///             \li \c H5F_OBJ_DATATYPE - Named datatypes only
///             \li \c H5F_OBJ_ATTR - Attributes only
///             \li \c H5F_OBJ_ALL    - All of the above, i.e., \c H5F_OBJ_FILE
///                     | \c H5F_OBJ_DATASET | \c H5F_OBJ_GROUP
///                     | \c H5F_OBJ_DATATYPE | \c H5F_OBJ_ATTR
///\par
/// Multiple object types can be combined with the logical OR operator (|).
//
// Notes: will do the overload for this one after hearing from Quincey???
// May 2004
//--------------------------------------------------------------------------
void
H5File::getObjIDs(unsigned types, size_t max_objs, hid_t *oid_list) const
{
    ssize_t ret_value = H5Fget_obj_ids(id, types, max_objs, oid_list);
    if (ret_value < 0) {
        throw FileIException("H5File::getObjIDs", "H5Fget_obj_ids failed");
    }
}

//--------------------------------------------------------------------------
// Function:    H5File::getVFDHandle
///\brief       Returns the pointer to the file handle of the low-level file
///             driver.
///\param       fapl        - File access property list
///\param       file_handle - Pointer to the file handle being used by
///                           the low-level virtual file driver
///\exception   H5::FileIException
///\par Description
///             For the \c FAMILY or \c MULTI drivers, \a fapl should be
///             defined through the property list functions:
///             \c FileAccPropList::setFamilyOffset for the \c FAMILY driver
///             and \c FileAccPropList::setMultiType for the \c MULTI driver.
///
///             The obtained file handle is dynamic and is valid only while
///             the file remains open; it will be invalid if the file is
///             closed and reopened or opened during a subsequent session.
// May 2004
//--------------------------------------------------------------------------
void
H5File::getVFDHandle(const FileAccPropList &fapl, void **file_handle) const
{
    hid_t  fapl_id   = fapl.getId();
    herr_t ret_value = H5Fget_vfd_handle(id, fapl_id, file_handle);
    if (ret_value < 0) {
        throw FileIException("H5File::getVFDHandle", "H5Fget_vfd_handle failed");
    }
}

//--------------------------------------------------------------------------
// Function:    H5File::getVFDHandle
// Purpose      This is an overloaded member function, kept for backward
//              compatibility.  It differs from the above function in that it
//              misses const's.  This wrapper will be removed in future release.
// Param        fapl        - File access property list
// Param        file_handle - Pointer to the file handle being used by
//                            the low-level virtual file driver
// Exception    H5::FileIException
// May 2004
// Modification
//              Planned for removal. -BMR, 2014/04/16
//              Removed from documentation. -BMR, 2016/03/07 1.8.17 and 1.10.0
//              Removed from code. -BMR, 2016/08/11 1.8.18 and 1.10.1
//--------------------------------------------------------------------------
// void H5File::getVFDHandle(FileAccPropList& fapl, void **file_handle) const
//{
//    getVFDHandle((const FileAccPropList)fapl, file_handle);
//}

//--------------------------------------------------------------------------
// Function:    H5File::getVFDHandle
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function only in what arguments it
///             accepts.
///\param       file_handle - Pointer to the file handle being used by
///                     the low-level virtual file driver
///\exception   H5::FileIException
// May 2004
//--------------------------------------------------------------------------
void
H5File::getVFDHandle(void **file_handle) const
{
    herr_t ret_value = H5Fget_vfd_handle(id, H5P_DEFAULT, file_handle);
    if (ret_value < 0) {
        throw FileIException("H5File::getVFDHandle", "H5Fget_vfd_handle failed");
    }
}

//--------------------------------------------------------------------------
// Function:    H5File::getFileSize
///\brief       Returns the file size of the HDF5 file.
///\return      File size
///\exception   H5::FileIException
///\par Description
///             This function is called after an existing file is opened in
///             order to learn the true size of the underlying file.
// Programmer   Raymond Lu - June 24, 2004
//--------------------------------------------------------------------------
hsize_t
H5File::getFileSize() const
{
    hsize_t file_size;
    herr_t  ret_value = H5Fget_filesize(id, &file_size);
    if (ret_value < 0) {
        throw FileIException("H5File::getFileSize", "H5Fget_filesize failed");
    }
    return (file_size);
}

//--------------------------------------------------------------------------
// Function:    H5File::getFileNum
///\brief       Returns the file number of the HDF5 file.
///\return      File number
///\exception   H5::FileIException
///\par Description
///             This function is called after an existing file is opened in
///             order to retrieve the unique 'file number' for the file.
// Programmer   Quincey Koziol - April 13, 2019
//--------------------------------------------------------------------------
unsigned long
H5File::getFileNum() const
{
    unsigned long fileno    = 0;
    herr_t        ret_value = H5Fget_fileno(id, &fileno);
    if (ret_value < 0) {
        throw FileIException("H5File::getFileNum", "H5Fget_fileno failed");
    }
    return (fileno);
}

//--------------------------------------------------------------------------
// Function:    H5File::getId
///\brief       Get the id of this file
///\return      File identifier
// Modification
//      May 2008 - BMR
//              Class hierarchy is revised to address bugzilla 1068.  Class
//              AbstractDS and Attribute are moved out of H5Object.  In
//              addition, member IdComponent::id is moved into subclasses, and
//              IdComponent::getId now becomes pure virtual function.
// May, 2008
//--------------------------------------------------------------------------
hid_t
H5File::getId() const
{
    return (id);
}

#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------------------
// Function:    H5File::reopen
// Purpose      Reopens this file.
// Exception    H5::FileIException
// Description
//              This function is replaced by the above function reOpen.
// December 2000
//--------------------------------------------------------------------------
void
H5File::reopen()
{
    H5File::reOpen();
}

//--------------------------------------------------------------------------
// Function:    H5File::getLocId
// Purpose      Get the id of this file
// Description
//              This function is a redefinition of CommonFG::getLocId.  It
//              is used by CommonFG member functions to get the file id.
// December 2000
// Deprecated:
//      Aug 18, 2016 -BMR
//              After HDFFV-9920, the Group's methods can use getId() and
//              getLocId() is kept for backward compatibility.
//--------------------------------------------------------------------------
hid_t
H5File::getLocId() const
{
    return (getId());
}

//--------------------------------------------------------------------------
// Function:    H5File::p_setId (protected)
///\brief       Sets the identifier of this object to a new value.
///
///\exception   H5::IdComponentException when the attempt to close the HDF5
///             object fails
// Description
//              The underlaying reference counting in the C library ensures
//              that the current valid id of this object is properly closed.
//              Then the object's id is reset to the new id.
// December 2000
//--------------------------------------------------------------------------
void
H5File::p_setId(const hid_t new_id)
{
    // handling references to this old id
    try {
        close();
    }
    catch (Exception &E) {
        throw FileIException("H5File::p_setId", E.getDetailMsg());
    }
    // reset object's id to the given id
    id = new_id;
}
#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:    H5File::close
///\brief       Closes this HDF5 file.
///
///\exception   H5::FileIException
// March 2005
//--------------------------------------------------------------------------
void
H5File::close()
{
    if (p_valid_id(id)) {
        herr_t ret_value = H5Fclose(id);
        if (ret_value < 0) {
            throw FileIException("H5File::close", "H5Fclose failed");
        }
        // reset the id
        id = H5I_INVALID_HID;
    }
}

//--------------------------------------------------------------------------
// Function:    H5File::throwException
///\brief       Throws file exception - initially implemented for CommonFG
///\param       func_name - Name of the function where failure occurs
///\param       msg       - Message describing the failure
///\exception   H5::FileIException
// Description
//              This function is also used in H5Location implementation so that
//              proper exception can be thrown for file or group.  The
//              "H5File::" will be inserted to indicate the function called is
//              an implementation of H5File.
// December 2000
//--------------------------------------------------------------------------
void
H5File::throwException(const H5std_string &func_name, const H5std_string &msg) const
{
    H5std_string full_name = func_name;
    full_name.insert(0, "H5File::");
    throw FileIException(full_name, msg);
}

//--------------------------------------------------------------------------
// Function:    H5File destructor
///\brief       Properly terminates access to this file.
// December 2000
// Modification
//              - Replaced resetIdComponent() with decRefCount() to use C
//              library ID reference counting mechanism - BMR, Feb 20, 2005
//              - Replaced decRefCount with close() to let the C library
//              handle the reference counting - BMR, Jun 1, 2006
//--------------------------------------------------------------------------
H5File::~H5File()
{
    try {
        close();
    }
    catch (Exception &close_error) {
        cerr << "H5File::~H5File - " << close_error.getDetailMsg() << endl;
    }
}

} // namespace H5

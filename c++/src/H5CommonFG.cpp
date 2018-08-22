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

#include "H5private.h"             // for HDstrcpy
#include "H5Include.h"
#include "H5Exception.h"
#include "H5IdComponent.h"
#include "H5DataSpace.h"
#include "H5PropList.h"
#include "H5DxferProp.h"
#include "H5OcreatProp.h"
#include "H5DcreatProp.h"
#include "H5LcreatProp.h"
#include "H5LaccProp.h"
#include "H5DaccProp.h"
#include "H5Location.h"
#include "H5Object.h"
#include "H5Alltypes.h"
#include "H5AbstractDs.h"
#include "H5DataSet.h"
#include "H5CommonFG.h"

// There are a few comments that are common to most of the functions
// defined in this file so they are listed here.
// - getLocId is called by all functions, that call a C API, to get
//   the location id, which can be either a file id or a group id.
//   This function is pure virtual and it's up to H5File and Group
//   to call the right getId() - although, as the structure of the
//   library at this time, getId() is basically the IdComponent::getId()
// - when a failure returned by the C API, the functions will call
//   throwException, which is a pure virtual function and is implemented
//   by H5File to throw a FileIException and by Group to throw a
//   GroupIException.
// December 2000

namespace H5 {

//--------------------------------------------------------------------------
// Function:    CommonFG::openDataType
///\brief       Opens the named generic datatype at this location.
///\param       name  - IN: Name of the datatype to open
///\return      DataType instance
///\exception   H5::FileIException or H5::GroupIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataType CommonFG::openDataType(const char* name) const
{
    // Call C function H5Topen2 to open the named datatype in this group,
    // given either the file or group id
    hid_t type_id = H5Topen2(getLocId(), name, H5P_DEFAULT);

    // If the datatype's opening failed, throw an exception
    if (type_id < 0)
        throwException("openDataType", "H5Topen2 failed");

    // No failure, create and return the DataType object
    DataType data_type;
    f_DataType_setId(&data_type, type_id);
    return(data_type);
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openDataType
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a name.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
DataType CommonFG::openDataType(const H5std_string& name) const
{
    return(openDataType(name.c_str()));
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openArrayType
///\brief       Opens the named array datatype at this location.
///\param       name  - IN: Name of the array datatype to open
///\return      ArrayType instance
///\exception   H5::FileIException or H5::GroupIException
// Programmer   Binh-Minh Ribler - Jul, 2005
//--------------------------------------------------------------------------
ArrayType CommonFG::openArrayType(const char* name) const
{
    // Call C function H5Topen2 to open the named datatype in this group,
    // given either the file or group id
    hid_t type_id = H5Topen2(getLocId(), name, H5P_DEFAULT);

    // If the datatype's opening failed, throw an exception
    if (type_id < 0)
        throwException("openArrayType", "H5Topen2 failed");

    // No failure, create and return the ArrayType object
    ArrayType array_type;
    f_DataType_setId(&array_type, type_id);
    return(array_type);
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openArrayType
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a name.
// Programmer   Binh-Minh Ribler - Jul, 2005
//--------------------------------------------------------------------------
ArrayType CommonFG::openArrayType(const H5std_string& name) const
{
    return(openArrayType(name.c_str()));
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openCompType
///\brief       Opens the named compound datatype at this location.
///\param       name  - IN: Name of the compound datatype to open
///\return      CompType instance
///\exception   H5::FileIException or H5::GroupIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CompType CommonFG::openCompType(const char* name) const
{
    // Call C function H5Topen2 to open the named datatype in this group,
    // given either the file or group id
    hid_t type_id = H5Topen2(getLocId(), name, H5P_DEFAULT);

    // If the datatype's opening failed, throw an exception
    if (type_id < 0)
        throwException("openCompType", "H5Topen2 failed");

    // No failure, create and return the CompType object
    CompType comp_type;
    f_DataType_setId(&comp_type, type_id);
    return(comp_type);
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openCompType
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a name.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CompType CommonFG::openCompType(const H5std_string& name) const
{
    return(openCompType(name.c_str()));
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openEnumType
///\brief       Opens the named enumeration datatype at this location.
///\param       name  - IN: Name of the enumeration datatype to open
///\return      EnumType instance
///\exception   H5::FileIException or H5::GroupIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
EnumType CommonFG::openEnumType(const char* name) const
{
    // Call C function H5Topen2 to open the named datatype in this group,
    // given either the file or group id
    hid_t type_id = H5Topen2(getLocId(), name, H5P_DEFAULT);

    // If the datatype's opening failed, throw an exception
    if (type_id < 0)
        throwException("openEnumType", "H5Topen2 failed");

    // No failure, create and return the EnumType object
    EnumType enum_type;
    f_DataType_setId(&enum_type, type_id);
    return(enum_type);
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openEnumType
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a name.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
EnumType CommonFG::openEnumType(const H5std_string& name) const
{
    return(openEnumType(name.c_str()));
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openIntType
///\brief       Opens the named integer datatype at this location.
///\param       name  - IN: Name of the integer datatype to open
///\return      IntType instance
///\exception   H5::FileIException or H5::GroupIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IntType CommonFG::openIntType(const char* name) const
{
    // Call C function H5Topen2 to open the named datatype in this group,
    // given either the file or group id
    hid_t type_id = H5Topen2(getLocId(), name, H5P_DEFAULT);

    // If the datatype's opening failed, throw an exception
    if (type_id < 0)
        throwException("openIntType", "H5Topen2 failed");

    // No failure, create and return the IntType object
    IntType int_type;
    f_DataType_setId(&int_type, type_id);
    return(int_type);
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openIntType
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a name.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IntType CommonFG::openIntType(const H5std_string& name) const
{
    return(openIntType(name.c_str()));
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openFloatType
///\brief       Opens the named floating-point datatype at this location.
///\param       name  - IN: Name of the floating-point datatype to open
///\return      FloatType instance
///\exception   H5::FileIException or H5::GroupIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
FloatType CommonFG::openFloatType(const char* name) const
{
    // Call C function H5Topen2 to open the named datatype in this group,
    // given either the file or group id
    hid_t type_id = H5Topen2(getLocId(), name, H5P_DEFAULT);

    // If the datatype's opening failed, throw an exception
    if (type_id < 0)
        throwException("openFloatType", "H5Topen2 failed");

    // No failure, create and return the FloatType object
    FloatType float_type;
    f_DataType_setId(&float_type, type_id);
    return(float_type);
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openFloatType
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a name.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
FloatType CommonFG::openFloatType(const H5std_string& name) const
{
    return(openFloatType(name.c_str()));
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openStrType
///\brief       Opens the named string datatype at this location.
///\param       name  - IN: Name of the string datatype to open
///\return      StrType instance
///\exception   H5::FileIException or H5::GroupIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrType CommonFG::openStrType(const char* name) const
{
    // Call C function H5Topen2 to open the named datatype in this group,
    // given either the file or group id
    hid_t type_id = H5Topen2(getLocId(), name, H5P_DEFAULT);

    // If the datatype's opening failed, throw an exception
    if (type_id < 0)
        throwException("openStrType", "H5Topen2 failed");

    // No failure, create and return the StrType object
    StrType str_type;
    f_DataType_setId(&str_type, type_id);
    return(str_type);
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openStrType
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a name.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrType CommonFG::openStrType(const H5std_string& name) const
{
    return(openStrType(name.c_str()));
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openVarLenType
///\brief       Opens the named variable length datatype at this location.
///\param       name  - IN: Name of the variable length datatype to open
///\return      VarLenType instance
///\exception   H5::FileIException or H5::GroupIException
// Programmer   Binh-Minh Ribler - Jul, 2005
//--------------------------------------------------------------------------
VarLenType CommonFG::openVarLenType(const char* name) const
{
    // Call C function H5Topen2 to open the named datatype in this group,
    // given either the file or group id
    hid_t type_id = H5Topen2(getLocId(), name, H5P_DEFAULT);

    // If the datatype's opening failed, throw an exception
    if (type_id < 0)
        throwException("openVarLenType", "H5Topen2 failed");

    // No failure, create and return the VarLenType object
    VarLenType varlen_type;
    f_DataType_setId(&varlen_type, type_id);
    return(varlen_type);
}

//--------------------------------------------------------------------------
// Function:    CommonFG::openVarLenType
///\brief       This is an overloaded member function, provided for convenience.
///             It differs from the above function in that it takes an
///             \c H5std_string for \a name.
// Programmer   Binh-Minh Ribler - Jul, 2005
//--------------------------------------------------------------------------
VarLenType CommonFG::openVarLenType(const H5std_string& name) const
{
    return(openVarLenType(name.c_str()));
}

#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------------------
// Function:    CommonFG default constructor
///\brief       Default constructor.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CommonFG::CommonFG() {}

//--------------------------------------------------------------------------
// Function:    CommonFG destructor
///\brief       Noop destructor.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
CommonFG::~CommonFG() {}

//--------------------------------------------------------------------------
// Function:    f_DataType_setId - friend
// Purpose:     This function is friend to class H5::DataType so that it
//              can set DataType::id in order to work around a problem
//              described in the JIRA issue HDFFV-7947.
//              Applications shouldn't need to use it.
// param        dtype   - IN/OUT: DataType object to be changed
// param        new_id - IN: New id to set
// Programmer   Binh-Minh Ribler - 2015
//--------------------------------------------------------------------------
void f_DataType_setId(DataType* dtype, hid_t new_id)
{
    dtype->p_setId(new_id);
}

//--------------------------------------------------------------------------
// Function:    f_DataSet_setId - friend
// Purpose:     This function is friend to class H5::DataSet so that it
//              can set DataSet::id in order to work around a problem
//              described in the JIRA issue HDFFV-7947.
//              Applications shouldn't need to use it.
// param        dset   - IN/OUT: DataSet object to be changed
// param        new_id - IN: New id to set
// Programmer   Binh-Minh Ribler - 2015
//--------------------------------------------------------------------------
void f_DataSet_setId(DataSet* dset, hid_t new_id)
{
    dset->p_setId(new_id);
}

#endif // DOXYGEN_SHOULD_SKIP_THIS

} // end namespace

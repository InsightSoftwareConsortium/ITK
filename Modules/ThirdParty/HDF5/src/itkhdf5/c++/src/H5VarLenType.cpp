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
#include "H5DcreatProp.h"
#include "H5LcreatProp.h"
#include "H5LaccProp.h"
#include "H5DaccProp.h"
#include "H5Location.h"
#include "H5Object.h"
#include "H5DataType.h"
#include "H5VarLenType.h"

namespace H5 {

//--------------------------------------------------------------------------
// Function:    VarLenType default constructor
///\brief       Default constructor: Creates a stub variable-length datatype.
//--------------------------------------------------------------------------
VarLenType::VarLenType() : DataType() {}

//--------------------------------------------------------------------------
// Function:    VarLenType overloaded constructor
///\brief       Creates an VarLenType object using an existing id.
///\param       existing_id - IN: Id of an existing datatype
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
VarLenType::VarLenType(const hid_t existing_id) : DataType(existing_id) {}

//--------------------------------------------------------------------------
// Function:    VarLenType copy constructor
///\brief       Copy constructor: same HDF5 object as \a original
// Programmer   Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
VarLenType::VarLenType(const VarLenType& original) : DataType(original) {}

//--------------------------------------------------------------------------
// Function:    VarLenType overloaded constructor
///\brief       Deprecated - will be removed after 1.10.2
///\param       base_type - IN: Pointer to existing datatype
///\exception   H5::DataTypeIException
// Description
//              DataType passed by pointer to avoid clashing with copy
//              constructor.
//              Updated: this is unnecessary.
//              -BMR, Sep, 2017
// Programmer   Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
VarLenType::VarLenType(const DataType* base_type) : DataType()
{
    id = H5Tvlen_create(base_type->getId());
    if (id < 0)
    {
        throw DataTypeIException("VarLenType constructor",
                "H5Tvlen_create returns negative value");
    }
}

//--------------------------------------------------------------------------
// Function:    VarLenType overloaded constructor
///\brief       Creates a new variable-length datatype based on the specified
///             \a base_type.
///\param       base_type - IN: An existing datatype
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
VarLenType::VarLenType(const DataType& base_type) : DataType()
{
    id = H5Tvlen_create(base_type.getId());
    if (id < 0)
    {
        throw DataTypeIException("VarLenType constructor",
                "H5Tvlen_create returns negative value");
    }
}

//--------------------------------------------------------------------------
// Function:    VarLenType overloaded constructor
///\brief       Creates an VarLenType instance by opening an HDF5 variable
///             length datatype given its name, provided as a C char*.
///\param       loc        - IN: Location of the type
///\param       dtype_name - IN: Variable length type name
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - Dec 2016
// Description
//              In 1.10.1, this constructor was introduced and may replace the
//              existing function CommonFG::openVarLenType(const char*) to
//              improve usability.
//              -BMR, Dec 2016
//--------------------------------------------------------------------------
VarLenType::VarLenType(const H5Location& loc, const char *dtype_name) : DataType()
{
    id = p_opentype(loc, dtype_name);
}

//--------------------------------------------------------------------------
// Function:    VarLenType overloaded constructor
///\brief       Creates an VarLenType instance by opening an HDF5 variable
///             length datatype given its name, provided as an \c H5std_string.
///\param       loc        - IN: Location of the type
///\param       dtype_name - IN: Variable length type name
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - Dec 2016
// Description
//              In 1.10.1, this constructor was introduced and may replace the
//              existing function CommonFG::openVarLenType(const H5std_string&)
//              to improve usability.
//              -BMR, Dec 2016
//--------------------------------------------------------------------------
VarLenType::VarLenType(const H5Location& loc, const H5std_string& dtype_name) : DataType()
{
    id = p_opentype(loc, dtype_name.c_str());
}

//--------------------------------------------------------------------------
// Function:    VarLenType::decode
///\brief       Returns an VarLenType object via DataType* by decoding the
///             binary object description of this type.
///
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - Aug 2017
//--------------------------------------------------------------------------
DataType* VarLenType::decode() const
{
    hid_t encoded_vltype_id = H5I_INVALID_HID;
    try {
        encoded_vltype_id = p_decode();
    }
    catch (DataTypeIException &err) {
        throw;
    }
    VarLenType *encoded_vltype = new VarLenType;
    encoded_vltype->p_setId(encoded_vltype_id);
    return(encoded_vltype);
}

//--------------------------------------------------------------------------
// Function:    VarLenType destructor
///\brief       Properly terminates access to this datatype.
// Programmer   Binh-Minh Ribler - May, 2004
//--------------------------------------------------------------------------
VarLenType::~VarLenType() {}

} // end namespace

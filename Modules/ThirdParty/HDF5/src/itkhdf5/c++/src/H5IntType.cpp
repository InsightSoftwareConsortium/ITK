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
#include "H5DxferProp.h"
#include "H5LcreatProp.h"
#include "H5LaccProp.h"
#include "H5DaccProp.h"
#include "H5Location.h"
#include "H5Object.h"
#include "H5DataType.h"
#include "H5AbstractDs.h"
#include "H5DataSpace.h"
#include "H5AtomType.h"
#include "H5IntType.h"
#include "H5DataSet.h"
#include "H5PredType.h"

namespace H5 {

//--------------------------------------------------------------------------
// Function:    IntType default constructor
///\brief       Default constructor: Creates a stub integer datatype
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IntType::IntType() {}

//--------------------------------------------------------------------------
// Function:    IntType copy constructor
///\brief       Copy constructor: same HDF5 object as \a original
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IntType::IntType(const IntType& original) : AtomType( original ) {}

//--------------------------------------------------------------------------
// Function:    IntType overloaded constructor
///\brief       Creates a integer type using a predefined type
///\param       pred_type - IN: Predefined datatype
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IntType::IntType(const PredType& pred_type) : AtomType()
{
    // use DataType::copy to make a copy of this predefined type
    copy(pred_type);
}

//--------------------------------------------------------------------------
// Function:    IntType overloaded constructor
///\brief       Creates an integer datatype using the id of an existing
///             datatype.
///\param       existing_id - IN: Id of an existing datatype
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IntType::IntType(const hid_t existing_id) : AtomType( existing_id ) {}

//--------------------------------------------------------------------------
// Function:    IntType overloaded constructor
///\brief       Gets the integer datatype of the specified dataset.
///\param       dataset - IN: Dataset that this integer datatype associates with
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IntType::IntType(const DataSet& dataset) : AtomType()
{
    // Calls C function H5Dget_type to get the id of the datatype
    id = H5Dget_type(dataset.getId());
 
    if (id < 0)
    {
        throw DataSetIException("IntType constructor", "H5Dget_type failed");
    }
}

//--------------------------------------------------------------------------
// Function:    IntType overloaded constructor
///\brief       Creates a IntType instance by opening an HDF5 integer datatype
///             given its name as a char*.
///\param       loc        - IN: Location of the type
///\param       dtype_name - IN: Integer type name
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - Dec 2016
// Description
//              In 1.10.1, this constructor was introduced and may replace the
//              existing function CommonFG::openIntType(const char*) to
//              improve usability.
//              -BMR, Dec 2016
//--------------------------------------------------------------------------
IntType::IntType(const H5Location& loc, const char *dtype_name) : AtomType()
{
    id = p_opentype(loc, dtype_name);
}

//--------------------------------------------------------------------------
// Function:    IntType overloaded constructor
///\brief       Creates a IntType instance by opening an HDF5 integer datatype
///             given its name, provided as an \c H5std_string.
///\param       loc        - IN: Location of the type
///\param       dtype_name - IN: Integer type name
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - Dec 2016
// Description
//              In 1.10.1, this constructor was introduced and may replace the
//              existing function CommonFG::openIntType(const H5std_string&)
//              to improve usability.
//              -BMR, Dec 2016
//--------------------------------------------------------------------------
IntType::IntType(const H5Location& loc, const H5std_string& dtype_name) : AtomType()
{
    id = p_opentype(loc, dtype_name.c_str());
}

//--------------------------------------------------------------------------
// Function:    IntType::decode
///\brief       Returns an IntType object via DataType* by decoding the
///             binary object description of this type.
///
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - Aug 2017
//--------------------------------------------------------------------------
DataType* IntType::decode() const
{
    hid_t encoded_inttype_id = H5I_INVALID_HID;
    try {
        encoded_inttype_id = p_decode();
    }
    catch (DataTypeIException &err) {
        throw;
    }
    IntType *encoded_inttype = new IntType;
    encoded_inttype->p_setId(encoded_inttype_id);
    return(encoded_inttype);
}

//--------------------------------------------------------------------------
// Function:    IntType::getSign
///\brief       Retrieves the sign type for an integer type.
///\return      Valid sign type
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5T_sign_t IntType::getSign() const
{
    H5T_sign_t type_sign = H5Tget_sign(id);  // C routine

    // Returns a valid sign type if no errors
    if (type_sign == H5T_SGN_ERROR)
    {
        throw DataTypeIException("IntType::getSign",
            "H5Tget_sign failed - returned H5T_SGN_ERROR for the sign type");
    }
    return(type_sign);
}

//--------------------------------------------------------------------------
// Function:    IntType::getSign
///\brief       Sets the sign property for an integer type.
///\param       sign - IN: Sign type
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void IntType::setSign(H5T_sign_t sign) const
{
    // Call C routine to set the sign property
    herr_t ret_value = H5Tset_sign(id, sign);
    if (ret_value < 0)
    {
        throw DataTypeIException("IntType::setSign", "H5Tset_sign failed");
    }
}

//--------------------------------------------------------------------------
// Function:    IntType destructor
///\brief       Noop destructor.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
IntType::~IntType() {}

} // end namespace

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
#include "H5AtomType.h"
#include "H5AbstractDs.h"
#include "H5DataSpace.h"
#include "H5StrType.h"
#include "H5DataSet.h"
#include "H5PredType.h"

namespace H5 {

//--------------------------------------------------------------------------
// Function:    StrType default constructor
///\brief       Default constructor: Creates a stub string datatype
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrType::StrType() : AtomType() {}

//--------------------------------------------------------------------------
// Function:    StrType overloaded constructor
///\brief       Creates a string datatype using a predefined type.
///\param       pred_type - IN: Predefined datatype
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrType::StrType(const PredType& pred_type) : AtomType()
{
    // use DataType::copy to make a copy of this predefined type
    copy(pred_type);
}

//--------------------------------------------------------------------------
// Function:    StrType overloaded constructor
// Purpose      Creates a string datatype with a specified length
// Param        pred_type - IN: String predefined type to replicate.
// Param        size     - IN: Length of the new string type
// Exception    H5::DataTypeIException
// Description
//              The 1st argument could have been skipped, but this
//              constructor will collide with the one that takes an
//              existing id.
//
//              Update: replacing the 1st argument with a dummy 0 to
//              avoid the clashing problem, that doesn't eliminate the
//              the 1st argument but it's simpler for the user to type
//              a '0' than PredType::C_S1.  - Dec 2, 2005
// Note
//              The use of this constructor can be shortened by using
//              its overloaded below as StrType(0, size).
// Programmer   Binh-Minh Ribler - 2000
// Modification
//              Planned for removal. -BMR, 2005/12/02
//              Removed from documentation. -BMR, 2016/03/07
//--------------------------------------------------------------------------
StrType::StrType(const PredType& pred_type, const size_t& size) : AtomType()
{
    // use DataType::copy to make a copy of the string predefined type
    // then set its length
    copy(pred_type);
    setSize(size);
}

//--------------------------------------------------------------------------
// Function:    StrType overloaded constructor
///\brief       Creates a string datatype with a specified length
///\param       dummy - IN: To simplify calling the previous constructor
///                         and avoid prototype clash with another constructor
///\param       size  - IN: Length of the new string type
///\exception   H5::DataTypeIException
///\par Description
///             The 1st argument is just a dummy to simplify calling the
///             previous constructor, such as:
///             StrType atype(0, size) instead of
///             StrType atype(PredType::C_S1, size)
// Note
//              This constructor replaced the previous one.
// Programmer   Binh-Minh Ribler - Nov 28, 2005
//--------------------------------------------------------------------------
StrType::StrType(const int dummy, const size_t& size) : AtomType()
{
    // use DataType::copy to make a copy of the string predefined type
    // then set its length
    copy(PredType::C_S1);
    setSize(size);
}

//--------------------------------------------------------------------------
// Function:    StrType overloaded constructor
///\brief       Creates an StrType object using the id of an existing datatype.
///\param       existing_id - IN: Id of an existing datatype
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrType::StrType(const hid_t existing_id) : AtomType( existing_id ) {}

//--------------------------------------------------------------------------
// Function:    StrType copy constructor
///\brief       Copy constructor: same HDF5 object as \a original
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrType::StrType(const StrType& original) : AtomType ( original ) {}

//--------------------------------------------------------------------------
// Function:    StrType overloaded constructor
///\brief       Gets the string datatype of the specified dataset
///\param       dataset - IN: Dataset that this string datatype associates with
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrType::StrType(const DataSet& dataset) : AtomType ()
{
    // Calls C function H5Dget_type to get the id of the datatype
    id = H5Dget_type(dataset.getId());

    if (id < 0)
    {
        throw DataSetIException("StrType constructor", "H5Dget_type failed");
    }
}

//--------------------------------------------------------------------------
// Function:    StrType overloaded constructor
///\brief       Creates an StrType instance by opening an HDF5 string datatype
///             given its name, provided as a C character string.
///\param       loc        - IN: Location of the type
///\param       dtype_name - IN: String type name
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - Dec 2016
// Description
//              In 1.10.1, this constructor was introduced and may replace the
//              existing function CommonFG::openStrType(const char*) to
//              improve usability.
//              -BMR, Dec 2016
//--------------------------------------------------------------------------
StrType::StrType(const H5Location& loc, const char *dtype_name) : AtomType()
{
    id = p_opentype(loc, dtype_name);
}

//--------------------------------------------------------------------------
// Function:    StrType overloaded constructor
///\brief       Creates an StrType instance by opening an HDF5 string datatype
///             given its name, provided as an \c H5std_string.
///\param       loc        - IN: Location of the type
///\param       dtype_name - IN: String type name
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - Dec 2016
// Description
//              In 1.10.1, this constructor was introduced and may replace the
//              existing function CommonFG::openStrType(const H5std_string&)
//              to improve usability.
//              -BMR, Dec 2016
//--------------------------------------------------------------------------
StrType::StrType(const H5Location& loc, const H5std_string& dtype_name) : AtomType()
{
    id = p_opentype(loc, dtype_name.c_str());
}

//--------------------------------------------------------------------------
// Function:    StrType::decode
///\brief       Returns an StrType object via DataType* by decoding the
///             binary object description of this type.
///
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - Aug 2017
//--------------------------------------------------------------------------
DataType* StrType::decode() const
{
    hid_t encoded_strtype_id = H5I_INVALID_HID;
    try {
        encoded_strtype_id = p_decode();
    }
    catch (DataTypeIException &err) {
        throw;
    }
    StrType *encoded_strtype = new StrType;
    encoded_strtype->p_setId(encoded_strtype_id);
    return(encoded_strtype);
}

//--------------------------------------------------------------------------
// Function:    StrType::getCset
///\brief       Retrieves the character set type of this string datatype.
///\return      Character set type, which can be:
///             \li \c H5T_CSET_ASCII (0) - Character set is US ASCII.
///\note
///     ASCII and UTF-8 Unicode are the only currently supported character
///     encodings. Extended ASCII encodings (for example, ISO 8859) are not
///     supported. This encoding policy is not enforced by the HDF5 Library.
///     Using encodings other than ASCII and UTF-8 can lead to compatibility
///     and usability problems. See the C API entry H5Pset_char_encoding for
///     more information.
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5T_cset_t StrType::getCset() const
{
    H5T_cset_t cset = H5Tget_cset(id);

    // Returns a valid character set type if successful
    if (cset == H5T_CSET_ERROR)
    {
        throw DataTypeIException("StrType::getCset", "H5Tget_cset failed");
    }
    return(cset);
}

//--------------------------------------------------------------------------
// Function:    StrType::setCset
///\brief       Sets character set to be used.
///\param       cset - IN: character set type, which can be:
///             \li \c H5T_CSET_ASCII (0) - Character set is US ASCII.
///\note
///     ASCII and UTF-8 Unicode are the only currently supported character
///     encodings. Extended ASCII encodings (for example, ISO 8859) are not
///     supported. This encoding policy is not enforced by the HDF5 Library.
///     Using encodings other than ASCII and UTF-8 can lead to compatibility
///     and usability problems. See the C API entry H5Pset_char_encoding for
///     more information.
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void StrType::setCset(H5T_cset_t cset) const
{
    herr_t ret_value = H5Tset_cset(id, cset);
    if (ret_value < 0)
    {
        throw DataTypeIException("StrType::setCset", "H5Tset_cset failed");
    }
}

//--------------------------------------------------------------------------
// Function:    StrType::getStrpad
///\brief       Retrieves the storage mechanism for of this string datatype.
///\return      String storage mechanism, which can be:
///             \li \c H5T_STR_NULLTERM (0) - Null terminate (as C does)
///             \li \c H5T_STR_NULLPAD (0) - Pad with zeros
///             \li \c H5T_STR_SPACEPAD (0) - pad with spaces (as FORTRAN does)
///\exception   H5::DataTypeIException
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
H5T_str_t StrType::getStrpad() const
{
    H5T_str_t strpad = H5Tget_strpad(id);

    // Returns a valid string padding type if successful
    if (strpad == H5T_STR_ERROR)
    {
        throw DataTypeIException("StrType::getStrpad",
            "H5Tget_strpad failed - returned H5T_STR_ERROR");
    }
    return(strpad);
}

//--------------------------------------------------------------------------
// Function:    StrType::setStrpad
///\brief       Defines the storage mechanism for this string datatype.
///\param       strpad - IN: String padding type
///\exception   H5::DataTypeIException
///\par Description
///             For information, please refer to the H5Tset_strpad API in
///             the HDF5 C Reference Manual.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
void StrType::setStrpad(H5T_str_t strpad) const
{
    herr_t ret_value = H5Tset_strpad(id, strpad);
    if (ret_value < 0)
    {
        throw DataTypeIException("StrType::setStrpad", "H5Tset_strpad failed");
    }
}

//--------------------------------------------------------------------------
// Function:    StrType destructor
///\brief       Properly terminates access to this string datatype.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
StrType::~StrType() {}

} // end namespace

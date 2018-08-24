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
#include "H5AtomType.h"
#include "H5PredType.h"

namespace H5 {

#ifndef DOXYGEN_SHOULD_SKIP_THIS
//--------------------------------------------------------------------------
// Function:    PredType overloaded constructor
///\brief       Creates a PredType object using the id of an existing
///             predefined datatype.
///\param       predtype_id - IN: Id of a predefined datatype
// Description
//              This constructor creates a PredType object by copying
//              the provided HDF5 predefined datatype.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PredType::PredType(const hid_t predtype_id) : AtomType(predtype_id)
{
    id = H5Tcopy(predtype_id);
}

//--------------------------------------------------------------------------
// Function:    PredType default constructor
///\brief       Default constructor: Creates a stub predefined datatype
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PredType::PredType() : AtomType() {}
#endif // DOXYGEN_SHOULD_SKIP_THIS

//--------------------------------------------------------------------------
// Function:    PredType copy constructor
///\brief       Copy constructor: same HDF5 object as \a original
///\param       original - IN: PredType instance to copy
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PredType::PredType(const PredType& original) : AtomType(original) {}

//--------------------------------------------------------------------------
// Function:    PredType::operator=
///\brief       Assignment operator.
///\param       rhs - IN: Reference to the predefined datatype
///\return      Reference to PredType instance
///\exception   H5::DataTypeIException
// Description
//              Makes a copy of the type on the right hand side and stores
//              the new id in the left hand side object.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PredType& PredType::operator=(const PredType& rhs)
{
    if (this != &rhs)
        copy(rhs);
    return(*this);
}

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// These dummy functions do not inherit from DataType - they'll
// throw an DataTypeIException if invoked.
void PredType::commit(H5Location& loc, const char* name)
{
    throw DataTypeIException("PredType::commit", "Error: Attempted to commit a predefined datatype.  Invalid operation!");
}

void PredType::commit(H5Location& loc, const H5std_string& name)
{
    commit(loc, name.c_str());
}

bool PredType::committed()
{
    throw DataTypeIException("PredType::committed", "Error: Attempting to check for commit status on a predefined datatype.");
}
#endif // DOXYGEN_SHOULD_SKIP_THIS

// Default destructor
//--------------------------------------------------------------------------
// Function:    PredType destructor
///\brief       Noop destructor.
// Programmer   Binh-Minh Ribler - 2000
//--------------------------------------------------------------------------
PredType::~PredType() {}

/*****************************************************************************
        The following section is regarding the global constants PredType,
        DataSpace, and PropList.

 *****************************************************************************/

#ifndef DOXYGEN_SHOULD_SKIP_THIS
// Definition pointers for the constants
PredType* PredType::PREDTYPE_CONST_ = 0; //dummy
PredType* PredType::STD_I8BE_;
PredType* PredType::STD_I8LE_;
PredType* PredType::STD_I16BE_;
PredType* PredType::STD_I16LE_;
PredType* PredType::STD_I32BE_;
PredType* PredType::STD_I32LE_;
PredType* PredType::STD_I64BE_;
PredType* PredType::STD_I64LE_;
PredType* PredType::STD_U8BE_;
PredType* PredType::STD_U8LE_;
PredType* PredType::STD_U16BE_;
PredType* PredType::STD_U16LE_;
PredType* PredType::STD_U32BE_;
PredType* PredType::STD_U32LE_;
PredType* PredType::STD_U64BE_;
PredType* PredType::STD_U64LE_;
PredType* PredType::STD_B8BE_;
PredType* PredType::STD_B8LE_;
PredType* PredType::STD_B16BE_;
PredType* PredType::STD_B16LE_;
PredType* PredType::STD_B32BE_;
PredType* PredType::STD_B32LE_;
PredType* PredType::STD_B64BE_;
PredType* PredType::STD_B64LE_;
PredType* PredType::STD_REF_OBJ_;
PredType* PredType::STD_REF_DSETREG_;

PredType* PredType::C_S1_;
PredType* PredType::FORTRAN_S1_;

PredType* PredType::IEEE_F32BE_;
PredType* PredType::IEEE_F32LE_;
PredType* PredType::IEEE_F64BE_;
PredType* PredType::IEEE_F64LE_;

PredType* PredType::UNIX_D32BE_;
PredType* PredType::UNIX_D32LE_;
PredType* PredType::UNIX_D64BE_;
PredType* PredType::UNIX_D64LE_;

PredType* PredType::INTEL_I8_;
PredType* PredType::INTEL_I16_;
PredType* PredType::INTEL_I32_;
PredType* PredType::INTEL_I64_;
PredType* PredType::INTEL_U8_;
PredType* PredType::INTEL_U16_;
PredType* PredType::INTEL_U32_;
PredType* PredType::INTEL_U64_;
PredType* PredType::INTEL_B8_;
PredType* PredType::INTEL_B16_;
PredType* PredType::INTEL_B32_;
PredType* PredType::INTEL_B64_;
PredType* PredType::INTEL_F32_;
PredType* PredType::INTEL_F64_;

PredType* PredType::ALPHA_I8_;
PredType* PredType::ALPHA_I16_;
PredType* PredType::ALPHA_I32_;
PredType* PredType::ALPHA_I64_;
PredType* PredType::ALPHA_U8_;
PredType* PredType::ALPHA_U16_;
PredType* PredType::ALPHA_U32_;
PredType* PredType::ALPHA_U64_;
PredType* PredType::ALPHA_B8_;
PredType* PredType::ALPHA_B16_;
PredType* PredType::ALPHA_B32_;
PredType* PredType::ALPHA_B64_;
PredType* PredType::ALPHA_F32_;
PredType* PredType::ALPHA_F64_;

PredType* PredType::MIPS_I8_;
PredType* PredType::MIPS_I16_;
PredType* PredType::MIPS_I32_;
PredType* PredType::MIPS_I64_;
PredType* PredType::MIPS_U8_;
PredType* PredType::MIPS_U16_;
PredType* PredType::MIPS_U32_;
PredType* PredType::MIPS_U64_;
PredType* PredType::MIPS_B8_;
PredType* PredType::MIPS_B16_;
PredType* PredType::MIPS_B32_;
PredType* PredType::MIPS_B64_;
PredType* PredType::MIPS_F32_;
PredType* PredType::MIPS_F64_;

PredType* PredType::NATIVE_CHAR_;
PredType* PredType::NATIVE_SCHAR_;
PredType* PredType::NATIVE_UCHAR_;
PredType* PredType::NATIVE_SHORT_;
PredType* PredType::NATIVE_USHORT_;
PredType* PredType::NATIVE_INT_;
PredType* PredType::NATIVE_UINT_;
PredType* PredType::NATIVE_LONG_;
PredType* PredType::NATIVE_ULONG_;
PredType* PredType::NATIVE_LLONG_;
PredType* PredType::NATIVE_ULLONG_;
PredType* PredType::NATIVE_FLOAT_;
PredType* PredType::NATIVE_DOUBLE_;
PredType* PredType::NATIVE_LDOUBLE_;
PredType* PredType::NATIVE_B8_;
PredType* PredType::NATIVE_B16_;
PredType* PredType::NATIVE_B32_;
PredType* PredType::NATIVE_B64_;
PredType* PredType::NATIVE_OPAQUE_;
PredType* PredType::NATIVE_HSIZE_;
PredType* PredType::NATIVE_HSSIZE_;
PredType* PredType::NATIVE_HERR_;
PredType* PredType::NATIVE_HBOOL_;

PredType* PredType::NATIVE_INT8_;
PredType* PredType::NATIVE_UINT8_;
PredType* PredType::NATIVE_INT16_;
PredType* PredType::NATIVE_UINT16_;
PredType* PredType::NATIVE_INT32_;
PredType* PredType::NATIVE_UINT32_;
PredType* PredType::NATIVE_INT64_;
PredType* PredType::NATIVE_UINT64_;
// LEAST types
#if H5_SIZEOF_INT_LEAST8_T != 0
PredType* PredType::NATIVE_INT_LEAST8_;
#endif /* H5_SIZEOF_INT_LEAST8_T */
#if H5_SIZEOF_UINT_LEAST8_T != 0
PredType* PredType::NATIVE_UINT_LEAST8_;
#endif /* H5_SIZEOF_UINT_LEAST8_T */

#if H5_SIZEOF_INT_LEAST16_T != 0
PredType* PredType::NATIVE_INT_LEAST16_;
#endif /* H5_SIZEOF_INT_LEAST16_T */
#if H5_SIZEOF_UINT_LEAST16_T != 0
PredType* PredType::NATIVE_UINT_LEAST16_;
#endif /* H5_SIZEOF_UINT_LEAST16_T */

#if H5_SIZEOF_INT_LEAST32_T != 0
PredType* PredType::NATIVE_INT_LEAST32_;
#endif /* H5_SIZEOF_INT_LEAST32_T */
#if H5_SIZEOF_UINT_LEAST32_T != 0
PredType* PredType::NATIVE_UINT_LEAST32_;
#endif /* H5_SIZEOF_UINT_LEAST32_T */

#if H5_SIZEOF_INT_LEAST64_T != 0
PredType* PredType::NATIVE_INT_LEAST64_;
#endif /* H5_SIZEOF_INT_LEAST64_T */
#if H5_SIZEOF_UINT_LEAST64_T != 0
PredType* PredType::NATIVE_UINT_LEAST64_;
#endif /* H5_SIZEOF_UINT_LEAST64_T */

// FAST types
#if H5_SIZEOF_INT_FAST8_T != 0
PredType* PredType::NATIVE_INT_FAST8_;
#endif /* H5_SIZEOF_INT_FAST8_T */
#if H5_SIZEOF_UINT_FAST8_T != 0
PredType* PredType::NATIVE_UINT_FAST8_;
#endif /* H5_SIZEOF_UINT_FAST8_T */

#if H5_SIZEOF_INT_FAST16_T != 0
PredType* PredType::NATIVE_INT_FAST16_;
#endif /* H5_SIZEOF_INT_FAST16_T */
#if H5_SIZEOF_UINT_FAST16_T != 0
PredType* PredType::NATIVE_UINT_FAST16_;
#endif /* H5_SIZEOF_UINT_FAST16_T */

#if H5_SIZEOF_INT_FAST32_T != 0
PredType* PredType::NATIVE_INT_FAST32_;
#endif /* H5_SIZEOF_INT_FAST32_T */
#if H5_SIZEOF_UINT_FAST32_T != 0
PredType* PredType::NATIVE_UINT_FAST32_;
#endif /* H5_SIZEOF_UINT_FAST32_T */

#if H5_SIZEOF_INT_FAST64_T != 0
PredType* PredType::NATIVE_INT_FAST64_;
#endif /* H5_SIZEOF_INT_FAST64_T */
#if H5_SIZEOF_UINT_FAST64_T != 0
PredType* PredType::NATIVE_UINT_FAST64_;
#endif /* H5_SIZEOF_UINT_FAST64_T */

//--------------------------------------------------------------------------
// Function:    PredType::getPredTypes
// Purpose      Returns the dummy PredType constant object pointer
// Return:      PredType object pointer
// Description
//              If the dummy constant PREDTYPE_CONST_ is not allocated yet,
//              call makePredTypes() to allocate all of the PredType constants.
//              Otherwise, just simply return the object pointer PREDTYPE_CONST_.
//
//              Note that, there is a similar function to getPredTypes() in
//              other classes, that have global constants, is called getConstant().
//
// Programmer   Binh-Minh Ribler - September 2015
//--------------------------------------------------------------------------
PredType* PredType::getPredTypes()
{
    // Tell the C library not to clean up, H5Library::termH5cpp will call
    // H5close - more dependency if use H5Library::dontAtExit()
    if (!IdComponent::H5dontAtexit_called)
    {
        (void) H5dont_atexit();
        IdComponent::H5dontAtexit_called = true;
    }

    // If the dummy constant pointer is not allocated, allocate all PredType
    // constant pointers.  Otherwise, throw because it shouldn't be.
    if (PREDTYPE_CONST_ == 0)
        makePredTypes();
    else
        throw H5::DataTypeIException("PredType::getPredTypes", "PredType::getPredTypes is being invoked on an allocated PREDTYPE_CONST_");
    return PREDTYPE_CONST_;
}

//--------------------------------------------------------------------------
// Function:    PredType::makePredTypes
// Purpose      Allocate all PredType constants.
// Programmer   Binh-Minh Ribler - September 2015
//--------------------------------------------------------------------------
void PredType::makePredTypes()
{
    PREDTYPE_CONST_ = new PredType;
    C_S1_ = new PredType(H5T_C_S1);
    FORTRAN_S1_ = new PredType(H5T_FORTRAN_S1);

    STD_I8BE_ = new PredType(H5T_STD_I8BE);
    STD_I8LE_ = new PredType(H5T_STD_I8LE);
    STD_I16BE_ = new PredType(H5T_STD_I16BE);
    STD_I16LE_ = new PredType(H5T_STD_I16LE);
    STD_I32BE_ = new PredType(H5T_STD_I32BE);
    STD_I32LE_ = new PredType(H5T_STD_I32LE);
    STD_I64BE_ = new PredType(H5T_STD_I64BE);
    STD_I64LE_ = new PredType(H5T_STD_I64LE);
    STD_U8BE_ = new PredType(H5T_STD_U8BE);
    STD_U8LE_ = new PredType(H5T_STD_U8LE);
    STD_U16BE_ = new PredType(H5T_STD_U16BE);
    STD_U16LE_ = new PredType(H5T_STD_U16LE);
    STD_U32BE_ = new PredType(H5T_STD_U32BE);
    STD_U32LE_ = new PredType(H5T_STD_U32LE);
    STD_U64BE_ = new PredType(H5T_STD_U64BE);
    STD_U64LE_ = new PredType(H5T_STD_U64LE);
    STD_B8BE_ = new PredType(H5T_STD_B8BE);
    STD_B8LE_ = new PredType(H5T_STD_B8LE);

    STD_B16BE_ = new PredType(H5T_STD_B16BE);
    STD_B16LE_ = new PredType(H5T_STD_B16LE);
    STD_B32BE_ = new PredType(H5T_STD_B32BE);
    STD_B32LE_ = new PredType(H5T_STD_B32LE);
    STD_B64BE_ = new PredType(H5T_STD_B64BE);
    STD_B64LE_ = new PredType(H5T_STD_B64LE);
    STD_REF_OBJ_ = new PredType(H5T_STD_REF_OBJ);
    STD_REF_DSETREG_ = new PredType(H5T_STD_REF_DSETREG);

    IEEE_F32BE_ = new PredType(H5T_IEEE_F32BE);
    IEEE_F32LE_ = new PredType(H5T_IEEE_F32LE);
    IEEE_F64BE_ = new PredType(H5T_IEEE_F64BE);
    IEEE_F64LE_ = new PredType(H5T_IEEE_F64LE);

    UNIX_D32BE_ = new PredType(H5T_UNIX_D32BE);
    UNIX_D32LE_ = new PredType(H5T_UNIX_D32LE);
    UNIX_D64BE_ = new PredType(H5T_UNIX_D64BE);
    UNIX_D64LE_ = new PredType(H5T_UNIX_D64LE);

    INTEL_I8_ = new PredType(H5T_INTEL_I8);
    INTEL_I16_ = new PredType(H5T_INTEL_I16);
    INTEL_I32_ = new PredType(H5T_INTEL_I32);
    INTEL_I64_ = new PredType(H5T_INTEL_I64);
    INTEL_U8_ = new PredType(H5T_INTEL_U8);
    INTEL_U16_ = new PredType(H5T_INTEL_U16);
    INTEL_U32_ = new PredType(H5T_INTEL_U32);
    INTEL_U64_ = new PredType(H5T_INTEL_U64);
    INTEL_B8_ = new PredType(H5T_INTEL_B8);
    INTEL_B16_ = new PredType(H5T_INTEL_B16);
    INTEL_B32_ = new PredType(H5T_INTEL_B32);
    INTEL_B64_ = new PredType(H5T_INTEL_B64);
    INTEL_F32_ = new PredType(H5T_INTEL_F32);
    INTEL_F64_ = new PredType(H5T_INTEL_F64);

    ALPHA_I8_ = new PredType(H5T_ALPHA_I8);
    ALPHA_I16_ = new PredType(H5T_ALPHA_I16);
    ALPHA_I32_ = new PredType(H5T_ALPHA_I32);
    ALPHA_I64_ = new PredType(H5T_ALPHA_I64);
    ALPHA_U8_ = new PredType(H5T_ALPHA_U8);
    ALPHA_U16_ = new PredType(H5T_ALPHA_U16);
    ALPHA_U32_ = new PredType(H5T_ALPHA_U32);
    ALPHA_U64_ = new PredType(H5T_ALPHA_U64);
    ALPHA_B8_ = new PredType(H5T_ALPHA_B8);
    ALPHA_B16_ = new PredType(H5T_ALPHA_B16);
    ALPHA_B32_ = new PredType(H5T_ALPHA_B32);
    ALPHA_B64_ = new PredType(H5T_ALPHA_B64);
    ALPHA_F32_ = new PredType(H5T_ALPHA_F32);
    ALPHA_F64_ = new PredType(H5T_ALPHA_F64);

    MIPS_I8_ = new PredType(H5T_MIPS_I8);
    MIPS_I16_ = new PredType(H5T_MIPS_I16);
    MIPS_I32_ = new PredType(H5T_MIPS_I32);
    MIPS_I64_ = new PredType(H5T_MIPS_I64);
    MIPS_U8_ = new PredType(H5T_MIPS_U8);
    MIPS_U16_ = new PredType(H5T_MIPS_U16);
    MIPS_U32_ = new PredType(H5T_MIPS_U32);
    MIPS_U64_ = new PredType(H5T_MIPS_U64);
    MIPS_B8_ = new PredType(H5T_MIPS_B8);
    MIPS_B16_ = new PredType(H5T_MIPS_B16);
    MIPS_B32_ = new PredType(H5T_MIPS_B32);
    MIPS_B64_ = new PredType(H5T_MIPS_B64);
    MIPS_F32_ = new PredType(H5T_MIPS_F32);
    MIPS_F64_ = new PredType(H5T_MIPS_F64);

    NATIVE_CHAR_ = new PredType(H5T_NATIVE_CHAR);
    NATIVE_INT_ = new PredType(H5T_NATIVE_INT);
    NATIVE_FLOAT_ = new PredType(H5T_NATIVE_FLOAT);
    NATIVE_SCHAR_ = new PredType(H5T_NATIVE_SCHAR);
    NATIVE_UCHAR_ = new PredType(H5T_NATIVE_UCHAR);
    NATIVE_SHORT_ = new PredType(H5T_NATIVE_SHORT);
    NATIVE_USHORT_ = new PredType(H5T_NATIVE_USHORT);
    NATIVE_UINT_ = new PredType(H5T_NATIVE_UINT);
    NATIVE_LONG_ = new PredType(H5T_NATIVE_LONG);
    NATIVE_ULONG_ = new PredType(H5T_NATIVE_ULONG);
    NATIVE_LLONG_ = new PredType(H5T_NATIVE_LLONG);
    NATIVE_ULLONG_ = new PredType(H5T_NATIVE_ULLONG);
    NATIVE_DOUBLE_ = new PredType(H5T_NATIVE_DOUBLE);
#if H5_SIZEOF_LONG_DOUBLE !=0
    NATIVE_LDOUBLE_ = new PredType(H5T_NATIVE_LDOUBLE);
#endif
    NATIVE_B8_ = new PredType(H5T_NATIVE_B8);
    NATIVE_B16_ = new PredType(H5T_NATIVE_B16);
    NATIVE_B32_ = new PredType(H5T_NATIVE_B32);
    NATIVE_B64_ = new PredType(H5T_NATIVE_B64);
    NATIVE_OPAQUE_ = new PredType(H5T_NATIVE_OPAQUE);
    NATIVE_HSIZE_ = new PredType(H5T_NATIVE_HSIZE);
    NATIVE_HSSIZE_ = new PredType(H5T_NATIVE_HSSIZE);
    NATIVE_HERR_ = new PredType(H5T_NATIVE_HERR);
    NATIVE_HBOOL_ = new PredType(H5T_NATIVE_HBOOL);

    NATIVE_INT8_ = new PredType(H5T_NATIVE_INT8);
    NATIVE_UINT8_ = new PredType(H5T_NATIVE_UINT8);
    NATIVE_INT16_ = new PredType(H5T_NATIVE_INT16);
    NATIVE_UINT16_ = new PredType(H5T_NATIVE_UINT16);
    NATIVE_INT32_ = new PredType(H5T_NATIVE_INT32);
    NATIVE_UINT32_ = new PredType(H5T_NATIVE_UINT32);
    NATIVE_INT64_ = new PredType(H5T_NATIVE_INT64);
    NATIVE_UINT64_ = new PredType(H5T_NATIVE_UINT64);

// LEAST types
#if H5_SIZEOF_INT_LEAST8_T != 0
    NATIVE_INT_LEAST8_ = new PredType(H5T_NATIVE_INT_LEAST8);
#endif /* H5_SIZEOF_INT_LEAST8_T */
#if H5_SIZEOF_UINT_LEAST8_T != 0
    NATIVE_UINT_LEAST8_ = new PredType(H5T_NATIVE_UINT_LEAST8);
#endif /* H5_SIZEOF_UINT_LEAST8_T */

#if H5_SIZEOF_INT_LEAST16_T != 0
    NATIVE_INT_LEAST16_ = new PredType(H5T_NATIVE_INT_LEAST16);
#endif /* H5_SIZEOF_INT_LEAST16_T */
#if H5_SIZEOF_UINT_LEAST16_T != 0
    NATIVE_UINT_LEAST16_ = new PredType(H5T_NATIVE_UINT_LEAST16);
#endif /* H5_SIZEOF_UINT_LEAST16_T */

#if H5_SIZEOF_INT_LEAST32_T != 0
    NATIVE_INT_LEAST32_ = new PredType(H5T_NATIVE_INT_LEAST32);
#endif /* H5_SIZEOF_INT_LEAST32_T */
#if H5_SIZEOF_UINT_LEAST32_T != 0
    NATIVE_UINT_LEAST32_ = new PredType(H5T_NATIVE_UINT_LEAST32);
#endif /* H5_SIZEOF_UINT_LEAST32_T */

#if H5_SIZEOF_INT_LEAST64_T != 0
    NATIVE_INT_LEAST64_ = new PredType(H5T_NATIVE_INT_LEAST64);
#endif /* H5_SIZEOF_INT_LEAST64_T */
#if H5_SIZEOF_UINT_LEAST64_T != 0
    NATIVE_UINT_LEAST64_ = new PredType(H5T_NATIVE_UINT_LEAST64);
#endif /* H5_SIZEOF_UINT_LEAST64_T */

// FAST types
#if H5_SIZEOF_INT_FAST8_T != 0
    NATIVE_INT_FAST8_ = new PredType(H5T_NATIVE_INT_FAST8);
#endif /* H5_SIZEOF_INT_FAST8_T */
#if H5_SIZEOF_UINT_FAST8_T != 0
    NATIVE_UINT_FAST8_ = new PredType(H5T_NATIVE_UINT_FAST8);
#endif /* H5_SIZEOF_UINT_FAST8_T */

#if H5_SIZEOF_INT_FAST16_T != 0
    NATIVE_INT_FAST16_ = new PredType(H5T_NATIVE_INT_FAST16);
#endif /* H5_SIZEOF_INT_FAST16_T */
#if H5_SIZEOF_UINT_FAST16_T != 0
    NATIVE_UINT_FAST16_ = new PredType(H5T_NATIVE_UINT_FAST16);
#endif /* H5_SIZEOF_UINT_FAST16_T */

#if H5_SIZEOF_INT_FAST32_T != 0
    NATIVE_INT_FAST32_ = new PredType(H5T_NATIVE_INT_FAST32);
#endif /* H5_SIZEOF_INT_FAST32_T */
#if H5_SIZEOF_UINT_FAST32_T != 0
    NATIVE_UINT_FAST32_ = new PredType(H5T_NATIVE_UINT_FAST32);
#endif /* H5_SIZEOF_UINT_FAST32_T */

#if H5_SIZEOF_INT_FAST64_T != 0
    NATIVE_INT_FAST64_ = new PredType(H5T_NATIVE_INT_FAST64);
#endif /* H5_SIZEOF_INT_FAST64_T */
#if H5_SIZEOF_UINT_FAST64_T != 0
    NATIVE_UINT_FAST64_ = new PredType(H5T_NATIVE_UINT_FAST64);
#endif /* H5_SIZEOF_UINT_FAST64_T */

} // makePredTypes


//--------------------------------------------------------------------------
// Function:    PredType::deleteConstants
// Purpose      Deletes all PredType constant pointers.
// Programmer   Binh-Minh Ribler - September 2015
//--------------------------------------------------------------------------
void PredType::deleteConstants()
{
    delete STD_I8BE_;
    delete STD_I8LE_;
    delete STD_I16BE_;
    delete STD_I16LE_;
    delete STD_I32BE_;
    delete STD_I32LE_;
    delete STD_I64BE_;
    delete STD_I64LE_;
    delete STD_U8BE_;
    delete STD_U8LE_;
    delete STD_U16BE_;
    delete STD_U16LE_;
    delete STD_U32BE_;
    delete STD_U32LE_;
    delete STD_U64BE_;
    delete STD_U64LE_;
    delete STD_B8BE_;
    delete STD_B8LE_;
    delete STD_B16BE_;
    delete STD_B16LE_;
    delete STD_B32BE_;
    delete STD_B32LE_;
    delete STD_B64BE_;
    delete STD_B64LE_;
    delete STD_REF_OBJ_;
    delete STD_REF_DSETREG_;

    delete C_S1_;
    delete FORTRAN_S1_;

    delete IEEE_F32BE_;
    delete IEEE_F32LE_;
    delete IEEE_F64BE_;
    delete IEEE_F64LE_;

    delete UNIX_D32BE_;
    delete UNIX_D32LE_;
    delete UNIX_D64BE_;
    delete UNIX_D64LE_;

    delete INTEL_I8_;
    delete INTEL_I16_;
    delete INTEL_I32_;
    delete INTEL_I64_;
    delete INTEL_U8_;
    delete INTEL_U16_;
    delete INTEL_U32_;
    delete INTEL_U64_;
    delete INTEL_B8_;
    delete INTEL_B16_;
    delete INTEL_B32_;
    delete INTEL_B64_;
    delete INTEL_F32_;
    delete INTEL_F64_;

    delete ALPHA_I8_;
    delete ALPHA_I16_;
    delete ALPHA_I32_;
    delete ALPHA_I64_;
    delete ALPHA_U8_;
    delete ALPHA_U16_;
    delete ALPHA_U32_;
    delete ALPHA_U64_;
    delete ALPHA_B8_;
    delete ALPHA_B16_;
    delete ALPHA_B32_;
    delete ALPHA_B64_;
    delete ALPHA_F32_;
    delete ALPHA_F64_;

    delete MIPS_I8_;
    delete MIPS_I16_;
    delete MIPS_I32_;
    delete MIPS_I64_;
    delete MIPS_U8_;
    delete MIPS_U16_;
    delete MIPS_U32_;
    delete MIPS_U64_;
    delete MIPS_B8_;
    delete MIPS_B16_;
    delete MIPS_B32_;
    delete MIPS_B64_;
    delete MIPS_F32_;
    delete MIPS_F64_;

    delete NATIVE_CHAR_;
    delete NATIVE_SCHAR_;
    delete NATIVE_UCHAR_;
    delete NATIVE_SHORT_;
    delete NATIVE_USHORT_;
    delete NATIVE_INT_;
    delete NATIVE_UINT_;
    delete NATIVE_LONG_;
    delete NATIVE_ULONG_;
    delete NATIVE_LLONG_;
    delete NATIVE_ULLONG_;
    delete NATIVE_FLOAT_;
    delete NATIVE_DOUBLE_;
    delete NATIVE_LDOUBLE_;
    delete NATIVE_B8_;
    delete NATIVE_B16_;
    delete NATIVE_B32_;
    delete NATIVE_B64_;
    delete NATIVE_OPAQUE_;
    delete NATIVE_HSIZE_;
    delete NATIVE_HSSIZE_;
    delete NATIVE_HERR_;
    delete NATIVE_HBOOL_;

    delete NATIVE_INT8_;
    delete NATIVE_UINT8_;
    delete NATIVE_INT16_;
    delete NATIVE_UINT16_;
    delete NATIVE_INT32_;
    delete NATIVE_UINT32_;
    delete NATIVE_INT64_;
    delete NATIVE_UINT64_;

// LEAST types
#if H5_SIZEOF_INT_LEAST8_T != 0
    delete NATIVE_INT_LEAST8_;
#endif /* H5_SIZEOF_INT_LEAST8_T */
#if H5_SIZEOF_UINT_LEAST8_T != 0
    delete NATIVE_UINT_LEAST8_;
#endif /* H5_SIZEOF_UINT_LEAST8_T */

#if H5_SIZEOF_INT_LEAST16_T != 0
    delete NATIVE_INT_LEAST16_;
#endif /* H5_SIZEOF_INT_LEAST16_T */
#if H5_SIZEOF_UINT_LEAST16_T != 0
    delete NATIVE_UINT_LEAST16_;
#endif /* H5_SIZEOF_UINT_LEAST16_T */

#if H5_SIZEOF_INT_LEAST32_T != 0
    delete NATIVE_INT_LEAST32_;
#endif /* H5_SIZEOF_INT_LEAST32_T */
#if H5_SIZEOF_UINT_LEAST32_T != 0
    delete NATIVE_UINT_LEAST32_;
#endif /* H5_SIZEOF_UINT_LEAST32_T */

#if H5_SIZEOF_INT_LEAST64_T != 0
    delete NATIVE_INT_LEAST64_;
#endif /* H5_SIZEOF_INT_LEAST64_T */
#if H5_SIZEOF_UINT_LEAST64_T != 0
    delete NATIVE_UINT_LEAST64_;
#endif /* H5_SIZEOF_UINT_LEAST64_T */

// FAST types
#if H5_SIZEOF_INT_FAST8_T != 0
    delete NATIVE_INT_FAST8_;
#endif /* H5_SIZEOF_INT_FAST8_T */
#if H5_SIZEOF_UINT_FAST8_T != 0
    delete NATIVE_UINT_FAST8_;
#endif /* H5_SIZEOF_UINT_FAST8_T */

#if H5_SIZEOF_INT_FAST16_T != 0
    delete NATIVE_INT_FAST16_;
#endif /* H5_SIZEOF_INT_FAST16_T */
#if H5_SIZEOF_UINT_FAST16_T != 0
    delete NATIVE_UINT_FAST16_;
#endif /* H5_SIZEOF_UINT_FAST16_T */

#if H5_SIZEOF_INT_FAST32_T != 0
    delete NATIVE_INT_FAST32_;
#endif /* H5_SIZEOF_INT_FAST32_T */
#if H5_SIZEOF_UINT_FAST32_T != 0
    delete NATIVE_UINT_FAST32_;
#endif /* H5_SIZEOF_UINT_FAST32_T */

#if H5_SIZEOF_INT_FAST64_T != 0
    delete NATIVE_INT_FAST64_;
#endif /* H5_SIZEOF_INT_FAST64_T */
#if H5_SIZEOF_UINT_FAST64_T != 0
    delete NATIVE_UINT_FAST64_;
#endif /* H5_SIZEOF_UINT_FAST64_T */

    delete PREDTYPE_CONST_;
    PREDTYPE_CONST_ = 0;
} // deleteConstants

// Assigning the constant references to the dynamically allocated constants
// after using PREDTYPE_CONST to activate the creation of those constants.

//  PREDTYPE_CONST will be the first static constant declared in the file.
//  getPredTypes() will call makePredTypes() to allocate memory for all the
//  PredType constants.  Note that, there is a similar function to getPredTypes()
//  in other classes, that have global constants, is called getConstant().

const PredType& PredType::PREDTYPE_CONST = *PredType::getPredTypes();
const PredType& PredType::STD_I8BE = *STD_I8BE_;
const PredType& PredType::STD_I8LE = *STD_I8LE_;
const PredType& PredType::STD_I16BE = *STD_I16BE_;
const PredType& PredType::STD_I16LE = *STD_I16LE_;
const PredType& PredType::STD_I32BE = *STD_I32BE_;
const PredType& PredType::STD_I32LE = *STD_I32LE_;
const PredType& PredType::STD_I64BE = *STD_I64BE_;
const PredType& PredType::STD_I64LE = *STD_I64LE_;
const PredType& PredType::STD_U8BE = *STD_U8BE_;
const PredType& PredType::STD_U8LE = *STD_U8LE_;
const PredType& PredType::STD_U16BE = *STD_U16BE_;
const PredType& PredType::STD_U16LE = *STD_U16LE_;
const PredType& PredType::STD_U32BE = *STD_U32BE_;
const PredType& PredType::STD_U32LE = *STD_U32LE_;
const PredType& PredType::STD_U64BE = *STD_U64BE_;
const PredType& PredType::STD_U64LE = *STD_U64LE_;
const PredType& PredType::STD_B8BE = *STD_B8BE_;
const PredType& PredType::STD_B8LE = *STD_B8LE_;
const PredType& PredType::STD_B16BE = *STD_B16BE_;
const PredType& PredType::STD_B16LE = *STD_B16LE_;
const PredType& PredType::STD_B32BE = *STD_B32BE_;
const PredType& PredType::STD_B32LE = *STD_B32LE_;
const PredType& PredType::STD_B64BE = *STD_B64BE_;
const PredType& PredType::STD_B64LE = *STD_B64LE_;
const PredType& PredType::STD_REF_OBJ = *STD_REF_OBJ_;
const PredType& PredType::STD_REF_DSETREG = *STD_REF_DSETREG_;

const PredType& PredType::C_S1 = *C_S1_;
const PredType& PredType::FORTRAN_S1 = *FORTRAN_S1_;

const PredType& PredType::IEEE_F32BE = *IEEE_F32BE_;
const PredType& PredType::IEEE_F32LE = *IEEE_F32LE_;
const PredType& PredType::IEEE_F64BE = *IEEE_F64BE_;
const PredType& PredType::IEEE_F64LE = *IEEE_F64LE_;

const PredType& PredType::UNIX_D32BE = *UNIX_D32BE_;
const PredType& PredType::UNIX_D32LE = *UNIX_D32LE_;
const PredType& PredType::UNIX_D64BE = *UNIX_D64BE_;
const PredType& PredType::UNIX_D64LE = *UNIX_D64LE_;

const PredType& PredType::INTEL_I8 = *INTEL_I8_;
const PredType& PredType::INTEL_I16 = *INTEL_I16_;
const PredType& PredType::INTEL_I32 = *INTEL_I32_;
const PredType& PredType::INTEL_I64 = *INTEL_I64_;
const PredType& PredType::INTEL_U8 = *INTEL_U8_;
const PredType& PredType::INTEL_U16 = *INTEL_U16_;
const PredType& PredType::INTEL_U32 = *INTEL_U32_;
const PredType& PredType::INTEL_U64 = *INTEL_U64_;
const PredType& PredType::INTEL_B8 = *INTEL_B8_;
const PredType& PredType::INTEL_B16 = *INTEL_B16_;
const PredType& PredType::INTEL_B32 = *INTEL_B32_;
const PredType& PredType::INTEL_B64 = *INTEL_B64_;
const PredType& PredType::INTEL_F32 = *INTEL_F32_;
const PredType& PredType::INTEL_F64 = *INTEL_F64_;

const PredType& PredType::ALPHA_I8 = *ALPHA_I8_;
const PredType& PredType::ALPHA_I16 = *ALPHA_I16_;
const PredType& PredType::ALPHA_I32 = *ALPHA_I32_;
const PredType& PredType::ALPHA_I64 = *ALPHA_I64_;
const PredType& PredType::ALPHA_U8 = *ALPHA_U8_;
const PredType& PredType::ALPHA_U16 = *ALPHA_U16_;
const PredType& PredType::ALPHA_U32 = *ALPHA_U32_;
const PredType& PredType::ALPHA_U64 = *ALPHA_U64_;
const PredType& PredType::ALPHA_B8 = *ALPHA_B8_;
const PredType& PredType::ALPHA_B16 = *ALPHA_B16_;
const PredType& PredType::ALPHA_B32 = *ALPHA_B32_;
const PredType& PredType::ALPHA_B64 = *ALPHA_B64_;
const PredType& PredType::ALPHA_F32 = *ALPHA_F32_;
const PredType& PredType::ALPHA_F64 = *ALPHA_F64_;

const PredType& PredType::MIPS_I8 = *MIPS_I8_;
const PredType& PredType::MIPS_I16 = *MIPS_I16_;
const PredType& PredType::MIPS_I32 = *MIPS_I32_;
const PredType& PredType::MIPS_I64 = *MIPS_I64_;
const PredType& PredType::MIPS_U8 = *MIPS_U8_;
const PredType& PredType::MIPS_U16 = *MIPS_U16_;
const PredType& PredType::MIPS_U32 = *MIPS_U32_;
const PredType& PredType::MIPS_U64 = *MIPS_U64_;
const PredType& PredType::MIPS_B8 = *MIPS_B8_;
const PredType& PredType::MIPS_B16 = *MIPS_B16_;
const PredType& PredType::MIPS_B32 = *MIPS_B32_;
const PredType& PredType::MIPS_B64 = *MIPS_B64_;
const PredType& PredType::MIPS_F32 = *MIPS_F32_;
const PredType& PredType::MIPS_F64 = *MIPS_F64_;

const PredType& PredType::NATIVE_CHAR = *NATIVE_CHAR_;
const PredType& PredType::NATIVE_SCHAR = *NATIVE_SCHAR_;
const PredType& PredType::NATIVE_UCHAR = *NATIVE_UCHAR_;
const PredType& PredType::NATIVE_SHORT = *NATIVE_SHORT_;
const PredType& PredType::NATIVE_USHORT = *NATIVE_USHORT_;
const PredType& PredType::NATIVE_INT = *NATIVE_INT_;
const PredType& PredType::NATIVE_UINT = *NATIVE_UINT_;
const PredType& PredType::NATIVE_LONG = *NATIVE_LONG_;
const PredType& PredType::NATIVE_ULONG = *NATIVE_ULONG_;
const PredType& PredType::NATIVE_LLONG = *NATIVE_LLONG_;
const PredType& PredType::NATIVE_ULLONG = *NATIVE_ULLONG_;
const PredType& PredType::NATIVE_FLOAT = *NATIVE_FLOAT_;
const PredType& PredType::NATIVE_DOUBLE = *NATIVE_DOUBLE_;
const PredType& PredType::NATIVE_LDOUBLE = *NATIVE_LDOUBLE_;
const PredType& PredType::NATIVE_B8 = *NATIVE_B8_;
const PredType& PredType::NATIVE_B16 = *NATIVE_B16_;
const PredType& PredType::NATIVE_B32 = *NATIVE_B32_;
const PredType& PredType::NATIVE_B64 = *NATIVE_B64_;
const PredType& PredType::NATIVE_OPAQUE = *NATIVE_OPAQUE_;
const PredType& PredType::NATIVE_HSIZE = *NATIVE_HSIZE_;
const PredType& PredType::NATIVE_HSSIZE = *NATIVE_HSSIZE_;
const PredType& PredType::NATIVE_HERR = *NATIVE_HERR_;
const PredType& PredType::NATIVE_HBOOL = *NATIVE_HBOOL_;

const PredType& PredType::NATIVE_INT8 = *NATIVE_INT8_;
const PredType& PredType::NATIVE_UINT8 = *NATIVE_UINT8_;
const PredType& PredType::NATIVE_INT16 = *NATIVE_INT16_;
const PredType& PredType::NATIVE_UINT16 = *NATIVE_UINT16_;
const PredType& PredType::NATIVE_INT32 = *NATIVE_INT32_;
const PredType& PredType::NATIVE_UINT32 = *NATIVE_UINT32_;
const PredType& PredType::NATIVE_INT64 = *NATIVE_INT64_;
const PredType& PredType::NATIVE_UINT64 = *NATIVE_UINT64_;

// LEAST types
#if H5_SIZEOF_INT_LEAST8_T != 0
const PredType& PredType::NATIVE_INT_LEAST8 = *NATIVE_INT_LEAST8_;
#endif /* H5_SIZEOF_INT_LEAST8_T */
#if H5_SIZEOF_UINT_LEAST8_T != 0
const PredType& PredType::NATIVE_UINT_LEAST8 = *NATIVE_UINT_LEAST8_;
#endif /* H5_SIZEOF_UINT_LEAST8_T */

#if H5_SIZEOF_INT_LEAST16_T != 0
const PredType& PredType::NATIVE_INT_LEAST16 = *NATIVE_INT_LEAST16_;
#endif /* H5_SIZEOF_INT_LEAST16_T */
#if H5_SIZEOF_UINT_LEAST16_T != 0
const PredType& PredType::NATIVE_UINT_LEAST16 = *NATIVE_UINT_LEAST16_;
#endif /* H5_SIZEOF_UINT_LEAST16_T */

#if H5_SIZEOF_INT_LEAST32_T != 0
const PredType& PredType::NATIVE_INT_LEAST32 = *NATIVE_INT_LEAST32_;
#endif /* H5_SIZEOF_INT_LEAST32_T */
#if H5_SIZEOF_UINT_LEAST32_T != 0
const PredType& PredType::NATIVE_UINT_LEAST32 = *NATIVE_UINT_LEAST32_;
#endif /* H5_SIZEOF_UINT_LEAST32_T */

#if H5_SIZEOF_INT_LEAST64_T != 0
const PredType& PredType::NATIVE_INT_LEAST64 = *NATIVE_INT_LEAST64_;
#endif /* H5_SIZEOF_INT_LEAST64_T */
#if H5_SIZEOF_UINT_LEAST64_T != 0
const PredType& PredType::NATIVE_UINT_LEAST64 = *NATIVE_UINT_LEAST64_;
#endif /* H5_SIZEOF_UINT_LEAST64_T */

// FAST types
#if H5_SIZEOF_INT_FAST8_T != 0
const PredType& PredType::NATIVE_INT_FAST8 = *NATIVE_INT_FAST8_;
#endif /* H5_SIZEOF_INT_FAST8_T */
#if H5_SIZEOF_UINT_FAST8_T != 0
const PredType& PredType::NATIVE_UINT_FAST8 = *NATIVE_UINT_FAST8_;
#endif /* H5_SIZEOF_UINT_FAST8_T */

#if H5_SIZEOF_INT_FAST16_T != 0
const PredType& PredType::NATIVE_INT_FAST16 = *NATIVE_INT_FAST16_;
#endif /* H5_SIZEOF_INT_FAST16_T */
#if H5_SIZEOF_UINT_FAST16_T != 0
const PredType& PredType::NATIVE_UINT_FAST16 = *NATIVE_UINT_FAST16_;
#endif /* H5_SIZEOF_UINT_FAST16_T */

#if H5_SIZEOF_INT_FAST32_T != 0
const PredType& PredType::NATIVE_INT_FAST32 = *NATIVE_INT_FAST32_;
#endif /* H5_SIZEOF_INT_FAST32_T */
#if H5_SIZEOF_UINT_FAST32_T != 0
const PredType& PredType::NATIVE_UINT_FAST32 = *NATIVE_UINT_FAST32_;
#endif /* H5_SIZEOF_UINT_FAST32_T */

#if H5_SIZEOF_INT_FAST64_T != 0
const PredType& PredType::NATIVE_INT_FAST64 = *NATIVE_INT_FAST64_;
#endif /* H5_SIZEOF_INT_FAST64_T */
#if H5_SIZEOF_UINT_FAST64_T != 0
const PredType& PredType::NATIVE_UINT_FAST64 = *NATIVE_UINT_FAST64_;
#endif /* H5_SIZEOF_UINT_FAST64_T */

#endif // DOXYGEN_SHOULD_SKIP_THIS

} // end namespace

/***************************************************************************
                                Design Note
                                ===========

September 2015:

        The C++ library has several types of global constants from different
        classes, such as PropList, PredType, DataSpace, etc...  Previously,
        these global constants were declared statically and the C++ library used
        a constant, called PredType::AtExit, to detect when all the global
        contants are destroyed then close the C library (H5close).  This method
        relied on the order of the constants being created and destroyed and
        that PredType constants be the last to be destroyed.  In September
        2015, it was recognized that the order in which the global constants were
        created and destroyed was actually undefined, thus can be different
        between different compilers.  This resulted in failure when compilers
        destroy PredType constants before others because when PredType::AtExit
        was destroyed, the C library was closed, so when the constants of other
        classes such as PropList or DataSpace were being deleted, the C library
        would not be available.

        These are the classes that have global constants:
                + PredType
                + DataSpace
                + PropList (and its subclasses below)
                + DSetMemXferPropList
                + DSetCreatPropList
                + DSetAccPropList
                + FileAccPropList
                + FileCreatPropList
                + LinkAccPropList
                + LinkCreatPropList
                + ObjCreatPropList

        The new method includes these main points:

        - The C++ library uses dynamically allocated constants to have the
          control in which order the global constants are created/destroyed.

        - The previous static constants are changed to be the references to
          the dynamically allocated constants to avoid impact on applications.

        - The first time an IdComponent default constructor is invoked, it
          will call the function H5Library::initH5cpp which registers the
          terminating functions from each class that has the global constants
          so that these functions can destroy those constants at the exit of the
          application.  IdComponent is a baseclass of any object class that has
          an identifier, such as Group, DataSet, DataType,...  The classes which
          have the global constants are all derived from IdComponent.

        - At the normal termination of the application, each registered function
          for each constant type will delete all the allocated constants in
          that type class, then a different terminating function, which was also
          registered with atexit() by initH5cpp, will call H5close to close the
          C library.

        The following list presents the differences between the old and new
        methods and the changes implemented for the new method.

        1.  The following items are added to class H5Library:
                // Private instance to be created by H5Library only
                static H5Library* instance;

                // Returns a singleton H5Library to initialize the global
                // constants, invoked in IdComponent default constructor
                static H5Library* getInstance(); // public

                // Registers cleanup and terminating functions with atexit(),
                // called in IdComponent default constructor
                static void initH5cpp(void); // public

                // Calls H5close to terminate the library, registered with
                // atexit(), as the last thing to be done.
                static void termH5cpp(void); // public

        2.  The following shows the differences between the old and new methods
            for allocating the PredType constants.  There are more than 100
            constants, but only one is shown here for examples.

        Old Method:
        ----------
                // Declaration of the constant - in "H5PredType.h"
                static const PredType NATIVE_INT;

                // Definition of the constant - in "H5PredType.cpp"
                const PredType PredType::NATIVE_INT(H5T_NATIVE_INT);

        New Method:
        ----------
                // Declare pointer for a constant - in "H5PredType.h"
                static PredType* NATIVE_INT_; // "H5PredType.h"

                // Change previous constant to reference - in "H5PredType.h"
                static const PredType& NATIVE_INT;

                // The assignment of the first static constant, named
                // PREDTYPE_CONST, calls makePredTypes() which allocates the
                // dynamic memory for every PredType constant.

                // Creates a dynamic PredType object representing a C constant
                // - in makePredTypes()
                NATIVE_INT_ = new PredType(H5T_NATIVE_INT);

                // Assign the constant reference to the dynamic object
                // - in "H5PredType.cpp"
                const PredType& PredType::NATIVE_INT = *NATIVE_INT_;

            Functions added to class PredType:

                // Creates the constants
                static void makePredTypes(); // private

                // Calls makePredTypes to create the constants and returns
                // the dummy constant PREDTYPE_CONST;
                static PredType* getPredTypes(); // private

                // Deletes the constants
                static void deleteConstants(); // public

        3.  This section shows the differences between the old and new methods
            for allocating the DataSpace constant, DataSpace::ALL.

        Old Method:
        ----------
                // Declaration of the constant - in "H5DataSpace.h"
                static const DataSpace ALL;

                // Definition of the constant - in "H5DataSpace.cpp"
                const DataSpace DataSpace::ALL(H5S_ALL);

        New Method:
        ----------
                // Declare pointer for a constant - in "H5DataSpace.h"
                static DataSpace* ALL_; // "H5DataSpace.h"

                // Change previous constant to reference - in "H5DataSpace.h"
                static const DataSpace& ALL;

                // Creates a dynamic DataSpace object representing the C constant
                // - in "H5DataSpace.cpp"
                ALL_ = new DataSpace(H5S_ALL);

                // Assign the constant reference to the dynamic object
                // - in "H5DataSpace.cpp"
                const DataSpace& DataSpace::ALL = *ALL_;

            Functions added to class DataSpace:

                // Creates the constant
                static DataSpace* getConstant(); // private

                // Deletes the constant
                static void deleteConstants(); // public

        4.  This section shows the differences between the old and new methods
            for allocating the following constants
                - PropList constant, PropList::DEFAULT.
                - DSetAccPropList constant, DSetAccPropList::DEFAULT.
                - DSetCreatPropList constant, DSetCreatPropList::DEFAULT.
                - DSetMemXferPropList constant, DSetMemXferPropList::DEFAULT.
                - FileCreatPropList constant, FileCreatPropList::DEFAULT.
                - FileAccPropList constant, FileAccPropList::DEFAULT.
                - LinkAccPropList constant, LinkAccPropList::DEFAULT.
                - LinkCreatPropList constant, LinkCreatPropList::DEFAULT.
                - ObjCreatPropList constant, ObjCreatPropList::DEFAULT.

            For these constants, the library has the same changes, except the
            class names and the HDF5 corresponding constants. Only the items
            of PropList are listed, and "PropList" can be replaced by any of
            DSetAccPropList, DSetCreatPropList, DSetMemXferPropList,
            FileCreatPropList, FileAccPropList, LinkAccPropList, LinkCreatPropList,
            ObjCreatPropList for those classes.  The HDF5 C constant "H5P_DEFAULT"
            can be replaced by any of these respectively: H5P_DATASET_ACCESS,
            H5P_DATASET_CREATE, H5P_DATASET_XFER, H5P_FILE_CREATE, H5P_FILE_ACCESS,
            H5P_LINK_ACCESS, H5P_LINK_CREATE, and H5P_OBJECT_CREATE.

        Old Method:
        ----------
                // Declaration of the constant - in "H5PropList.h"
                static const PropList DEFAULT;

                // Definition of the constant - in "H5PropList.cpp"
                const PropList PropList::DEFAULT(H5P_DEFAULT);

        New Method:
        ----------
                // Declare pointer for a constant - in "H5PropList.h"
                static PropList* DEFAULT_; // "H5PropList.h"

                // Change previous constant to reference - in "H5PropList.h"
                static const PropList& DEFAULT;

                // Creates a dynamic PropList object representing the C constant
                // - in "H5PropList.cpp"
                DEFAULT_ = new PropList(H5P_DEFAULT);

                // Assign the constant reference to the dynamic object
                // - in "H5PropList.cpp"
                const PropList& PropList::DEFAULT = *DEFAULT_;

            Functions added to class PropList:

                // Creates the constant
                static PropList* getConstant(); // private

                // Deletes the constants
                static void deleteConstants(); // public

            The same functions are added to the subclasses of PropList instead of
            using PropList's because of the class types and in favor of clarity.

****************************************************************************/


/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapConversionTable.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "wrapConversionTable.h"
#include "wrapTypeInfo.h"
#include "wrapConverters.h"
#include "wrapException.h"

namespace _wrap_
{


/**
 * Constructor registers predefined conversions with the table.
 */
ConversionTable::ConversionTable()
{
  this->InitializePredefinedConversions();
}


/**
 * Determine whether a conversion function exists for the given conversion.
 */
bool ConversionTable::Exists(const CvQualifiedType& from,
                             const Type* to) const
{
  return (this->GetConversion(from, to) != NULL);
}


/**
 * Set the conversion function for the given conversion.
 */
void ConversionTable::SetConversion(const CvQualifiedType& from,
                                    const Type* to, ConversionFunction f)
{
  ConversionKey conversionKey(from, to);
  m_ConversionMap[conversionKey] = f;
}


/**
 * Retrieve the function for the given conversion.  If an exact match for
 * the "from" type is not found, an attempt is made to find one that is
 * more cv-qualified.  If none exists, NULL is returned.
 */
ConversionFunction
ConversionTable::GetConversion(const CvQualifiedType& from,
                               const Type* to) const
{
  // Try to find exact match for "from" type.
  ConversionKey conversionKey(from, to);
  ConversionMap::const_iterator i = m_ConversionMap.find(conversionKey);
  if(i != m_ConversionMap.end())
    {
    return i->second;
    }
  
  // If the "from" type is a reference type, we can try adding
  // qualifiers to the type it references.
  if(from.GetType()->IsReferenceType())
    {
    CvQualifiedType referencedType =
      ReferenceType::SafeDownCast(from.GetType())->GetReferencedType();
    conversionKey = ConversionKey(TypeInfo::GetReferenceType(referencedType.GetMoreQualifiedType(true, false)), to);
    i = m_ConversionMap.find(conversionKey);
    if(i != m_ConversionMap.end())
      {
      return i->second;
      }
    conversionKey = ConversionKey(TypeInfo::GetReferenceType(referencedType.GetMoreQualifiedType(false, true)), to);
    i = m_ConversionMap.find(conversionKey);
    if(i != m_ConversionMap.end())
      {
      return i->second;
      }
    conversionKey = ConversionKey(TypeInfo::GetReferenceType(referencedType.GetMoreQualifiedType(true, true)), to);
    i = m_ConversionMap.find(conversionKey);
    if(i != m_ConversionMap.end())
      {
      return i->second;
      }
    }
  // If the "from" type is not a reference type, we can try adding
  // qualifiers to it.
  else
    {
    // Try adding a const qualifier to the "from" type.
    conversionKey = ConversionKey(from.GetMoreQualifiedType(true, false), to);
    i = m_ConversionMap.find(conversionKey);
    if(i != m_ConversionMap.end())
      {
      return i->second;
      }
    // Try adding a volatile qualifier to the "from" type.
    conversionKey = ConversionKey(from.GetMoreQualifiedType(false, true), to);
    i = m_ConversionMap.find(conversionKey);
    if(i != m_ConversionMap.end())
      {
      return i->second;
      }
    // Try adding both const and volatile qualifiers to the "from" type.
    conversionKey = ConversionKey(from.GetMoreQualifiedType(true, true), to);
    i = m_ConversionMap.find(conversionKey);
    if(i != m_ConversionMap.end())
      {
      return i->second;
      }
    }
  
  // Couldn't find a conversion.
  return NULL;
}


// Macro to shorten InitializePredefinedConversions function body.
#define _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(T1, T2) \
this->SetConversion(CvPredefinedType<const T1>::type, \
                    CvPredefinedType<T2>::type.GetType(), \
                    Converter::ConversionByConstructor<T1, T2>::GetConversionFunction()); \
this->SetConversion(CvPredefinedType<const T2>::type, \
                    CvPredefinedType<T1>::type.GetType(), \
                    Converter::ConversionByConstructor<T2, T1>::GetConversionFunction())

/**
 * Registers basic type conversion functions for this ConversionTable.
 * Only called from the constructor.
 *
 * The "from" type for any conversion added here should be
 * const-friendly, if possible.  This way, conversion from a non-const
 * type can still chain up to the conversion from the const type, thus
 * avoiding duplication of the conversion function.
 */
void ConversionTable::InitializePredefinedConversions()
{
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(int, unsigned short);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(int, unsigned int);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(int, unsigned long);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, unsigned short);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, unsigned int);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, unsigned long);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, short);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, int);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(int, float);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, float);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(int, double);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(long, double);
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(float, double);
}


/**
 * Get an ConversionTable object set up to deal with the given Tcl interpreter.
 * If one exists, it will be returned.  Otherwise, a new one will be
 * created.
 */
ConversionTable* ConversionTable::GetForInterpreter(Tcl_Interp* interp)
{
  // See if an ConversionTable exists for the given interpreter.
  if(interpreterConversionTableMap.count(interp) == 0)
    {
    // No, we must create a new ConversionTable for this interpreter.
    interpreterConversionTableMap[interp] = new ConversionTable();
    }
  
  // Return the ConversionTable.
  return interpreterConversionTableMap[interp];  
}


/**
 * Map from a Tcl interpreter to the ConversionTable for it.
 */
ConversionTable::InterpreterConversionTableMap ConversionTable::interpreterConversionTableMap;

} // namespace _wrap_

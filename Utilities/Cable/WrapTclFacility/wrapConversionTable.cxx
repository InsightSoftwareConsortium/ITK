/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapConversionTable.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "wrapConversionTable.h"

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
  // Couldn't find a conversion.
  return NULL;
}


// Macro to shorten InitializePredefinedConversions function body.
#define _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(T1, T2) \
this->SetConversion(CvType<const T1>::type, \
                    CvType<T2>::type.GetType(), \
                    Converter::ConversionByConstructor<T1, T2>::GetConversionFunction()); \
this->SetConversion(CvType<const T2>::type, \
                    CvType<T1>::type.GetType(), \
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
  _wrap_REGISTER_FUNDAMENTAL_TYPE_CONVERSIONS(int, float);
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

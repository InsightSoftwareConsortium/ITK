/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapConversionTable.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapConversionTable_h
#define _wrapConversionTable_h

#include "wrapUtils.h"
#include <map>

namespace _wrap_
{

/**
 * The general type of a conversion function.  A real conversion function
 * will return something, but they are all cast to this for storage
 * in the table.
 */
typedef void (*ConversionFunction)(void*);


/**
 * A class to maintain a table of conversion functions for a Tcl interpreter.
 *
 * The table is keyed on the "from" and "to" types.  The top level
 * cv-qualifiers on the "from" type can affect the conversion, but
 * those on the "to" type cannot.
 *
 * 8.3.5/3 Top level cv-qualifiers on target type never matter for
 * conversions.  They only affect the parameter inside the function body.
 */
class _wrap_EXPORT ConversionTable
{
public:  
  ConversionTable();
  
  bool Exists(const CvQualifiedType& from, const Type* to) const;
  void SetConversion(const CvQualifiedType& from,
                     const Type* to, ConversionFunction);
  ConversionFunction GetConversion(const CvQualifiedType& from,
                                   const Type* to) const;
  
private:
  typedef std::pair<CvQualifiedType, const Type*> ConversionKey;
  typedef std::map<ConversionKey, ConversionFunction> ConversionMap;

  /**
   * Map from type in/out pair to conversion function.
   */
  ConversionMap m_ConversionMap;
public:
  static ConversionTable* GetForInterpreter(Tcl_Interp*);
  
private:
  typedef std::map<const Tcl_Interp*, ConversionTable*,
                   PointerCompare<const Tcl_Interp> >  InterpreterConversionTableMap;
  static InterpreterConversionTableMap interpreterConversionTableMap;  
};

} // namespace _wrap_

#endif

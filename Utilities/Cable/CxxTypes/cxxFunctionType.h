/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxFunctionType.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cxxFunctionType_h
#define _cxxFunctionType_h

#include "cxxCvQualifiedType.h"

#include <vector>

namespace _cxx_
{


/**
 * Represent a C++ function type.  This consists of the return type and
 * argument types.
 */
class _cxx_EXPORT FunctionType: public Type
{
public:
  typedef FunctionType Self;
  
  virtual RepresentationType GetRepresentationType() const;

  virtual String GenerateName(const String& indirection,
                              bool isConst, bool isVolatile) const;
protected:
  FunctionType(const CvQualifiedType& returnType,
               const CvQualifiedTypes& arguments);
  FunctionType(const Self&): m_ReturnType(NULL) {}
  void operator=(const Self&) {}
  virtual ~FunctionType() {}
  
private:
  /**
   * The function's return type.
   */
  CvQualifiedType m_ReturnType;
  
  /**
   * The function's argument types.
   */
  CvQualifiedTypes m_Arguments;
  
  friend TypeSystem;
};

} // namespace _cxx_


#endif

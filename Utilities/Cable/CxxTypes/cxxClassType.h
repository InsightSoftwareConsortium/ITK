/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxClassType.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cxxClassType_h
#define _cxxClassType_h

#include "cxxCvQualifiedType.h"

namespace _cxx_
{

class ClassType;

/**
 * A list of ClassType s.
 */
typedef std::list<const ClassType*> ClassTypeList;

/**
 * Represents a C++ class type.  This could have been produced by a
 * class, struct, union, template full specialization, or template
 * instantiation.
 */
class ClassType: public Type
{
public:
  typedef ClassType Self;
  
  virtual RepresentationType GetRepresentationType() const;

  void AddParent(const ClassType*);
  
protected:
  ClassType(const String&);
  ClassType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~ClassType() {}
  
private:  
  /**
   * The name of the class.
   */
  String m_Name;
  
  /**
   * The immediate public superclasses of this class.
   * A pointer or reference ot this class can be cast up to these
   * types.
   */
  ClassTypeList m_Parents;
  
  /**
   * The list of types from which this class can construct.
   */
  CvQualifiedTypeList m_ConversionByConstructor;
  
  /**
   * The list of types to which this class can convert by type conversion
   * operator.
   */
  CvQualifiedTypeList m_ConversionOperators;
  
  friend TypeSystem;
};


} // namespace _cxx_


#endif

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCreateObjectFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkCreateObjectFunction_h
#define __itkCreateObjectFunction_h

#include "itkObject.h"

namespace itk
{

/** \class CreateObjectFunctionBase
 * Define API for object creation callback functions.
 */
class CreateObjectFunctionBase: public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef CreateObjectFunctionBase  Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /** 
   * Smart pointer typedef support. 
   */
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Create an object and return a pointer to it as an
   * itkLightObject.
   */
  virtual SmartPointer<LightObject> CreateObject() = 0;
};


/** \class CreateObjectFunction
 * CreateObjectFunction is used to create callback functions that
 * create itkObjects for use with the itkObjectFactory.
 */
template <class T>
class CreateObjectFunction : public CreateObjectFunctionBase
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef CreateObjectFunction  Self;

  /** 
   * Smart pointer typedef support. 
   */
  typedef SmartPointer<Self>    Pointer;
  
  // Methods from Object
  static Pointer New() { return new Self;}
  LightObject::Pointer CreateObject()
    {
    return LightObject::Pointer(T::New());
    }
};

} // end namespace itk

#endif



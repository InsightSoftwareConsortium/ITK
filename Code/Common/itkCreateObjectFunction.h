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
/**
 * CreateObjectFunction is used to create callback functions that
 * create itkObjects for use with the itkObjectFactory.
 */
#ifndef __itkCreaetObjectFunction_h
#define __itkCreaetObjectFunction_h

#include "itkObject.h"

namespace itk
{

class CreateObjectFunctionBase: public Object
{
public:
  typedef CreateObjectFunctionBase  Self;
  typedef SmartPointer<Self>        Pointer;
  virtual LightObject* CreateObject() = 0;
};

template <class T>
class CreateObjectFunction : public CreateObjectFunctionBase
{
public:
  typedef CreateObjectFunction  Self;
  typedef SmartPointer<Self>    Pointer;
  
  // Methods from itkObject
  static Pointer New() { return new Self;}
  LightObject* CreateObject()
    {
      return T::New();
    }
};

} // namespace itk

#endif



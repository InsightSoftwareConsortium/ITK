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
 * itkCreateObjectFunction is used to create callback functions that
 * create itkObjects for use with the itkObjectFactory.
 */
#ifndef __itkCreaetObjectFunction_h
#define __itkCreaetObjectFunction_h

#include "itkObject.h"


class itkCreateObjectFunctionBase : public itkObject
{
public:
  typedef itkSmartPointer<itkCreateObjectFunctionBase> Pointer;
  virtual itkLightObject* CreateObject() = 0;
};


template <class T>
class itkCreateObjectFunction : public itkCreateObjectFunctionBase
{
public:
  // Methods from itkObject
  static Pointer New() { return new itkCreateObjectFunction<T>;}
  itkLightObject* CreateObject()
    {
      return T::New();
    }
};

#endif



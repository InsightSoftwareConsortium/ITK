/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkObjectFactory.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkObjectFactory is a helper class used to created instances of a
 * class. Object factories are used for instantiation because they allow
 * run-time replacement of a class with a user-supplied version. For
 * example, if you wished to replace an algorithm with your own custom
 * version, or with a hardware-accelerated version, itkObjectFactory
 * can be used to do this.
 *
 * This implementation of the object factory is templated and uses RTTI
 * (Run-Time Type Information) to create the name of the class it is to
 * instantiate. (The name may include template type parameters, depending
 * on the class definition.)
 */

#ifndef __itkObjectFactory_h
#define __itkObjectFactory_h

#include "itkObjectFactoryBase.h"

template <class T>
class itkObjectFactory : public itkObjectFactoryBase
{
public:
  static T* Create()
    {
    itkLightObject* ret = itkObjectFactory::CreateInstance(typeid(T).name());
    if(ret)
      {
      try
        {
        return dynamic_cast<T*>(ret);
        }
      catch (...)
        {
        return 0;
        }
      }
    return 0;
    }
};


#endif

        

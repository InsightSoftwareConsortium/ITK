
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
// 


#ifndef __itkObjectFactory_h
#define __itkObjectFactory_h
#include "itkObjectFactoryBase.h"

template <class T>
class itkObjectFactory : public itkObjectFactoryBase
{
public:
  static T* Create()
    {
      itkObject* ret = 
	itkObjectFactory::CreateInstance(
	  typeid(T).name());
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

	

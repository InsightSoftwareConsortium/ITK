/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFactoryTestLib.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkFactoryTestLib_h
#define __itkFactoryTestLib_h

#include "itkObjectFactoryBase.h"

#ifdef WIN32
#define FactoryTestPlugin_EXPORT __declspec(dllexport)
#else
#define FactoryTestPlugin_EXPORT 
#endif

/**
 * Routine that is called when the shared library is loaded by
 * itk::ObjectFactoryBase::LoadDynamicFactories().
 *
 * itkLoad() is C (not C++) function.
 */
extern "C" {
    FactoryTestPlugin_EXPORT itk::ObjectFactoryBase* itkLoad();
} 

#endif  

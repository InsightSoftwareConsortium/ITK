/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileFreeImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkFileFreeImageIOFactory.h"
#include "itkFileFreeImageIO.h"
#include "itkCreateObjectFunction.h"
#include "itkVersion.h"

/**
 * Routine that is called when the shared library is loaded by
 * itk::ObjectFactoryBase::LoadDynamicFactories().
 *
 * itkLoad() is C (not C++) function.
 */
/**
 * Routine that is called when the shared library is loaded by
 * itk::ObjectFactoryBase::LoadDynamicFactories().
 *
 * itkLoad() is C (not C++) function.
 */
#ifdef WIN32
#define FileFreeIOPlugin_EXPORT __declspec(dllexport)
#else
#define FileFreeIOPlugin_EXPORT 
#endif

extern "C" {
  FileFreeIOPlugin_EXPORT itk::ObjectFactoryBase* itkLoad();
} 


itk::ObjectFactoryBase* itkLoad()
{
  static itk::FileFreeImageIOFactory::Pointer f
    = itk::FileFreeImageIOFactory::New();
  return f;
}
  
namespace itk
{

FileFreeImageIOFactory::FileFreeImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkFileFreeImageIO",
                         "ImageIO that creates an in-memory file from a text description",
                         1,
                         CreateObjectFunction<FileFreeImageIO>::New());
}
  
FileFreeImageIOFactory::~FileFreeImageIOFactory()
{
}

const char* 
FileFreeImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char* 
FileFreeImageIOFactory::GetDescription() const
{
  return "ImageIO that creates an in-memory file from a text description";
}

} // end namespace itk


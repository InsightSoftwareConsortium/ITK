/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMetaImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkMetaImageIO.h"
#include "itkVersion.h"

  
namespace itk
{
MetaImageIOFactory::MetaImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkMetaImageIO",
                         "Meta Image IO",
                         1,
                         CreateObjectFunction<MetaImageIO>::New());
}
  
MetaImageIOFactory::~MetaImageIOFactory()
{
}

const char* 
MetaImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char* 
MetaImageIOFactory::GetDescription() const
{
  return "Meta ImageIO Factory, allows the loading of Meta images into insight";
}

} // end namespace itk


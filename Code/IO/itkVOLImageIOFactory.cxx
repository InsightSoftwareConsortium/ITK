/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVOLImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkVOLImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkVOLImageIO.h"
#include "itkVersion.h"
  
namespace itk
{

VOLImageIOFactory::VOLImageIOFactory()
{
  
  this->RegisterOverride("itkImageIOBase",
                         "itkVOLImageIO",
                         "VOL Image IO",
                         1,
                         CreateObjectFunction<VOLImageIO>::New());
                         
}
VOLImageIOFactory::~VOLImageIOFactory()
{
}

const char* 
VOLImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char* 
VOLImageIOFactory::GetDescription(void) const
{
  return "VOL ImageIO Factory, allows the loading of VOL images into insight";
}


} // end namespace itk



 

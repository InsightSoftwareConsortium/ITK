/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkB2MaskImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkB2MaskImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkB2MaskImageIO.h"
#include "itkVersion.h"

  
namespace itk
{
void B2MaskImageIOFactory::PrintSelf(std::ostream& os, Indent indent) const
{

}


B2MaskImageIOFactory::B2MaskImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkB2MaskImageIO",
                         "Brains2 Mask Image IO",
                         1,
                         CreateObjectFunction<B2MaskImageIO>::New());
}
  
B2MaskImageIOFactory::~B2MaskImageIOFactory()
{
}

const char* 
B2MaskImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char* 
B2MaskImageIOFactory::GetDescription() const
{
  return "Brains2 Mask ImageIO Factory, allows the loading of Brains2 binary mask as images into insight";
}

} // end namespace itk


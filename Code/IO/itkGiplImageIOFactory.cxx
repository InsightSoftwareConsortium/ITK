/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGiplImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkGiplImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkGiplImageIO.h"
#include "itkVersion.h"

  
namespace itk
{
void GiplImageIOFactory::PrintSelf(std::ostream& os, Indent indent) const
{
  
}
  

GiplImageIOFactory::GiplImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkGiplImageIO",
                         "Gipl Image IO",
                         1,
                         CreateObjectFunction<GiplImageIO>::New());
}
  
GiplImageIOFactory::~GiplImageIOFactory()
{
}

const char* 
GiplImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char* 
GiplImageIOFactory::GetDescription() const
{
  return "Gipl ImageIO Factory, allows the loading of Gipl images into Insight";
}

} // end namespace itk


/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkGE5ImageIOFactory.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkGE5ImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkGE5ImageIO.h"
#include "itkVersion.h"


namespace itk
{
void GE5ImageIOFactory::PrintSelf(std::ostream&, Indent) const
{

}


GE5ImageIOFactory::GE5ImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkGE5ImageIO",
                         "GE5 Image IO",
                         1,
                         CreateObjectFunction<GE5ImageIO>::New());
}

GE5ImageIOFactory::~GE5ImageIOFactory()
{
}

const char*
GE5ImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char*
GE5ImageIOFactory::GetDescription() const
{
  return "GE5 ImageIO Factory, allows the loading of GE5 images into insight";
}

} // end namespace itk


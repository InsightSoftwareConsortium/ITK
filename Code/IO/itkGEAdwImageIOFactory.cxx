/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkGEAdwImageIOFactory.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkGEAdwImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkGEAdwImageIO.h"
#include "itkVersion.h"


namespace itk
{
void GEAdwImageIOFactory::PrintSelf(std::ostream&, Indent) const
{

}


GEAdwImageIOFactory::GEAdwImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkGEAdwImageIO",
                         "GEAdw Image IO",
                         1,
                         CreateObjectFunction<GEAdwImageIO>::New());
}

GEAdwImageIOFactory::~GEAdwImageIOFactory()
{
}

const char*
GEAdwImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char*
GEAdwImageIOFactory::GetDescription() const
{
  return "GEAdw ImageIO Factory, allows the loading of GEAdw images into insight";
}

} // end namespace itk


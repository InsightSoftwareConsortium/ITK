/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkJPEGImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkJPEGImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkJPEGImageIO.h"
#include "itkVersion.h"

namespace itk
{
JPEGImageIOFactory::JPEGImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkJPEGImageIO",
                          "JPEG Image IO",
                          1,
                          CreateObjectFunction< JPEGImageIO >::New() );
}

JPEGImageIOFactory::~JPEGImageIOFactory()
{}

const char *
JPEGImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
JPEGImageIOFactory::GetDescription(void) const
{
  return "JPEG ImageIO Factory, allows the loading of JPEG images into insight";
}
} // end namespace itk

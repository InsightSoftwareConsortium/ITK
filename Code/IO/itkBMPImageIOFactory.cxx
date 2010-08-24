/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBMPImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkBMPImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkBMPImageIO.h"
#include "itkVersion.h"

namespace itk
{
BMPImageIOFactory::BMPImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkBMPImageIO",
                          "BMP Image IO",
                          1,
                          CreateObjectFunction< BMPImageIO >::New() );
}

BMPImageIOFactory::~BMPImageIOFactory()
{}

const char *
BMPImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
BMPImageIOFactory::GetDescription() const
{
  return "BMP ImageIO Factory, allows the loading of BMP images into Insight";
}
} // end namespace itk

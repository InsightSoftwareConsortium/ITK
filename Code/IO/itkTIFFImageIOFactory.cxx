/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTIFFImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTIFFImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkTIFFImageIO.h"
#include "itkVersion.h"

namespace itk
{
TIFFImageIOFactory::TIFFImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkTIFFImageIO",
                          "TIFF Image IO",
                          1,
                          CreateObjectFunction< TIFFImageIO >::New() );
}

TIFFImageIOFactory::~TIFFImageIOFactory()
{}

const char *
TIFFImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
TIFFImageIOFactory::GetDescription(void) const
{
  return "TIFF ImageIO Factory, allows the loading of TIFF images into insight";
}
} // end namespace itk

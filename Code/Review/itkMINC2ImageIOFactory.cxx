/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMINC2ImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMINC2ImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkMINC2ImageIO.h"
#include "itkVersion.h"

namespace itk
{
MINC2ImageIOFactory::MINC2ImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkMINC2ImageIO",
                          "MINC2 Image IO",
                          1,
                          CreateObjectFunction< MINC2ImageIO >::New() );
}

MINC2ImageIOFactory::~MINC2ImageIOFactory()
{}

const char *
MINC2ImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
MINC2ImageIOFactory::GetDescription(void) const
{
  return "MINC2 ImageIO Factory, allows the loading of MINC2 images into insight";
}
} // end namespace itk

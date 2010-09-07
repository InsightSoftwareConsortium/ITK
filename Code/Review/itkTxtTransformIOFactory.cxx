/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkTxtTransformIOFactory.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkTxtTransformIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkTxtTransformIO.h"
#include "itkVersion.h"

namespace itk
{
void TxtTransformIOFactory::PrintSelf(std::ostream &, Indent) const
{}

TxtTransformIOFactory::TxtTransformIOFactory()
{
  this->RegisterOverride( "itkTransformIOBase",
                          "itkTxtTransformIO",
                          "Txt Transform IO",
                          1,
                          CreateObjectFunction< TxtTransformIO >::New() );
}

TxtTransformIOFactory::~TxtTransformIOFactory()
{}

const char *
TxtTransformIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
TxtTransformIOFactory::GetDescription() const
{
  return "Txt TransformIO Factory, allows the"
         " loading of Nifti images into insight";
}
} // end namespace itk

/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkMatlabTransformIOFactory.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMatlabTransformIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkMatlabTransformIO.h"
#include "itkVersion.h"

namespace itk
{
void MatlabTransformIOFactory::PrintSelf(std::ostream &, Indent) const
{}

MatlabTransformIOFactory::MatlabTransformIOFactory()
{
  this->RegisterOverride( "itkTransformIOBase",
                          "itkMatlabTransformIO",
                          "Matlab Transform IO",
                          1,
                          CreateObjectFunction< MatlabTransformIO >::New() );
}

MatlabTransformIOFactory::~MatlabTransformIOFactory()
{}

const char *
MatlabTransformIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
MatlabTransformIOFactory::GetDescription() const
{
  return "Matlab TransformIO Factory, allows the "
         "loading of Nifti images into insight";
}
} // end namespace itk

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVTKImageIO2Factory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkVTKImageIO2Factory.h"
#include "itkCreateObjectFunction.h"
#include "itkVTKImageIO2.h"
#include "itkVersion.h"

namespace itk
{
VTKImageIO2Factory::VTKImageIO2Factory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkVTKImageIO2",
                          "VTK Image IO",
                          1,
                          CreateObjectFunction< VTKImageIO2 >::New() );
}

VTKImageIO2Factory::~VTKImageIO2Factory()
{}

const char *
VTKImageIO2Factory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
VTKImageIO2Factory::GetDescription(void) const
{
  return "VTK ImageIO2 Factory, allows the loading and streaming of VTK images into ITK";
}
} // end namespace itk

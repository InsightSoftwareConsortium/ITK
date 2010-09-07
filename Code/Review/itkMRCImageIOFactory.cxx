/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMRCImageIOFactory.cxx
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
#include "itkMRCImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkMRCImageIO.h"
#include "itkVersion.h"

namespace itk
{
MRCImageIOFactory::MRCImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkMRCImageIO",
                          "MRC Image IO",
                          1,
                          CreateObjectFunction< MRCImageIO >::New() );
}

MRCImageIOFactory::~MRCImageIOFactory()
{}

const char *
MRCImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char *
MRCImageIOFactory::GetDescription(void) const
{
  return "MRC ImageIO Factory, allows the loading of MRC images into ITK";
}
} // end namespace itk

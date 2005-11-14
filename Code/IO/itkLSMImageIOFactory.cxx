/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLSMImageIOFactory.cxx
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
#include "itkLSMImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkLSMImageIO.h"
#include "itkVersion.h"

namespace itk
{
LSMImageIOFactory::LSMImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkLSMImageIO",
                         "LSM Image IO",
                         1,
                         CreateObjectFunction<LSMImageIO>::New());
}
  
LSMImageIOFactory::~LSMImageIOFactory()
{
}

const char* 
LSMImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char* 
LSMImageIOFactory::GetDescription() const
{
  return "LSM ImageIO Factory, allows the loading of LSM images into ITK";
}

} // end namespace itk


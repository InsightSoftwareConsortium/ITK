/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBioRadImageIOFactory.cxx
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
#include "itkBioRadImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkBioRadImageIO.h"
#include "itkVersion.h"

namespace itk
{

BioRadImageIOFactory::BioRadImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkBioRadImageIO",
                         "BioRad Image IO",
                         1,
                         CreateObjectFunction<BioRadImageIO>::New());
}
  
BioRadImageIOFactory::~BioRadImageIOFactory()
{
}

const char* 
BioRadImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char* 
BioRadImageIOFactory::GetDescription() const
{
  return "BioRad ImageIO Factory, allows the loading of BioRad images into ITK";
}

} // end namespace itk


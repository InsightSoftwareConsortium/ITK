/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGDCMImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkGDCMImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkGDCMImageIO.h"
#include "itkVersion.h"

  
namespace itk
{
GDCMImageIOFactory::GDCMImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkGDCMImageIO",
                         "GDCM Image IO",
                         1,
                         CreateObjectFunction<GDCMImageIO>::New());
}
  
GDCMImageIOFactory::~GDCMImageIOFactory()
{
}

const char* GDCMImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char* GDCMImageIOFactory::GetDescription() const
{
  return "GDCM ImageIO Factory, allows the loading of DICOM images into Insight";
}

} // end namespace itk


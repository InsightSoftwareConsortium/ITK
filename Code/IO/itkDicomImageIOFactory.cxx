/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDicomImageIOFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkDicomImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkDicomImageIO.h"
#include "itkVersion.h"

  
namespace itk
{
DicomImageIOFactory::DicomImageIOFactory()
{
  this->RegisterOverride("itkImageIOBase",
                         "itkDicomImageIO",
                         "Dicom Image IO",
                         1,
                         CreateObjectFunction<DicomImageIO>::New());
}
  
DicomImageIOFactory::~DicomImageIOFactory()
{
}

const char* 
DicomImageIOFactory::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

const char* 
DicomImageIOFactory::GetDescription() const
{
  return "Dicom ImageIO Factory, allows the loading of Dicom images into Insight";
}

} // end namespace itk


/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVXLReaderFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkVXLReaderFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkVersion.h"

namespace itk
{
template <class TImage>
VXLReaderFactory <TImage> ::VXLReaderFactory()
{
  this->RegisterOverride( "itkVideoReaderBase",
                          "itkVXLReaderFactory",
                          "VXL Reader Base",
                          1,
                          CreateObjectFunction< VXLReader<TImage> >::New() );
}

template <class TImage>
VXLReaderFactory <TImage> ::~VXLReaderFactory()
{}

template <class TImage>
const char *
VXLReaderFactory <TImage> ::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

template <class TImage>
const char *
VXLReaderFactory <TImage> ::GetDescription(void) const
{
  return "VXL Reader Factory, allows the loading of video into insight using the VXL (vidl) library";
}
} // end namespace itk

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOpenCVReaderFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkOpenCVReaderFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkVersion.h"

namespace itk
{
template <class TImage>
OpenCVReaderFactory <TImage> ::OpenCVReaderFactory()
{
  this->RegisterOverride( "itkVideoReaderBase",
                          "itkOpenCVReader",
                          "OpenCV Video Reader",
                          1,
                          CreateObjectFunction< OpenCVReader<TImage> >::New() );
}

template <class TImage>
OpenCVReaderFactory <TImage> ::~OpenCVReaderFactory()
{}

template <class TImage>
const char *
OpenCVReaderFactory <TImage> ::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

template <class TImage>
const char *
OpenCVReaderFactory <TImage> ::GetDescription(void) const
{
  return "OpenCV Reader Factory, allows the loading of video into insight using the openCV library";
}
} // end namespace itk

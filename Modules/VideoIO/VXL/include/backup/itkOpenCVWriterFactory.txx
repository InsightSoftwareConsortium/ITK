/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOpenCVWriterFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkOpenCVWriterFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkVersion.h"

namespace itk
{
template <class TImage>
OpenCVWriterFactory <TImage> ::OpenCVWriterFactory()
{
  this->RegisterOverride( "itkVideoWriterBase",
                          "itkOpenCVWriter",
                          "OpenCV Video Writer",
                          1,
                          CreateObjectFunction< OpenCVWriter<TImage> >::New() );
}

template <class TImage>
OpenCVWriterFactory <TImage> ::~OpenCVWriterFactory()
{}

template <class TImage>
const char *
OpenCVWriterFactory <TImage> ::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

template <class TImage>
const char *
OpenCVWriterFactory <TImage> ::GetDescription(void) const
{
  return "OpenCV Writer Video Factory, allows the loading of video into insight using the openCV library";
}
} // end namespace itk

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOpenCVViewerFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkOpenCVViewerFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkVersion.h"

namespace itk
{
template <class TImage>
OpenCVViewerFactory <TImage> ::OpenCVViewerFactory()
{
  this->RegisterOverride( "itkVideoViewerBase",
                          "itkOpenCVViewer",
                          "OpenCV Video Viewer",
                          1,
                          CreateObjectFunction< OpenCVViewer<TImage> >::New() );
}

template <class TImage>
OpenCVViewerFactory <TImage> ::~OpenCVViewerFactory()
{}

template <class TImage>
const char *
OpenCVViewerFactory <TImage> ::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

template <class TImage>
const char *
OpenCVViewerFactory <TImage> ::GetDescription(void) const
{
  return "OpenCV Viewer Video Factory, allows the loading of video into insight using the openCV library";
}
} // end namespace itk

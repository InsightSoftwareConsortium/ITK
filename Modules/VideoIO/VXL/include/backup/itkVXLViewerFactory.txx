/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVXLViewerFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkVXLViewerFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkVersion.h"

namespace itk
{
template <class TImage>
VXLViewerFactory <TImage>::VXLViewerFactory()
{
  this->RegisterOverride( "itkVideoViewerBase",
                          "itkVXLViewer",
                          "VXL Video Viewer",
                          1,
                          CreateObjectFunction< VXLViewer<TImage> >::New() );
}

template <class TImage>
VXLViewerFactory <TImage> ::~VXLViewerFactory()
{}

template <class TImage>
const char *
VXLViewerFactory <TImage> ::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

template <class TImage>
const char *
VXLViewerFactory <TImage> ::GetDescription(void) const
{
  return "VXL Viewer Video Factory, allows the loading of video into insight using the VXL library";
}
} // end namespace itk

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVXLWriterFactory.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkVXLWriterFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkVersion.h"

namespace itk
{
template <class TImage>
VXLWriterFactory <TImage> ::VXLWriterFactory()
{
  this->RegisterOverride( "itkVideoWriterBase",
                          "itkVXLWriter",
                          "VXL Video Writer",
                          1,
                          CreateObjectFunction< VXLWriter<TImage> >::New() );
}

template <class TImage>
VXLWriterFactory <TImage> ::~VXLWriterFactory()
{}

template <class TImage>
const char *
VXLWriterFactory <TImage> ::GetITKSourceVersion(void) const
{
  return ITK_SOURCE_VERSION;
}

template <class TImage>
const char *
VXLWriterFactory <TImage> ::GetDescription(void) const
{
  return "VXL Writer Video Factory, allows the loading of video into insight using the VXL (vidl) library";
}
} // end namespace itk

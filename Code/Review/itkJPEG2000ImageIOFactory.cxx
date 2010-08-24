/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkJPEG2000ImageIOFactory.cxx,v $
  Language:  C++
  Date:      $Date: 2007/03/22 14:28:51 $
  Version:   $Revision: 1.9 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkJPEG2000ImageIOFactory.h"
#include "itkCreateObjectFunction.h"
#include "itkJPEG2000ImageIO.h"
#include "itkVersion.h"

namespace itk
{
JPEG2000ImageIOFactory::JPEG2000ImageIOFactory()
{
  this->RegisterOverride( "itkImageIOBase",
                          "itkJPEG2000ImageIO",
                          "JPEG2000 Image IO",
                          1,
                          CreateObjectFunction< JPEG2000ImageIO >::New() );
}

JPEG2000ImageIOFactory::~JPEG2000ImageIOFactory()
{}

const char *
JPEG2000ImageIOFactory::GetITKSourceVersion() const
{
  return ITK_SOURCE_VERSION;
}

const char *
JPEG2000ImageIOFactory::GetDescription() const
{
  return "JPEG2000 ImageIO Factory, allows the loading of JPEG2000 images into insight";
}

//
// Entry point function for ITK to invoke, in order to create a new instance of
// a factory.
//
extern "C"
#ifdef _WIN32
__declspec(dllexport)
#endif
itk::ObjectFactoryBase * itkLoad()
  {
  std::cout << "Calling JPEG2000ImageIO itkLoad()" << std::endl;
  return itk::JPEG2000ImageIOFactory::FactoryNew();
  }
} // end namespace itk

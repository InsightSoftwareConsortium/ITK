/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: ImageReadRegionOfInterestWrite.cxx,v $
  Language:  C++
  Date:      $Date: 2005/08/27 01:46:11 $
  Version:   $Revision: 1.12 $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


#include "itkJPEG2000ImageIOFactory.h"


int itkJPEG2000ImageIOFactoryTest01( int /*argc */, char * /*argv*/[] )
{
  //  Register the factory
  itk::JPEG2000ImageIOFactory::RegisterOneFactory();

  itk::JPEG2000ImageIOFactory::Pointer factory =
    itk::JPEG2000ImageIOFactory::New();

  std::cout << "ITK Version = " << factory->GetITKSourceVersion() << std::endl;
  std::cout << "Description = " << factory->GetDescription() << std::endl;

  std::cout << "ClassName = " << factory->GetNameOfClass() << std::endl;

  itk::JPEG2000ImageIOFactory::Pointer factory2 =
    itk::JPEG2000ImageIOFactory::FactoryNew();

  return EXIT_SUCCESS;
}

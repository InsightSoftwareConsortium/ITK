/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkAnalyzeImageIOTest.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImage.h"
#include "iplitk/itkAnalyzeImageIOFactory.h"

int itkAnalyzeImageIOTest(int ac, char** av)
{
  if(ac < 2)
  {
    std::cerr << "Usage: " << av[0] << " Image\n";
    return EXIT_FAILURE;
  }

  // ATTENTION THIS IS THE PIXEL TYPE FOR
  // THE RESULTING IMAGE
  typedef unsigned short PixelType;

  typedef itk::Image<PixelType, 3> myImage;

  itk::ImageFileReader<myImage>::Pointer reader
    = itk::ImageFileReader<myImage>::New();

  // Register on factory capable of creating AnalyzeImage readers
  itk::AnalyzeImageIOFactory::RegisterOneFactory();

  reader->DebugOn();
  reader->SetFileName(av[1]);

  try
  {
    reader->Update();
  }
  catch (itk::ExceptionObject & e)
  {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e.GetDescription() << std::endl;
    std::cerr << e.GetLocation() << std::endl;
    return EXIT_FAILURE;
  }

  myImage::Pointer image = reader->GetOutput();

  image->Print(std::cout );

  myImage::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region;

  PixelType * data = image->GetPixelContainer()->GetBufferPointer();

  unsigned long numberOfPixels = region.GetNumberOfPixels();
  for(unsigned int i=0; i < numberOfPixels; i++ )
  {
    std::cout << i << " : " << *data++ << std::endl;
  }
  return EXIT_SUCCESS;
}

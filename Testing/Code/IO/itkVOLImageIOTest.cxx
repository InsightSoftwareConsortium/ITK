/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVOLImageIOTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageFileReader.h"
#include "itkVOLImageIOFactory.h"
#include "itkImage.h"

int itkVOLImageIOTest(int ac, char* av[])
{

  if(ac < 2)
    {
    std::cerr << "Usage: " << av[0] << " Image\n";
    return 1;
    }

  // Register at least one factory capable of producing 
  // VOL image file readers
  itk::VOLImageIOFactory::RegisterOneFactory();

  typedef unsigned char PixelType;
  typedef itk::Image<PixelType, 4> myImage;
  itk::ImageFileReader<myImage>::Pointer reader = itk::ImageFileReader<myImage>::New();
  //reader->DebugOn(); 
  reader->SetFileName(av[1]); 
  try
    {
    reader->Update();
    }
  catch (itk::ImageFileReaderException& e)
    {
    std::cout << "exception in file reader \n"  << e.GetDescription();
    return EXIT_FAILURE;
    }

  myImage::Pointer image = reader->GetOutput();
  image->Print(std::cout);
  myImage::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region;

  return EXIT_SUCCESS;
}

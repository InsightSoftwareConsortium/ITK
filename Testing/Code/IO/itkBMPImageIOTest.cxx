/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBMPImageIOTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"

int itkBMPImageIOTest( int ac, char* av[] )
{

  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " Input Output\n";
    return EXIT_FAILURE;
    }


  // ATTENTION THIS IS THE PIXEL TYPE FOR 
  // THE RESULTING IMAGE
  typedef itk::RGBPixel<unsigned char> PixelType;
  typedef itk::Image<PixelType, 2> myImage;

  itk::ImageFileReader<myImage>::Pointer reader 
                                  = itk::ImageFileReader<myImage>::New();
  
  reader->SetFileName(av[1]);

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in image file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
  
  myImage::Pointer image = reader->GetOutput();

  image->Print(std::cout );
  
  myImage::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region;

  // Print the IO
  reader->GetImageIO()->Print(std::cout); 

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  writer->SetInput( reader->GetOutput() );
  writer->SetFileName(av[2]);
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in image file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  // Print the IO
  writer->GetImageIO()->Print(std::cout); 

  return EXIT_SUCCESS;

}

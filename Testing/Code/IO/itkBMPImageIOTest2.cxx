/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBMPImageIOTest2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkRGBAPixel.h"
#include <fstream>

int itkBMPImageIOTest2( int ac, char* av[] )
{

  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " Input Output\n";
    return EXIT_FAILURE;
    }


  // ATTENTION THIS IS THE PIXEL TYPE FOR 
  // THE RESULTING IMAGE
  typedef itk::RGBAPixel<unsigned char> PixelType;
  typedef itk::Image<PixelType, 2> myImage;

  typedef itk::ImageFileReader<myImage>   ReaderType;
  
  ReaderType::Pointer reader = ReaderType::New();
  
  reader->SetFileName(av[1]);
  reader->UpdateOutputInformation();

  std::cout << "PixelType: " << reader->GetImageIO()->GetPixelTypeAsString(reader->GetImageIO()->GetPixelType()) << std::endl;
  std::cout << "ComponentType: " << reader->GetImageIO()->GetComponentTypeAsString(reader->GetImageIO()->GetComponentType()) << std::endl;
  std::cout << "NumberOfComponents: " << reader->GetImageIO()->GetNumberOfComponents() << std::endl;
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

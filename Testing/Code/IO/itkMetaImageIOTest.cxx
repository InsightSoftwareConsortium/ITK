/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaImageIOTest.cxx
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
#include "itkMetaImageIO.h"

int itkMetaImageIOTest(int ac, char* av[])
{
  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " Input Output [ShouldFail]\n";
    return EXIT_FAILURE;
    }
  
  // ATTENTION THIS IS THE PIXEL TYPE FOR 
  // THE RESULTING IMAGE
  typedef unsigned short PixelType;
  typedef itk::Image<PixelType, 3> myImage;

  itk::ImageFileReader<myImage>::Pointer reader 
                                  = itk::ImageFileReader<myImage>::New();
  
  // force use of MetaIO
  typedef itk::MetaImageIO IOType;
  IOType::Pointer metaIn = IOType::New();
  metaIn->SetDoublePrecision(8);  // Set manually for coverage
  reader->SetImageIO(metaIn);
  
  // check usability of dimension (for coverage)
  if (!metaIn->SupportsDimension(3))
    {
    std::cerr << "Did not support dimension 3" << std::endl;
    return EXIT_FAILURE;
    }
  
  // test subsampling factor (change it then change it back)
  unsigned int origSubSamplingFactor = metaIn->GetSubSamplingFactor();
  unsigned int subSamplingFactor = 2;
  metaIn->SetSubSamplingFactor(subSamplingFactor);
  if (metaIn->GetSubSamplingFactor() != subSamplingFactor)
    {
    std::cerr << "Did not set/get Sub Sampling factor correctly" << std::endl;
    return EXIT_FAILURE;
    }
  metaIn->SetSubSamplingFactor(origSubSamplingFactor);
  
  // read the file
  reader->SetFileName(av[1]);
  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
    if(ac == 3) // should fail
      {
      return EXIT_SUCCESS;
      }
    return EXIT_FAILURE;
    }
  
  myImage::Pointer image = reader->GetOutput();
  image->Print(std::cout );
  
  myImage::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region;

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  IOType::Pointer metaOut = IOType::New();
  writer->SetImageIO(metaOut);
  writer->SetInput( reader->GetOutput() );
  writer->SetFileName( av[2] );
  writer->Update();

  return EXIT_SUCCESS;
}

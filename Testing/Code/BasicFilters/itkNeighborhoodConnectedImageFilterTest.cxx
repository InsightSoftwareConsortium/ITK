/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodConnectedImageFilterTest.cxx
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
#include "itkNeighborhoodConnectedImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"

int itkNeighborhoodConnectedImageFilterTest(int ac, char* av[] )
{
  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage\n";
    return -1;
    }

  typedef unsigned char PixelType;
  typedef itk::Image<PixelType, 2> myImage;
  itk::ImageFileReader<myImage>::Pointer input 
    = itk::ImageFileReader<myImage>::New();
  input->SetFileName(av[1]);
  
  // Create a filter
  typedef itk::NeighborhoodConnectedImageFilter<myImage,myImage> FilterType;

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(input->GetOutput());
  
  FilterType::IndexType seed;
  
  seed[0] = 146; seed[1] = 88;
  filter->SetSeed(seed);
  
  filter->SetLower (0);
  filter->SetUpper (210);
  typedef FilterType::InputImageSizeType SizeType;
  SizeType radius;
  radius.Fill(5);
  
  filter->SetRadius(radius);
  filter->SetReplaceValue(255);
  
  // Test GetMacros
  PixelType lower = filter->GetLower();
  std::cout << "filter->GetLower(): " << lower << std::endl;
  PixelType upper  = filter->GetUpper();
  std::cout << "filter->GetUpper(): " << upper << std::endl;
  PixelType replaceValue = filter->GetReplaceValue();
  std::cout << "filter->GetReplaceValue(): " << replaceValue << std::endl;
  
  // Test GetConstReferenceMacro
  const SizeType & radius2 = filter->GetRadius();
  std::cout << "filter->GetRadius(): " << radius2 << std::endl;


  try
    {
    input->Update();
    filter->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return -1;
    }

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
    writer = itk::ImageFileWriter<myImage>::New();
    writer->SetInput( filter->GetOutput() );
    writer->SetFileName( av[2] );
    writer->Update();

    return 0;
}

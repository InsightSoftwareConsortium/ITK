/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIsolatedConnectedImageFilterTest.cxx
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
#include "itkIsolatedConnectedImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkFilterWatcher.h"

int itkIsolatedConnectedImageFilterTest(int ac, char* av[] )
{
  if(ac < 7)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage seed1_x seed1_y seed2_x seed2_y\n";
    return -1;
    }

  typedef unsigned char PixelType;
  typedef itk::Image<PixelType, 2> myImage;
  itk::ImageFileReader<myImage>::Pointer input 
    = itk::ImageFileReader<myImage>::New();
  input->SetFileName(av[1]);
  
  // Create a filter
  typedef itk::IsolatedConnectedImageFilter<myImage,myImage> FilterType;

  FilterType::Pointer filter = FilterType::New();
  FilterWatcher watcher(filter);

  filter->SetInput(input->GetOutput());
  
  FilterType::IndexType seed1;
  
  seed1[0] = atoi(av[3]); seed1[1] = atoi(av[4]);
  filter->SetSeed1(seed1);
  
  seed1[0] = atoi(av[5]); seed1[1] = atoi(av[6]);
  filter->SetSeed2(seed1);
  
  filter->SetLower(0);
  filter->SetReplaceValue(255);
  filter->SetUpperValueLimit(250);
    
  // Test SetMacro
  filter->SetIsolatedValueTolerance(1);
  
  // Test GetMacros
  PixelType lower = filter->GetLower();
  std::cout << "filter->GetLower(): "
            << static_cast<itk::NumericTraits<PixelType>::PrintType>(lower)
            << std::endl;
  PixelType isolatedValueTolerance = filter->GetIsolatedValueTolerance();
  std::cout << "filter->GetIsolatedValueTolerance(): " 
            << static_cast<itk::NumericTraits<PixelType>::PrintType>(isolatedValueTolerance)
            << std::endl;
  PixelType upperValueLimit = filter->GetUpperValueLimit();
  std::cout << "filter->GetUpperValueLimit(): "
            << static_cast<itk::NumericTraits<PixelType>::PrintType>(upperValueLimit)
            << std::endl;
  PixelType replaceValue = filter->GetReplaceValue();
  std::cout << "filter->GetReplaceValue(): "
            << static_cast<itk::NumericTraits<PixelType>::PrintType>(replaceValue)
            << std::endl;
  

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

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShiftScaleImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>

#include "itkImage.h"
#include "itkImageRegionIterator.h"

#include "itkShiftScaleImageFilter.h"
#include "itkRandomImageSource.h"

#include "itkFilterWatcher.h"
int itkShiftScaleImageFilterTest(int, char* [] )
{
  std::cout << "itkShiftScaleImageFilterTest Start" << std::endl;

  typedef itk::Image<char,3> TestInputImage;
  typedef itk::Image<unsigned char,3> TestOutputImage;

  TestInputImage::Pointer    inputImage  = TestInputImage::New();
  TestInputImage::RegionType region;
  TestInputImage::SizeType   size; size.Fill(64);
  TestInputImage::IndexType  index; index.Fill(0);

  region.SetIndex (index);
  region.SetSize (size);

  // first try a constant image
  double fillValue = -100.0;
  inputImage->SetRegions( region );
  inputImage->Allocate();
  inputImage->FillBuffer( static_cast< TestInputImage::PixelType >( fillValue ) );

  typedef itk::ShiftScaleImageFilter<TestInputImage,TestOutputImage> FilterType;
  FilterType::Pointer filter = FilterType::New();

  // Set up Start, End and Progress callbacks
  FilterWatcher filterWatch(filter);

  // Filter the image
  filter->SetInput (inputImage);
  filter->UpdateLargestPossibleRegion();

  // Now generate a real image

  typedef itk::RandomImageSource<TestInputImage> SourceType;
  SourceType::Pointer source = SourceType::New();
  unsigned long randomSize[3] = {17, 8, 20};

  // Set up Start, End and Progress callbacks
  FilterWatcher sourceWatch(source);

  // Set up source
  source->SetSize(randomSize);
  double minValue = -128.0;
  double maxValue = 127.0;

  source->SetMin( static_cast< TestInputImage::PixelType >( minValue ) );
  source->SetMax( static_cast< TestInputImage::PixelType >( maxValue ) );
  std::cout << source;
  
  filter->SetInput(source->GetOutput());
  filter->SetScale(4.0);
  try
    {
    filter->UpdateLargestPossibleRegion();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e;
    return -1;
    }
  
  return 0;
}

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNormalizeImageFilterTest.cxx
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

#include "itkNormalizeImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkStatisticsImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkFilterWatcher.h"

int main()
{
  std::cout << "itkNormalizeImageFilterTest Start" << std::endl;

  int status = 0;
  typedef itk::Image<short,3> ShortImage;
  typedef itk::Image<float,3> FloatImage;

  ShortImage::Pointer    image  = ShortImage::New();

  // Generate a real image

  typedef itk::RandomImageSource<ShortImage> SourceType;
  SourceType::Pointer source = SourceType::New();
  unsigned long randomSize[3] = {18, 17, 67};

  source->SetSize(randomSize);
  float minValue = -1000.0;
  float maxValue = 1000.0;

  source->SetMin(minValue);
  source->SetMax(maxValue);

  typedef itk::NormalizeImageFilter<ShortImage,FloatImage> NormalizeType;
  NormalizeType::Pointer normalize = NormalizeType::New();
  FilterWatcher watch(normalize);

  normalize->SetInput(source->GetOutput());
  normalize->UpdateLargestPossibleRegion();

  typedef itk::StreamingImageFilter<FloatImage,FloatImage> StreamingType;
  StreamingType::Pointer streaming = StreamingType::New();
  
  streaming->SetNumberOfStreamDivisions(10);
  streaming->SetInput (normalize->GetOutput());
  streaming->Update();
#if 0
  typedef itk::StatisticsImageFilter<FloatImage> StatisticsType;
  StatisticsType::Pointer statistics = StatisticsType::New();

  statistics->SetInput(normalize->GetOutput());
  statistics->UpdateLargestPossibleRegion();

  std::cout << "Mean is: " << statistics->GetMean() << " Sigma is: " << statistics->GetSigma() << std::endl;
#endif
  return 0;
}

/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>

#include "itkNormalizeImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkStreamingImageFilter.h"
#include "itkFilterWatcher.h"

int itkNormalizeImageFilterTest(int, char* [] )
{
  std::cout << "itkNormalizeImageFilterTest Start" << std::endl;

  typedef itk::Image<short,3> ShortImage;
  typedef itk::Image<float,3> FloatImage;

  // Generate a real image
  typedef itk::RandomImageSource<ShortImage> SourceType;
  SourceType::Pointer source = SourceType::New();
  ShortImage::SizeValueType randomSize[3] = {18, 17, 67};

  source->SetSize( randomSize );
  float minValue = -1000.0;
  float maxValue =  1000.0;

  source->SetMin( static_cast<ShortImage::PixelType>( minValue ) );
  source->SetMax( static_cast<ShortImage::PixelType>( maxValue ) );

  typedef itk::NormalizeImageFilter<ShortImage,FloatImage> NormalizeType;
  NormalizeType::Pointer normalize = NormalizeType::New();
  FilterWatcher watch(normalize, "Streaming");

  normalize->SetInput(source->GetOutput());

  typedef itk::StreamingImageFilter<FloatImage,FloatImage> StreamingType;
  StreamingType::Pointer streaming = StreamingType::New();

  streaming->SetNumberOfStreamDivisions(5);
  streaming->SetInput (normalize->GetOutput());
  streaming->Update();

  // Force the filter to re-execute
  source->Modified();

  typedef itk::StatisticsImageFilter<FloatImage> StatisticsType;
  StatisticsType::Pointer statistics = StatisticsType::New();

  statistics->SetInput(streaming->GetOutput());
  statistics->UpdateLargestPossibleRegion();

  std::cout << "Mean is: " << statistics->GetMean() << " Sigma is: " << statistics->GetSigma() << std::endl;


  return EXIT_SUCCESS;
}

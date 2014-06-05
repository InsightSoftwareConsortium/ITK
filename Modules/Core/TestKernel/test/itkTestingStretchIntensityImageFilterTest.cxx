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

#include "itkTestingMacros.h"
#include "itkTestingStretchIntensityImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include "itkImageRegionIterator.h"

int itkTestingStretchIntensityImageFilterTest(int, char* [] )
{
  typedef signed short                                         PixelType;
  typedef itk::Image<PixelType,2>                              ImageType;
  typedef itk::Testing::StretchIntensityImageFilter<ImageType> StretchFilterType;
  typedef itk::StatisticsImageFilter<ImageType>                StatsFilterType;

  ImageType::SizeType imageSize = {{ 32,32 }};
  ImageType::Pointer  image = ImageType::New();
  image->SetRegions(imageSize);
  image->Allocate();
  PixelType i = -511;
  for(itk::ImageRegionIterator<ImageType> it(image,image->GetLargestPossibleRegion());
      !it.IsAtEnd(); ++it, ++i)
    {
    it.Set(i);
    }

  StretchFilterType::Pointer stretchFilter = StretchFilterType::New();
  const PixelType outputMinPix(-5000);
  const PixelType outputMaxPix(16384);
  stretchFilter->SetOutputMinimum(outputMinPix);
  stretchFilter->SetOutputMaximum(outputMaxPix);
  TEST_SET_GET_VALUE(outputMinPix, stretchFilter->GetOutputMinimum());
  TEST_SET_GET_VALUE(outputMaxPix, stretchFilter->GetOutputMaximum());

  stretchFilter->SetInput(image);
  std::cout << stretchFilter;

  TRY_EXPECT_NO_EXCEPTION(stretchFilter->Update());

  const StretchFilterType::RealType scale = stretchFilter->GetScale();
  const StretchFilterType::RealType shift = stretchFilter->GetShift();
  const PixelType                   inputMinimum = stretchFilter->GetInputMinimum();
  const PixelType                   inputMaximum = stretchFilter->GetInputMaximum();
  std::cout << "Scale: " << scale << std::endl
            << "Shift: " << shift << std::endl
            << "InputMinimum: " << inputMinimum << std::endl
            << "InputMaximum: " << inputMaximum << std::endl;
  TEST_EXPECT_EQUAL(inputMinimum, -511);
  TEST_EXPECT_EQUAL(inputMaximum, 512);

  StatsFilterType::Pointer statsFilter = StatsFilterType::New();
  statsFilter->SetInput(stretchFilter->GetOutput());
  TRY_EXPECT_NO_EXCEPTION(statsFilter->Update());

  TEST_EXPECT_EQUAL(statsFilter->GetMinimum(), -5000);
  TEST_EXPECT_EQUAL(statsFilter->GetMaximum(), 16384);

  std::cout << "Output Minimum: " << statsFilter->GetMinimum() << std::endl
            << "Output Maximum: " << statsFilter->GetMaximum() << std::endl
            << "Output Mean: " << statsFilter->GetMean() << std::endl
            << "Output Variance: " << statsFilter->GetVariance() << std::endl
            << "Output Sigma: " << statsFilter->GetSigma() << std::endl;
  return EXIT_SUCCESS;
}

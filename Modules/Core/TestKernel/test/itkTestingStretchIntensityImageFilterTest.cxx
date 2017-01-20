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

int itkTestingStretchIntensityImageFilterTest( int itkNotUsed( argc ), char* itkNotUsed( argv )[] )
{
  const unsigned int Dimension = 2;
  typedef signed short                                            PixelType;
  typedef itk::Image< PixelType, Dimension >                      ImageType;
  typedef itk::Testing::StretchIntensityImageFilter< ImageType >  StretchFilterType;
  typedef itk::StatisticsImageFilter< ImageType >                 StatsFilterType;

  ImageType::SizeType imageSize = {{ 32, 32 }};
  ImageType::Pointer  image = ImageType::New();
  image->SetRegions( imageSize );
  image->Allocate();
  PixelType i = -511;
  for( itk::ImageRegionIterator<ImageType> it( image, image->GetLargestPossibleRegion() );
      !it.IsAtEnd(); ++it, ++i )
    {
    it.Set( i );
    }

  StretchFilterType::Pointer stretchFilter = StretchFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( stretchFilter, StretchIntensityImageFilter, ImageSource );

  stretchFilter->SetInput( image );

  int outputMinimumPixelValue = 16384;
  int outputMaximumPixelValue = -5000;
  PixelType outputMinPix( outputMinimumPixelValue );
  PixelType outputMaxPix( outputMaximumPixelValue );
  stretchFilter->SetOutputMinimum( outputMinPix );
  stretchFilter->SetOutputMaximum( outputMaxPix );

  TRY_EXPECT_EXCEPTION( stretchFilter->Update() );

  outputMinimumPixelValue = -5000;
  outputMaximumPixelValue = 16384;
  outputMinPix = outputMinimumPixelValue;
  outputMaxPix = outputMaximumPixelValue;
  stretchFilter->SetOutputMinimum( outputMinPix );
  stretchFilter->SetOutputMaximum( outputMaxPix );

  TEST_SET_GET_VALUE( outputMinimumPixelValue, stretchFilter->GetOutputMinimum() );
  TEST_SET_GET_VALUE( outputMaximumPixelValue, stretchFilter->GetOutputMaximum() );


  TRY_EXPECT_NO_EXCEPTION( stretchFilter->Update() );

  std::cout << "Scale: " << stretchFilter->GetScale() << std::endl;
  std::cout << "Shift: " << stretchFilter->GetShift() << std::endl;

  TEST_EXPECT_EQUAL( stretchFilter->GetInputMinimum(), -511 );
  TEST_EXPECT_EQUAL( stretchFilter->GetInputMaximum(), 512 );

  StatsFilterType::Pointer statsFilter = StatsFilterType::New();
  statsFilter->SetInput( stretchFilter->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( statsFilter->Update() );

  TEST_EXPECT_EQUAL( outputMinimumPixelValue, statsFilter->GetMinimum() );
  TEST_EXPECT_EQUAL( outputMaximumPixelValue, statsFilter->GetMaximum() );

  std::cout << "Output Minimum: " << statsFilter->GetMinimum() << std::endl
            << "Output Maximum: " << statsFilter->GetMaximum() << std::endl
            << "Output Mean: " << statsFilter->GetMean() << std::endl
            << "Output Variance: " << statsFilter->GetVariance() << std::endl
            << "Output Sigma: " << statsFilter->GetSigma() << std::endl;

  return EXIT_SUCCESS;
}

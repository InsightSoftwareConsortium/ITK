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

#include "itkAddImageFilter.h"
#include "itkFrequencyBandImageFilter.h"
#include "itkFrequencyShiftedFFTLayoutImageRegionIteratorWithIndex.h"
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkTestingMacros.h"

int
itkFrequencyBandImageFilterTest( int argc, char* argv[] )
{
  constexpr unsigned int Dimension = 3;

  if ( argc != 2 )
    {
    std::cerr << "Usage: " << argv[0]
              << " Even|Odd" << std::endl;
    return EXIT_FAILURE;
    }

  const std::string evenOrOddInput = argv[1];
  bool isOdd = false;
  if ( evenOrOddInput == "Even" )
    {
    isOdd = false;
    }
  else if ( evenOrOddInput == "Odd" )
    {
    isOdd = true;
    }
  else
    {
    std::cerr << "Unkown string: " + evenOrOddInput + " . Use Even or Odd." << std::endl;
    return EXIT_FAILURE;
    }

  using PixelType = float;
  using ImageType3D = itk::Image< PixelType, Dimension >;

  ImageType3D::SizeType size = { { 10, 20, 40 } };
  if ( isOdd )
    {
    for ( unsigned int i = 0; i < Dimension; ++i )
      {
      size[i]++;
      }
    }

  // Create an image
  auto image = ImageType3D::New();
  ImageType3D::RegionType region;
  region.SetSize( size );

  image->SetRegions(region);
  image->SetLargestPossibleRegion( region );
  image->SetBufferedRegion( region );
  image->Allocate();
  image->FillBuffer( 1.0 );

  using BandFilterType = itk::FrequencyBandImageFilter< ImageType3D >;
  auto passBandFilter = BandFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( passBandFilter, FrequencyBandImageFilter,
    InPlaceImageFilter );

  passBandFilter->SetInput( image );

  // Test exception cases
  BandFilterType::FrequencyValueType lowFreqThreshold = 0.5;
  passBandFilter->SetLowFrequencyThreshold( lowFreqThreshold );
  TEST_SET_GET_VALUE( lowFreqThreshold, passBandFilter->GetLowFrequencyThreshold() );

  BandFilterType::FrequencyValueType highFreqThreshold = 0.0;
  passBandFilter->SetHighFrequencyThreshold( highFreqThreshold );
  TEST_SET_GET_VALUE( highFreqThreshold, passBandFilter->GetHighFrequencyThreshold() );


  TRY_EXPECT_EXCEPTION( passBandFilter->Update() );


  lowFreqThreshold = 0.0;
  passBandFilter->SetLowFrequencyThreshold( lowFreqThreshold );
  TEST_SET_GET_VALUE( lowFreqThreshold, passBandFilter->GetLowFrequencyThreshold() );

  highFreqThreshold = 0.5;
  passBandFilter->SetHighFrequencyThreshold( highFreqThreshold );
  TEST_SET_GET_VALUE( highFreqThreshold, passBandFilter->GetHighFrequencyThreshold() );

  bool passBand = true;
  TEST_SET_GET_BOOLEAN(passBandFilter, PassBand, passBand);

  bool passLowFreqThreshold = true;
  TEST_SET_GET_BOOLEAN(passBandFilter, PassLowFrequencyThreshold, passLowFreqThreshold);

  bool passHighFreqThreshold = true;
  TEST_SET_GET_BOOLEAN(passBandFilter, PassHighFrequencyThreshold, passHighFreqThreshold);

  passBandFilter->SetPassBand( passLowFreqThreshold, passHighFreqThreshold );
  TEST_SET_GET_VALUE( passLowFreqThreshold, passBandFilter->GetPassLowFrequencyThreshold() );
  TEST_SET_GET_VALUE( passHighFreqThreshold, passBandFilter->GetPassHighFrequencyThreshold() );


  TRY_EXPECT_NO_EXCEPTION( passBandFilter->Update() );

  // Stop-band
  auto stopBandFilter = BandFilterType::New();

  stopBandFilter->SetInput(image);

  stopBandFilter->SetLowFrequencyThreshold( lowFreqThreshold );
  stopBandFilter->SetHighFrequencyThreshold( highFreqThreshold );

  passLowFreqThreshold = false;
  passHighFreqThreshold = false;
  stopBandFilter->SetStopBand( passLowFreqThreshold, passHighFreqThreshold );
  TEST_SET_GET_VALUE( passLowFreqThreshold, stopBandFilter->GetPassLowFrequencyThreshold() );
  TEST_SET_GET_VALUE( passHighFreqThreshold, stopBandFilter->GetPassHighFrequencyThreshold() );

  TRY_EXPECT_NO_EXCEPTION( stopBandFilter->Update() );

  // Regression test
  // Sum of bandPass and stopBand images with these settings should be equal
  // to original image
  using AddFilterType = itk::AddImageFilter< ImageType3D, ImageType3D >;
  auto addFilter = AddFilterType::New();
  addFilter->SetInput1( passBandFilter->GetOutput() );
  addFilter->SetInput2( stopBandFilter->GetOutput() );

  using DifferenceFilterType = itk::Testing::ComparisonImageFilter< ImageType3D, ImageType3D >;
  auto differenceFilter = DifferenceFilterType::New();

  differenceFilter->SetToleranceRadius( 0 );
  differenceFilter->SetDifferenceThreshold( 0 );
  differenceFilter->SetValidInput( image );
  differenceFilter->SetTestInput( addFilter->GetOutput() );

  differenceFilter->Update();

  unsigned int numberOfDiffPixels = differenceFilter->GetNumberOfPixelsWithDifferences();
  if ( numberOfDiffPixels > 0 )
    {
    std::cerr << "Test failed! " << std::endl;
    std::cerr << "Expected images to be equal, but got " << numberOfDiffPixels
              << "unequal pixels" << std::endl;
    return EXIT_FAILURE;
    }

  // Tests with radians

  BandFilterType::FrequencyValueType lowFreqThresholdRadians = itk::Math::pi_over_4;
  passBandFilter->SetLowFrequencyThresholdInRadians( lowFreqThresholdRadians );

  BandFilterType::FrequencyValueType highFreqThresholdRadians = itk::Math::pi_over_2;
  passBandFilter->SetHighFrequencyThresholdInRadians( highFreqThresholdRadians );

  BandFilterType::FrequencyValueType knownLowFrequencyHertz =
    lowFreqThresholdRadians / (2 * itk::Math::pi);
  BandFilterType::FrequencyValueType knownHighFrequencyHertz =
    highFreqThresholdRadians / (2 * itk::Math::pi);

  if ( itk::Math::NotAlmostEquals( knownLowFrequencyHertz,
         passBandFilter->GetLowFrequencyThreshold())
       || itk::Math::NotAlmostEquals( knownHighFrequencyHertz,
         passBandFilter->GetHighFrequencyThreshold()) )
    {
    std::cerr << "Test failed! " << std::endl;
    std::cerr << "Setting frequency in radians failed." << std::endl;
    return EXIT_FAILURE;
    }

  TRY_EXPECT_NO_EXCEPTION( passBandFilter->Update() );

  // Test the non-radial cut-off.
  // Don't pass negative frequency thresholds.

  bool radialBand = false;
  TEST_SET_GET_BOOLEAN(passBandFilter, RadialBand, radialBand);
  bool passNegativeLowFrequencyThreshold = false;
  TEST_SET_GET_BOOLEAN(passBandFilter, PassNegativeLowFrequencyThreshold, passNegativeLowFrequencyThreshold);
  bool passNegativeHighFrequencyThreshold = false;
  TEST_SET_GET_BOOLEAN(passBandFilter, PassNegativeHighFrequencyThreshold, passNegativeHighFrequencyThreshold);
  passBandFilter->Update();

  // Test with ShiftedIterator.
  using FrequencyShiftedIterator = itk::FrequencyShiftedFFTLayoutImageRegionIteratorWithIndex< ImageType3D >;
  using BandShiftedFilterType = itk::FrequencyBandImageFilter< ImageType3D, FrequencyShiftedIterator >;
  auto passBandShiftedFilter = BandShiftedFilterType::New();

  passBandShiftedFilter->SetInput( image );
  passBandShiftedFilter->SetLowFrequencyThreshold( lowFreqThreshold );
  passBandShiftedFilter->SetHighFrequencyThreshold( highFreqThreshold );
  passBandShiftedFilter->SetPassBand(true);
  passLowFreqThreshold = false;
  passHighFreqThreshold = true;
  passBandShiftedFilter->SetPassBand( passLowFreqThreshold, passHighFreqThreshold );
  TRY_EXPECT_NO_EXCEPTION( passBandShiftedFilter->Update() );

  // Stop-band with InPlaceOn
  auto stopBandInPlaceFilter = BandFilterType::New();
  stopBandInPlaceFilter->InPlaceOn();

  stopBandInPlaceFilter->SetInput(image);

  stopBandInPlaceFilter->SetLowFrequencyThreshold( lowFreqThreshold );
  stopBandInPlaceFilter->SetHighFrequencyThreshold( highFreqThreshold );

  passLowFreqThreshold = false;
  passHighFreqThreshold = false;
  stopBandInPlaceFilter->SetStopBand( passLowFreqThreshold, passHighFreqThreshold );

  TRY_EXPECT_NO_EXCEPTION( stopBandInPlaceFilter->Update() );

  auto differenceInPlaceFilter = DifferenceFilterType::New();

  differenceInPlaceFilter->SetToleranceRadius( 0 );
  differenceInPlaceFilter->SetDifferenceThreshold( 0 );
  differenceInPlaceFilter->SetValidInput( stopBandFilter->GetOutput() );
  differenceInPlaceFilter->SetTestInput( stopBandInPlaceFilter->GetOutput() );

  differenceInPlaceFilter->Update();

  if ( numberOfDiffPixels > 0 )
    {
    std::cerr << "Test failed! " << std::endl;
    std::cerr << "Expected images to be equal, but got " << differenceInPlaceFilter->GetNumberOfPixelsWithDifferences()
              << "unequal pixels" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

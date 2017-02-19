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
#include "itkFrequencyBandPassImageFilter.h"
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkTestingMacros.h"


// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

int
itkFrequencyBandPassImageFilterTest(int, char *[])
{
  const unsigned int Dimension = 3;

  typedef float                            PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType3D;

  ImageType3D::SizeType size = { { 20, 40, 80 } };

  // Create an image
  ImageType3D::Pointer    image = ImageType3D::New();
  ImageType3D::RegionType region;
  region.SetSize(size);

  image->SetRegions(region);
  image->SetLargestPossibleRegion(region);
  image->SetBufferedRegion(region);
  image->Allocate();
  image->FillBuffer(1.0);

  typedef itk::FrequencyBandPassImageFilter<ImageType3D> BandPassFilterType;
  BandPassFilterType::Pointer                            bandPassFilter = BandPassFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS(bandPassFilter, FrequencyBandPassImageFilter, ImageToImageFilter);

  bandPassFilter->SetInput(image);

  BandPassFilterType::FrequencyValueType lowFreqThreshold = 0.0;
  bandPassFilter->SetLowFrequencyThreshold(lowFreqThreshold);
  TEST_SET_GET_VALUE(lowFreqThreshold, bandPassFilter->GetLowFrequencyThreshold());

  BandPassFilterType::FrequencyValueType highFreqThreshold = 0.5;
  bandPassFilter->SetHighFrequencyThreshold(highFreqThreshold);
  TEST_SET_GET_VALUE(highFreqThreshold, bandPassFilter->GetHighFrequencyThreshold());

  bool passBand = true;
  bandPassFilter->SetPassBand(passBand);
  TEST_SET_GET_VALUE(passBand, bandPassFilter->GetPassBand());

  bool passLowFreqThreshold = true;
  TEST_SET_GET_VALUE(passLowFreqThreshold, bandPassFilter->GetPassLowFrequencyThreshold());

  bool passHighFreqThreshold = true;
  TEST_SET_GET_VALUE(passHighFreqThreshold, bandPassFilter->GetPassHighFrequencyThreshold());

  bandPassFilter->SetPassBand(passLowFreqThreshold, passHighFreqThreshold);
  TEST_SET_GET_VALUE(passLowFreqThreshold, bandPassFilter->GetPassLowFrequencyThreshold());
  TEST_SET_GET_VALUE(passHighFreqThreshold, bandPassFilter->GetPassHighFrequencyThreshold());


  TRY_EXPECT_NO_EXCEPTION(bandPassFilter->Update());

#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(bandPassFilter->GetOutput(), "PassBand - default");
#endif

  // Stop-band
  BandPassFilterType::Pointer stopBandFilter = BandPassFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS(stopBandFilter, FrequencyBandPassImageFilter, ImageToImageFilter);

  stopBandFilter->SetInput(image);

  stopBandFilter->SetLowFrequencyThreshold(lowFreqThreshold);
  TEST_SET_GET_VALUE(lowFreqThreshold, stopBandFilter->GetLowFrequencyThreshold());

  stopBandFilter->SetHighFrequencyThreshold(highFreqThreshold);
  TEST_SET_GET_VALUE(highFreqThreshold, stopBandFilter->GetHighFrequencyThreshold());

  passLowFreqThreshold = false;
  passHighFreqThreshold = false;
  stopBandFilter->SetStopBand(passLowFreqThreshold, passHighFreqThreshold);
  TEST_SET_GET_VALUE(passLowFreqThreshold, stopBandFilter->GetPassLowFrequencyThreshold());
  TEST_SET_GET_VALUE(passHighFreqThreshold, stopBandFilter->GetPassHighFrequencyThreshold());


  TRY_EXPECT_NO_EXCEPTION(stopBandFilter->Update());

#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(stopBandFilter->GetOutput(), "StopBand");
#endif

  // Regression test
  // Sum of bandPass and stopBand images with these settings should be equal
  // to original image
  typedef itk::AddImageFilter<ImageType3D, ImageType3D> AddFilterType;
  AddFilterType::Pointer                                addFilter = AddFilterType::New();
  addFilter->SetInput1(bandPassFilter->GetOutput());
  addFilter->SetInput2(stopBandFilter->GetOutput());

  typedef itk::Testing::ComparisonImageFilter<ImageType3D, ImageType3D> DifferenceFilterType;
  DifferenceFilterType::Pointer                                         differenceFilter = DifferenceFilterType::New();

  differenceFilter->SetToleranceRadius(0);
  differenceFilter->SetDifferenceThreshold(0);
  differenceFilter->SetValidInput(image);
  differenceFilter->SetTestInput(addFilter->GetOutput());

  differenceFilter->Update();

  unsigned int numberOfDiffPixels = differenceFilter->GetNumberOfPixelsWithDifferences();
  if (numberOfDiffPixels > 0)
  {
    std::cerr << "Test failed! " << std::endl;
    std::cerr << "Expected images to be equal, but got " << numberOfDiffPixels << "unequal pixels" << std::endl;
    return EXIT_FAILURE;
  }


  //
  // Tests with radians
  //
  lowFreqThreshold = itk::Math::pi_over_4;
  bandPassFilter->SetLowFrequencyThresholdInRadians(lowFreqThreshold);

  highFreqThreshold = itk::Math::pi_over_2;
  bandPassFilter->SetHighFrequencyThresholdInRadians(highFreqThreshold);

  passLowFreqThreshold = true;
  passHighFreqThreshold = true;
  bandPassFilter->SetPassBand(passLowFreqThreshold, passHighFreqThreshold);
  TEST_SET_GET_VALUE(passLowFreqThreshold, bandPassFilter->GetPassLowFrequencyThreshold());
  TEST_SET_GET_VALUE(passHighFreqThreshold, bandPassFilter->GetPassHighFrequencyThreshold());


  TRY_EXPECT_NO_EXCEPTION(bandPassFilter->Update());

#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(bandPassFilter->GetOutput(), "PassBand - radians");
#endif


  // typedef itk::ImageFileWriter< ImageType3D > WriterType;
  // WriterType::Pointer writer = WriterType::New();
  // writer->SetInput( bandPassFilter->GetOutput() );
  // writer->SetFileName( argv[1] );
  //
  // TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}

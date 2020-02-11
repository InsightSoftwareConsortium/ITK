/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#include "itkFrequencyBandImageFilter.h"
#include "itkUnaryFrequencyDomainFilter.h"

#include "itkAddImageFilter.h"
#include "itkFrequencyShiftedFFTLayoutImageRegionIteratorWithIndex.h"
#include "itkImage.h"
#include "itkImageFileWriter.h"
#include "itkTestingComparisonImageFilter.h"
#include "itkTestingMacros.h"

namespace
{
template <typename ImageType>
bool
compareImages(ImageType * baseline, ImageType * test)
{
  using DifferenceFilterType = itk::Testing::ComparisonImageFilter<ImageType, ImageType>;
  auto differenceFilter = DifferenceFilterType::New();

  differenceFilter->SetToleranceRadius(0);
  differenceFilter->SetDifferenceThreshold(0);
  differenceFilter->SetValidInput(baseline);
  differenceFilter->SetTestInput(test);

  differenceFilter->Update();

  unsigned int numberOfDiffPixels = differenceFilter->GetNumberOfPixelsWithDifferences();
  if (numberOfDiffPixels > 0)
  {
    std::cerr << "Expected images to be equal, but got " << numberOfDiffPixels << "unequal pixels" << std::endl;
    return false;
  }

  return true;
}

template <typename ImageType>
typename ImageType::Pointer
createImage(typename ImageType::SizeType size)
{
  auto                           image = ImageType::New();
  typename ImageType::RegionType region;
  region.SetSize(size);
  image->SetRegions(region);
  image->Allocate(false);

  typename ImageType::PixelType       value = itk::NumericTraits<typename ImageType::PixelType>::Zero;
  itk::ImageRegionIterator<ImageType> iter(image, region);
  while (!iter.IsAtEnd())
  {
    iter.Set(++value);
    ++iter;
  }

  return image;
}
} // namespace

int
itkFrequencyBandImageFilterTest(int argc, char * argv[])
{
  constexpr unsigned int Dimension = 3;

  if (argc != 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " Even|Odd" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string evenOrOddInput = argv[1];
  bool              isOdd = false;
  if (evenOrOddInput == "Even")
  {
    isOdd = false;
  }
  else if (evenOrOddInput == "Odd")
  {
    isOdd = true;
  }
  else
  {
    std::cerr << "Unkown string: " + evenOrOddInput + " . Use Even or Odd." << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = float;
  using ImageType3D = itk::Image<PixelType, Dimension>;

  ImageType3D::SizeType size = { { 10, 20, 40 } };
  if (isOdd)
  {
    for (unsigned int i = 0; i < Dimension; ++i)
    {
      size[i]++;
    }
  }

  auto image = createImage<ImageType3D>(size);


  using BandFilterType = itk::FrequencyBandImageFilter<ImageType3D>;
  auto passBandFilter = BandFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(passBandFilter, FrequencyBandImageFilter, UnaryFrequencyDomainFilter);

  passBandFilter->SetInput(image);

  // Test exception cases
  BandFilterType::FrequencyValueType lowFreqThreshold = 0.5;
  passBandFilter->SetLowFrequencyThreshold(lowFreqThreshold);
  ITK_TEST_SET_GET_VALUE(lowFreqThreshold, passBandFilter->GetLowFrequencyThreshold());

  BandFilterType::FrequencyValueType highFreqThreshold = 0.0;
  passBandFilter->SetHighFrequencyThreshold(highFreqThreshold);
  ITK_TEST_SET_GET_VALUE(highFreqThreshold, passBandFilter->GetHighFrequencyThreshold());


  ITK_TRY_EXPECT_EXCEPTION(passBandFilter->Update());


  lowFreqThreshold = 0.0;
  passBandFilter->SetLowFrequencyThreshold(lowFreqThreshold);
  ITK_TEST_SET_GET_VALUE(lowFreqThreshold, passBandFilter->GetLowFrequencyThreshold());

  highFreqThreshold = 0.5;
  passBandFilter->SetHighFrequencyThreshold(highFreqThreshold);
  ITK_TEST_SET_GET_VALUE(highFreqThreshold, passBandFilter->GetHighFrequencyThreshold());

  bool passBand = true;
  ITK_TEST_SET_GET_BOOLEAN(passBandFilter, PassBand, passBand);

  bool passLowFreqThreshold = true;
  ITK_TEST_SET_GET_BOOLEAN(passBandFilter, PassLowFrequencyThreshold, passLowFreqThreshold);

  bool passHighFreqThreshold = true;
  ITK_TEST_SET_GET_BOOLEAN(passBandFilter, PassHighFrequencyThreshold, passHighFreqThreshold);

  passBandFilter->SetPassBand(passLowFreqThreshold, passHighFreqThreshold);
  ITK_TEST_SET_GET_VALUE(passLowFreqThreshold, passBandFilter->GetPassLowFrequencyThreshold());
  ITK_TEST_SET_GET_VALUE(passHighFreqThreshold, passBandFilter->GetPassHighFrequencyThreshold());


  ITK_TRY_EXPECT_NO_EXCEPTION(passBandFilter->Update());

  // Stop-band
  auto stopBandFilter = BandFilterType::New();

  stopBandFilter->SetInput(image);

  stopBandFilter->SetLowFrequencyThreshold(lowFreqThreshold);
  stopBandFilter->SetHighFrequencyThreshold(highFreqThreshold);

  passLowFreqThreshold = false;
  passHighFreqThreshold = false;
  stopBandFilter->SetStopBand(passLowFreqThreshold, passHighFreqThreshold);
  ITK_TEST_SET_GET_VALUE(passLowFreqThreshold, stopBandFilter->GetPassLowFrequencyThreshold());
  ITK_TEST_SET_GET_VALUE(passHighFreqThreshold, stopBandFilter->GetPassHighFrequencyThreshold());

  ITK_TRY_EXPECT_NO_EXCEPTION(stopBandFilter->Update());

  // Regression test
  // Sum of bandPass and stopBand images with these settings should be equal
  // to original image
  using AddFilterType = itk::AddImageFilter<ImageType3D, ImageType3D>;
  auto addFilter = AddFilterType::New();
  addFilter->SetInput1(passBandFilter->GetOutput());
  addFilter->SetInput2(stopBandFilter->GetOutput());

  std::cout << "Comparing the original and sum of bandPass and stopBand images" << std::endl;
  bool success = compareImages(image.GetPointer(), addFilter->GetOutput());

  // Tests with radians

  BandFilterType::FrequencyValueType lowFreqThresholdRadians = itk::Math::pi_over_4;
  passBandFilter->SetLowFrequencyThresholdInRadians(lowFreqThresholdRadians);

  BandFilterType::FrequencyValueType highFreqThresholdRadians = itk::Math::pi_over_2;
  passBandFilter->SetHighFrequencyThresholdInRadians(highFreqThresholdRadians);

  BandFilterType::FrequencyValueType knownLowFrequencyHertz = lowFreqThresholdRadians / (2 * itk::Math::pi);
  BandFilterType::FrequencyValueType knownHighFrequencyHertz = highFreqThresholdRadians / (2 * itk::Math::pi);

  if (itk::Math::NotAlmostEquals(knownLowFrequencyHertz, passBandFilter->GetLowFrequencyThreshold()) ||
      itk::Math::NotAlmostEquals(knownHighFrequencyHertz, passBandFilter->GetHighFrequencyThreshold()))
  {
    std::cerr << "Test failed! " << std::endl;
    std::cerr << "Setting frequency in radians failed." << std::endl;
    success = false;
  }

  ITK_TRY_EXPECT_NO_EXCEPTION(passBandFilter->Update());

  // Test the non-radial cut-off.
  // Don't pass negative frequency thresholds.

  bool radialBand = false;
  ITK_TEST_SET_GET_BOOLEAN(passBandFilter, RadialBand, radialBand);
  bool passNegativeLowFrequencyThreshold = false;
  ITK_TEST_SET_GET_BOOLEAN(passBandFilter, PassNegativeLowFrequencyThreshold, passNegativeLowFrequencyThreshold);
  bool passNegativeHighFrequencyThreshold = false;
  ITK_TEST_SET_GET_BOOLEAN(passBandFilter, PassNegativeHighFrequencyThreshold, passNegativeHighFrequencyThreshold);
  passBandFilter->Update();

  // Test with ShiftedIterator.
  using FrequencyShiftedIterator = itk::FrequencyShiftedFFTLayoutImageRegionIteratorWithIndex<ImageType3D>;
  using BandShiftedFilterType = itk::FrequencyBandImageFilter<ImageType3D, FrequencyShiftedIterator>;
  auto passBandShiftedFilter = BandShiftedFilterType::New();

  passBandShiftedFilter->SetInput(image);
  passBandShiftedFilter->SetLowFrequencyThreshold(lowFreqThreshold);
  passBandShiftedFilter->SetHighFrequencyThreshold(highFreqThreshold);
  passBandShiftedFilter->SetPassBand(true);
  passLowFreqThreshold = false;
  passHighFreqThreshold = true;
  passBandShiftedFilter->SetPassBand(passLowFreqThreshold, passHighFreqThreshold);
  ITK_TRY_EXPECT_NO_EXCEPTION(passBandShiftedFilter->Update());

  // Stop-band with InPlaceOn
  auto stopBandInPlaceFilter = BandFilterType::New();
  stopBandInPlaceFilter->InPlaceOn();

  stopBandInPlaceFilter->SetInput(image);

  stopBandInPlaceFilter->SetLowFrequencyThreshold(lowFreqThreshold);
  stopBandInPlaceFilter->SetHighFrequencyThreshold(highFreqThreshold);

  passLowFreqThreshold = false;
  passHighFreqThreshold = false;
  stopBandInPlaceFilter->SetStopBand(passLowFreqThreshold, passHighFreqThreshold);

  ITK_TRY_EXPECT_NO_EXCEPTION(stopBandInPlaceFilter->Update());
  std::cout << "Comparing stopBand and stopBandInPlaceFilter results" << std::endl;
  success &= compareImages(stopBandFilter->GetOutput(), stopBandInPlaceFilter->GetOutput());


  // Custom functor UnaryFrequencyDomainFilter
  using UnaryFilterType = itk::UnaryFrequencyDomainFilter<ImageType3D>;
  using ValueFunctionType = UnaryFilterType::ValueFunctionType;
  using ConstRefFunctionType = UnaryFilterType::ConstRefFunctionType;

  auto testFilter = UnaryFilterType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(testFilter, UnaryFrequencyDomainFilter, InPlaceImageFilter);

  struct TestStruct
  {
    static double
    ConstUnaryFunction(const UnaryFilterType::FrequencyIteratorType & freqIt)
    {
      return std::sin(freqIt.GetFrequencyModuloSquare() * 10);
    }

    static void
    UnaryFunction(UnaryFilterType::FrequencyIteratorType & freqIt)
    {
      freqIt.Value() *= std::sin(freqIt.GetFrequencyModuloSquare() * 10);
    }
  };

  auto inPlaceLambda = [](UnaryFilterType::FrequencyIteratorType & freqIt) {
    freqIt.Value() *= std::sin(freqIt.GetFrequencyModuloSquare() * 10);
  };

  image = createImage<ImageType3D>(size); // stopBandInPlaceFilter has modified the original one


  // Test with lambda function
  auto lambdaFilter = UnaryFilterType::New();
  lambdaFilter->SetInput(image);
  lambdaFilter->SetFunctor(inPlaceLambda);
  ITK_TRY_EXPECT_NO_EXCEPTION(lambdaFilter->Update());

  // Test with C style function pointer
  auto cfpFilter = UnaryFilterType::New();
  cfpFilter->SetInput(image);
  cfpFilter->SetFunctor(static_cast<ConstRefFunctionType *>(TestStruct::ConstUnaryFunction));
  ITK_TRY_EXPECT_NO_EXCEPTION(cfpFilter->Update());

  // Test with std::functional
  auto stdFilter = UnaryFilterType::New();
  stdFilter->SetInput(image);
  std::function<double(const UnaryFilterType::FrequencyIteratorType &)> func1 =
    static_cast<ConstRefFunctionType *>(TestStruct::ConstUnaryFunction);
  stdFilter->SetFunctor(func1);
  ITK_TRY_EXPECT_NO_EXCEPTION(stdFilter->Update());

  // Test with C style function pointer (in-place variant)
  auto cfpFilterIP = UnaryFilterType::New();
  cfpFilterIP->SetInput(image);
  cfpFilterIP->SetFunctor(static_cast<ValueFunctionType *>(TestStruct::UnaryFunction));
  ITK_TRY_EXPECT_NO_EXCEPTION(cfpFilterIP->Update());

  // Test with std::functional (in-place variant)
  auto stdFilterIP = UnaryFilterType::New();
  stdFilterIP->SetInput(image);
  std::function<void(UnaryFilterType::FrequencyIteratorType &)> func2 =
    static_cast<ValueFunctionType *>(TestStruct::UnaryFunction);
  stdFilterIP->SetFunctor(func2);
  ITK_TRY_EXPECT_NO_EXCEPTION(stdFilterIP->Update());

  std::cout << "Comparing lambdaFilter and cfpFilter (const functor) results" << std::endl;
  success &= compareImages(lambdaFilter->GetOutput(), cfpFilter->GetOutput());

  std::cout << "Comparing lambdaFilter and stdFilter (const functor) results" << std::endl;
  success &= compareImages(lambdaFilter->GetOutput(), stdFilter->GetOutput());

  std::cout << "Comparing lambdaFilter and cfpFilter (in-place functor) results" << std::endl;
  success &= compareImages(lambdaFilter->GetOutput(), cfpFilterIP->GetOutput());

  std::cout << "Comparing lambdaFilter and stdFilter (in-place functor) results" << std::endl;
  success &= compareImages(lambdaFilter->GetOutput(), stdFilterIP->GetOutput());

  if (success)
  {
    std::cout << "Test PASSED!" << std::endl;
    return EXIT_SUCCESS;
  }
  else
  {
    std::cout << "Test FAILED!" << std::endl;
    return EXIT_FAILURE;
  }
}

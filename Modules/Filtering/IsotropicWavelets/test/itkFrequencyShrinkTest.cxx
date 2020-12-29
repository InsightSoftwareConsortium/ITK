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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include "itkComplexToRealImageFilter.h"
#include "itkComplexToImaginaryImageFilter.h"
#include "itkFrequencyShrinkImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkZeroDCImageFilter.h"
#include "itkComplexToComplexFFTImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include <itkCastImageFilter.h>
#include "itkIsotropicWaveletTestUtilities.h"
#include "itkAddImageFilter.h"
#include <itkChangeInformationImageFilter.h>
#include "itkTestingComparisonImageFilter.h"
#include "itkTestingMacros.h"

#include <memory>
#include <string>
#include <cmath>
#include <iomanip>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

template <unsigned int VDimension>
int
runFrequencyShrinkTest(const std::string & inputImage, const std::string & outputImage)
{
  bool               testPassed = true;
  const unsigned int Dimension = VDimension;

  using PixelType = double;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();

  // Calculate mean value and subtract
  using ZeroDCFilterType = itk::ZeroDCImageFilter<ImageType>;
  auto zeroDCFilter = ZeroDCFilterType::New();
  zeroDCFilter->SetInput(reader->GetOutput());

  // Perform FFT on input image.
  using FFTFilterType = itk::ForwardFFTImageFilter<ImageType>;
  auto fftFilter = FFTFilterType::New();
  fftFilter->SetInput(zeroDCFilter->GetOutput());
  using ComplexImageType = typename FFTFilterType::OutputImageType;

  // ShrinkFrequency
  using ShrinkType = itk::FrequencyShrinkImageFilter<ComplexImageType>;
  auto shrinkFilter = ShrinkType::New();

  unsigned int                           shrinkFactor = 2;
  typename ShrinkType::ShrinkFactorsType shrinkFactors;
  shrinkFactors.Fill(shrinkFactor);
  for (unsigned int i = 0; i < shrinkFactors.Size(); ++i)
  {
    shrinkFilter->SetShrinkFactor(i, shrinkFactors[i]);
  }
  ITK_TEST_SET_GET_VALUE(shrinkFactors, shrinkFilter->GetShrinkFactors());

  shrinkFactor = 2;
  shrinkFactors.Fill(shrinkFactor);
  shrinkFilter->SetShrinkFactors(shrinkFactors);
  ITK_TEST_SET_GET_VALUE(shrinkFactors, shrinkFilter->GetShrinkFactors());

  shrinkFilter->SetInput(fftFilter->GetOutput());

  shrinkFilter->Update();

  // InverseFFT
  using InverseFFTFilterType = itk::InverseFFTImageFilter<ComplexImageType, ImageType>;
  auto inverseFFT = InverseFFTFilterType::New();
  inverseFFT->SetInput(shrinkFilter->GetOutput());

  inverseFFT->Update();

  // image is even?
  itk::FixedArray<bool, Dimension> inputSizeIsEven;
  bool                             imageIsEven = true;
  for (unsigned int dim = 0; dim < Dimension; ++dim)
  {
    inputSizeIsEven[dim] = (zeroDCFilter->GetOutput()->GetLargestPossibleRegion().GetSize()[dim] % 2 == 0);
    if (inputSizeIsEven[dim] == false)
    {
      imageIsEven = false;
    }
  }
  std::cout << "Image Even? " << imageIsEven << std::endl;

  // Test Hermitian properties for even Images. Odd real images are not even hermitian after FFTw.
  if (imageIsEven)
  {
    bool fftIsHermitian = itk::Testing::ComplexImageIsHermitian(fftFilter->GetOutput());
    bool shrinkIsHermitian = itk::Testing::ComplexImageIsHermitian(shrinkFilter->GetOutput());
    if (!fftIsHermitian)
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "fft is not Hermitian" << std::endl;
      testPassed = false;
    }
    if (!shrinkIsHermitian)
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "shrink is not Hermitian" << std::endl;
      testPassed = false;
    }

    // Hermitian Test:
    // Simmetry and Hermitian test: ComplexInverseFFT will generate output with zero imaginary part.
    // Check that complex part is almost 0 after FFT and complex inverse FFT.
    {
      using ComplexFFTType = itk::ComplexToComplexFFTImageFilter<ComplexImageType>;
      auto complexInverseFFT = ComplexFFTType::New();
      complexInverseFFT->SetTransformDirection(ComplexFFTType::TransformDirectionEnum::INVERSE);
      complexInverseFFT->SetInput(fftFilter->GetOutput());

      complexInverseFFT->Update();

      itk::ImageRegionConstIterator<ComplexImageType> complexInverseIt(
        complexInverseFFT->GetOutput(), complexInverseFFT->GetOutput()->GetLargestPossibleRegion());
      complexInverseIt.GoToBegin();
      unsigned int notZeroComplexError = 0;
      double       accumSqDiff = 0;
      while (!complexInverseIt.IsAtEnd())
      {
        typename ComplexImageType::PixelType::value_type imageValue = complexInverseIt.Get().imag();
        if (itk::Math::NotAlmostEquals<typename ComplexImageType::PixelType::value_type>(imageValue, 0.0))
        {
          ++notZeroComplexError;
          accumSqDiff += imageValue * imageValue;
        }

        ++complexInverseIt;
      }

      // accumSqDiff /= notZeroComplexError;
      if (notZeroComplexError > 0)
      {
        std::cout << "Dev note: After the FFT filter the image is not "
                     "Hermitian. #Not_zero_imag_value Pixels: "
                  << notZeroComplexError << " accumSquareDifference: " << accumSqDiff << std::endl;
      }
    }
    // Check that complex part is almost 0 filter is correct after shrink
    {
      using ComplexFFTType = itk::ComplexToComplexFFTImageFilter<ComplexImageType>;
      auto complexInverseFFT = ComplexFFTType::New();
      complexInverseFFT->SetTransformDirection(ComplexFFTType::TransformDirectionEnum::INVERSE);
      complexInverseFFT->SetInput(shrinkFilter->GetOutput());

      complexInverseFFT->Update();

      itk::ImageRegionConstIterator<ComplexImageType> complexInverseIt(
        complexInverseFFT->GetOutput(), complexInverseFFT->GetOutput()->GetLargestPossibleRegion());
      complexInverseIt.GoToBegin();
      unsigned int notZeroComplexError = 0;
      double       accumSqDiff = 0;
      while (!complexInverseIt.IsAtEnd())
      {
        typename ComplexImageType::PixelType::value_type imageValue = complexInverseIt.Get().imag();
        if (itk::Math::NotAlmostEquals<typename ComplexImageType::PixelType::value_type>(imageValue, 0.0))
        {
          ++notZeroComplexError;
          accumSqDiff += imageValue * imageValue;
        }
        ++complexInverseIt;
      }

      // accumSqDiff /= notZeroComplexError;
      if (notZeroComplexError > 0)
      {
        std::cout << "Dev note: After the SHRINK filter the image is not "
                     "Hermitian. #Not_zero_imag_value pixels: "
                  << notZeroComplexError << " accumSquareDifference: " << accumSqDiff << std::endl;
      }
    }
  }

  // Test size and metadata
  typename ComplexImageType::PointType   fftOrigin = fftFilter->GetOutput()->GetOrigin();
  typename ComplexImageType::SpacingType fftSpacing = fftFilter->GetOutput()->GetSpacing();
  typename ComplexImageType::PointType   shrinkOrigin = shrinkFilter->GetOutput()->GetOrigin();
  typename ComplexImageType::SpacingType shrinkSpacing = shrinkFilter->GetOutput()->GetSpacing();

  std::cout << "ShrinkOrigin = " << shrinkOrigin << std::endl;
  if (shrinkOrigin != fftOrigin)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in Origin (has changed afterShrink): " << std::endl;
    std::cerr << "Expected: " << fftOrigin << ", but got " << shrinkOrigin << std::endl;
    testPassed = false;
  }

  std::cout << "ShrinkSpacing = " << shrinkSpacing << std::endl;
  if (shrinkSpacing != fftSpacing * 2.0)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in Spacing (should be double after shrink): " << std::endl;
    std::cerr << "Expected: " << fftSpacing * 2.0 << ", but got " << shrinkSpacing << std::endl;
    testPassed = false;
  }

  //
  // Test with frequency band filter.
  //

  using ChangeInformationFilterType = itk::ChangeInformationImageFilter<ComplexImageType>;
  auto                                 changeInputInfoFilter = ChangeInformationFilterType::New();
  typename ComplexImageType::PointType origin_new;
  origin_new.Fill(0);
  typename ComplexImageType::SpacingType spacing_new;
  spacing_new.Fill(1);
  typename ComplexImageType::DirectionType direction_new;
  direction_new.SetIdentity();
  changeInputInfoFilter->SetInput(fftFilter->GetOutput());
  changeInputInfoFilter->ChangeDirectionOn();
  changeInputInfoFilter->ChangeRegionOff();
  changeInputInfoFilter->ChangeSpacingOn();
  changeInputInfoFilter->ChangeOriginOn();
  changeInputInfoFilter->UseReferenceImageOff();
  changeInputInfoFilter->SetOutputOrigin(origin_new);
  changeInputInfoFilter->SetOutputSpacing(spacing_new);
  changeInputInfoFilter->SetOutputDirection(direction_new);
  changeInputInfoFilter->Update();

  auto shrinkBandFilter = ShrinkType::New();
  bool applyBandFilter = true;
  ITK_TEST_SET_GET_BOOLEAN(shrinkBandFilter, ApplyBandFilter, applyBandFilter);
  shrinkBandFilter->SetInput(changeInputInfoFilter->GetOutput());
  bool lowFreqThresholdPassing = true;
  bool highFreqThresholdPassing = true;
  shrinkBandFilter->GetFrequencyBandFilter()->SetPassBand(lowFreqThresholdPassing, highFreqThresholdPassing);

  auto shrinkNoIntersectionFilter = ShrinkType::New();
  ITK_TEST_SET_GET_BOOLEAN(shrinkNoIntersectionFilter, ApplyBandFilter, applyBandFilter);
  shrinkNoIntersectionFilter->SetInput(changeInputInfoFilter->GetOutput());
  lowFreqThresholdPassing = true;
  highFreqThresholdPassing = false;
  shrinkNoIntersectionFilter->GetFrequencyBandFilter()->SetPassBand(lowFreqThresholdPassing, highFreqThresholdPassing);

  auto shrinkIntersectionPassFilter = ShrinkType::New();
  ITK_TEST_SET_GET_BOOLEAN(shrinkIntersectionPassFilter, ApplyBandFilter, applyBandFilter);
  shrinkIntersectionPassFilter->SetInput(changeInputInfoFilter->GetOutput());
  lowFreqThresholdPassing = true;
  highFreqThresholdPassing = true;
  shrinkIntersectionPassFilter->GetFrequencyBandFilter()->SetFrequencyThresholdsInRadians(itk::Math::pi_over_2,
                                                                                          itk::Math::pi_over_2);
  shrinkIntersectionPassFilter->GetFrequencyBandFilter()->SetPassBand(lowFreqThresholdPassing,
                                                                      highFreqThresholdPassing);

  using AddFilterType = itk::AddImageFilter<ComplexImageType, ComplexImageType>;
  auto addFilter = AddFilterType::New();
  addFilter->SetInput1(shrinkNoIntersectionFilter->GetOutput());
  addFilter->SetInput2(shrinkIntersectionPassFilter->GetOutput());
  auto inverseFFTAdd = InverseFFTFilterType::New();
  inverseFFTAdd->SetInput(addFilter->GetOutput());

  auto inverseFFTShrinkBand = InverseFFTFilterType::New();
  inverseFFTShrinkBand->SetInput(shrinkBandFilter->GetOutput());

  using DifferenceFilterType = itk::Testing::ComparisonImageFilter<ImageType, ImageType>;
  auto differenceFilter = DifferenceFilterType::New();
  differenceFilter->SetToleranceRadius(0);
  differenceFilter->SetDifferenceThreshold(0);
  differenceFilter->SetValidInput(inverseFFTShrinkBand->GetOutput());
  differenceFilter->SetTestInput(inverseFFTAdd->GetOutput());
  differenceFilter->Update();

  unsigned int numberOfDiffPixels = differenceFilter->GetNumberOfPixelsWithDifferences();
  if (numberOfDiffPixels > 0)
  {
    std::cerr << "Test failed! " << std::endl;
    std::cerr << "Expected images to be equal, but got " << numberOfDiffPixels << " unequal pixels" << std::endl;
    testPassed = false;
  }

  // Write output
  using FloatImageType = itk::Image<float, Dimension>;
  using CastType = itk::CastImageFilter<ImageType, FloatImageType>;
  auto castFilter = CastType::New();
  castFilter->SetInput(inverseFFT->GetOutput());

  using WriterType = itk::ImageFileWriter<FloatImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(outputImage);
  writer->SetInput(castFilter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

#ifdef ITK_VISUALIZE_TESTS
  itk::ViewImage<ImageType>::View(zeroDCFilter->GetOutput(), "Original");
  itk::ViewImage<ImageType>::View(inverseFFT->GetOutput(), "FrequencyShrinker");
  // Compare with regular shrink filter.
  using RegularShrinkType = itk::ShrinkImageFilter<ImageType, ImageType>;
  auto regularShrinkFilter = RegularShrinkType::New();
  regularShrinkFilter->SetInput(zeroDCFilter->GetOutput());
  regularShrinkFilter->SetShrinkFactors(2);
  regularShrinkFilter->Update();
  itk::ViewImage<ImageType>::View(regularShrinkFilter->GetOutput(), "Regular shrinker");

  // Complex to real
  using ComplexToRealFilter = itk::ComplexToRealImageFilter<ComplexImageType, ImageType>;
  auto complexToRealFilter = ComplexToRealFilter::New();
  complexToRealFilter->SetInput(fftFilter->GetOutput());
  complexToRealFilter->Update();
  itk::ViewImage<ImageType>::View(complexToRealFilter->GetOutput(), "ComplexToReal. Original");
  auto complexToRealFilterShrink = ComplexToRealFilter::New();
  complexToRealFilterShrink->SetInput(shrinkFilter->GetOutput());
  complexToRealFilterShrink->Update();
  itk::ViewImage<ImageType>::View(complexToRealFilterShrink->GetOutput(), "ComplexToReal. Shrinked");

  // Complex to imaginary
  // using ComplexToImaginaryFilter = itk::ComplexToImaginaryImageFilter< ComplexImageType, ImageType >;
  // auto complexToImaginaryFilter = ComplexToImaginaryFilter::New();
  // complexToImaginaryFilter->SetInput( fftFilter->GetOutput() );
  // complexToImaginaryFilter->Update();
  // itk::ViewImage<ImageType>::View( complexToImaginaryFilter->GetOutput(), "ComplexToImaginary. Original" );
  // auto complexToImaginaryFilterShrink = ComplexToImaginaryFilter::New();
  // complexToImaginaryFilterShrink->SetInput( shrinkFilter->GetOutput() );
  // complexToImaginaryFilterShrink->Update();
  // itk::ViewImage<ImageType>::View( complexToImaginaryFilterShrink->GetOutput(), "ComplexToImaginary. Shrinked" );

#endif

  if (testPassed)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }
}

int
itkFrequencyShrinkTest(int argc, char * argv[])
{
  if (argc < 3 || argc > 4)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage [dimension]" << std::endl;
    return EXIT_FAILURE;
  }

  const std::string inputImage = argv[1];
  const std::string outputImage = argv[2];

  constexpr unsigned int ImageDimension = 3;
  using PixelType = double;
  using ImageType = itk::Image<PixelType, ImageDimension>;
  using FFTFilterType = itk::ForwardFFTImageFilter<ImageType>;
  using ComplexImageType = FFTFilterType::OutputImageType;

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  using ShrinkType = itk::FrequencyShrinkImageFilter<ComplexImageType>;
  auto shrinkFilter = ShrinkType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(shrinkFilter, FrequencyShrinkImageFilter, ImageToImageFilter);

  unsigned int dimension = 3;
  if (argc == 4)
  {
    dimension = std::stoi(argv[3]);
  }

  if (dimension == 2)
  {
    return runFrequencyShrinkTest<2>(inputImage, outputImage);
  }
  else if (dimension == 3)
  {
    return runFrequencyShrinkTest<3>(inputImage, outputImage);
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}

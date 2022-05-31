/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkFrequencyExpandImageFilter.h"
#include "itkFrequencyShrinkViaInverseFFTImageFilter.h"
#include "itkFrequencyExpandViaInverseFFTImageFilter.h"
#include "itkZeroDCImageFilter.h"
#include "itkComplexToComplexFFTImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkCastImageFilter.h"
#include "itkNumberToString.h"
#include "itkIsotropicWaveletTestUtilities.h"
#include "itkTestingMacros.h"
#include "itkTestingComparisonImageFilter.h"

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
runFrequencyExpandAndShrinkTest(const std::string & inputImage, const std::string & outputImage)
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
  zeroDCFilter->Update();

  // Perform FFT on input image.
  using FFTFilterType = itk::ForwardFFTImageFilter<ImageType>;
  auto fftFilter = FFTFilterType::New();
  fftFilter->SetInput(zeroDCFilter->GetOutput());
  fftFilter->Update();

  using ComplexImageType = typename FFTFilterType::OutputImageType;
  using InverseFFTFilterType = itk::InverseFFTImageFilter<ComplexImageType, ImageType>;
  size_t resizeFactor = 2;

  /*********** EXPAND ***************/
  using ExpandType = itk::FrequencyExpandImageFilter<ComplexImageType>;
  auto expandFilter = ExpandType::New();
  expandFilter->SetInput(fftFilter->GetOutput());
  expandFilter->SetExpandFactors(resizeFactor);
  expandFilter->Update();

  using ExpandViaInverseFFTType = itk::FrequencyExpandViaInverseFFTImageFilter<ComplexImageType>;
  auto expandViaInverseFFTFilter = ExpandViaInverseFFTType::New();
  expandViaInverseFFTFilter->SetInput(fftFilter->GetOutput());

  typename ExpandViaInverseFFTType::ExpandFactorsType expandFactors;
  expandFactors.Fill(resizeFactor);
  expandViaInverseFFTFilter->SetExpandFactors(resizeFactor);
  ITK_TEST_SET_GET_VALUE(expandFactors, expandViaInverseFFTFilter->GetExpandFactors());

  expandViaInverseFFTFilter->SetExpandFactors(expandFactors);
  ITK_TEST_SET_GET_VALUE(expandFactors, expandViaInverseFFTFilter->GetExpandFactors());

  ITK_TRY_EXPECT_NO_EXCEPTION(expandViaInverseFFTFilter->Update());

  // #ifdef ITK_VISUALIZE_TESTS
  //   auto inverseFFTExpand1 = InverseFFTFilterType::New();
  //   inverseFFTExpand1->SetInput(expandFilter->GetOutput());
  //   inverseFFTExpand1->Update();
  //   auto inverseFFTExpand2 = InverseFFTFilterType::New();
  //   inverseFFTExpand2->SetInput(expandViaInverseFFTFilter->GetOutput());
  //   inverseFFTExpand2->Update();
  //   itk::ViewImage<ImageType>::View(inverseFFTExpand1->GetOutput(), "Expand via frequency manipulation");
  //   itk::ViewImage<ImageType>::View(inverseFFTExpand2->GetOutput(), "Expand ViaInverseFFT");
  // #endif

  /*********** SHRINK ***************/
  using ShrinkType = itk::FrequencyShrinkImageFilter<ComplexImageType>;
  auto shrinkFilter = ShrinkType::New();
  shrinkFilter->SetInput(expandFilter->GetOutput());
  shrinkFilter->SetShrinkFactors(resizeFactor);

  ITK_TRY_EXPECT_NO_EXCEPTION(shrinkFilter->Update());

  using ShrinkViaInverseFFTType = itk::FrequencyShrinkViaInverseFFTImageFilter<ComplexImageType>;
  auto shrinkViaInverseFFTFilter = ShrinkViaInverseFFTType::New();
  shrinkViaInverseFFTFilter->SetInput(expandViaInverseFFTFilter->GetOutput());

  typename ShrinkViaInverseFFTType::ShrinkFactorsType shrinkFactors;
  shrinkFactors.Fill(resizeFactor);
  shrinkViaInverseFFTFilter->SetShrinkFactors(resizeFactor);
  ITK_TEST_SET_GET_VALUE(shrinkFactors, shrinkViaInverseFFTFilter->GetShrinkFactors());

  shrinkViaInverseFFTFilter->SetShrinkFactors(shrinkFactors);
  ITK_TEST_SET_GET_VALUE(shrinkFactors, shrinkViaInverseFFTFilter->GetShrinkFactors());

  ITK_TRY_EXPECT_NO_EXCEPTION(shrinkViaInverseFFTFilter->Update());


  // Test size and metadata
  typename ComplexImageType::PointType   fftOrigin = fftFilter->GetOutput()->GetOrigin();
  typename ComplexImageType::SpacingType fftSpacing = fftFilter->GetOutput()->GetSpacing();
  typename ComplexImageType::PointType   afterShrinkOrigin = shrinkFilter->GetOutput()->GetOrigin();
  typename ComplexImageType::SpacingType afterShrinkSpacing = shrinkFilter->GetOutput()->GetSpacing();

  if (afterShrinkOrigin != fftOrigin)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in Origin (has changed afterShrink): " << std::endl;
    std::cerr << "Expected: " << fftOrigin << ", but got " << afterShrinkOrigin << std::endl;
    testPassed = false;
  }

  if (afterShrinkSpacing != fftSpacing)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in Spacing : " << std::endl;
    std::cerr << "Expected: " << fftSpacing << ", but got " << afterShrinkSpacing << std::endl;
    testPassed = false;
  }

  /*********** InverseFFT ***************/

  auto inverseFFT1 = InverseFFTFilterType::New();
  inverseFFT1->SetInput(shrinkFilter->GetOutput());
  inverseFFT1->Update();
  auto inverseFFT2 = InverseFFTFilterType::New();
  inverseFFT2->SetInput(shrinkViaInverseFFTFilter->GetOutput());
  inverseFFT2->Update();

#ifdef ITK_VISUALIZE_TESTS
  itk::ViewImage<ImageType>::View(zeroDCFilter->GetOutput(), "Original");
  itk::ViewImage<ImageType>::View(inverseFFT1->GetOutput(), "ExpandAndShrink via frequency manipulation");
#endif

  // Comparison
  // Via direct frequency manipulation.
  using DifferenceFilterType = itk::Testing::ComparisonImageFilter<ImageType, ImageType>;
  auto differenceFilter = DifferenceFilterType::New();
  differenceFilter->SetToleranceRadius(0);
  differenceFilter->SetDifferenceThreshold(0.000001);
  differenceFilter->SetValidInput(zeroDCFilter->GetOutput());
  differenceFilter->SetTestInput(inverseFFT1->GetOutput());
  differenceFilter->Update();

  unsigned int numberOfDiffPixels = differenceFilter->GetNumberOfPixelsWithDifferences();
  if (numberOfDiffPixels > 0)
  {
    std::cerr << "Test failed! " << std::endl;
    std::cerr << "FrequencyExpand + FrequencyShrinker should be equal to input image, but got " << numberOfDiffPixels
              << " unequal pixels" << std::endl;
    testPassed = false;
  }

  // Via inverseFFT and spatial domain manipulation.
#ifdef ITK_VISUALIZE_TESTS
  itk::ViewImage<ImageType>::View(inverseFFT2->GetOutput(), "ExpandAndShrink ViaInverseFFT");
#endif
  differenceFilter->SetTestInput(inverseFFT2->GetOutput());
  differenceFilter->Update();
  numberOfDiffPixels = differenceFilter->GetNumberOfPixelsWithDifferences();
  if (numberOfDiffPixels > 0)
  {
    std::cerr << "Test failed! " << std::endl;
    std::cerr << "Expand + Shrinker via inverseFFT through spatial domain should be equal to input image, but got "
              << numberOfDiffPixels << " unequal pixels" << std::endl;
    testPassed = false;
  }


  // Write output
  using FloatImageType = itk::Image<float, Dimension>;
  using CastType = itk::CastImageFilter<ImageType, FloatImageType>;
  auto castFilter = CastType::New();
  castFilter->SetInput(inverseFFT2->GetOutput());

  using WriterType = itk::ImageFileWriter<FloatImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(outputImage);
  writer->SetInput(castFilter->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

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
itkFrequencyExpandAndShrinkTest(int argc, char * argv[])
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
  using ExpandViaInverseFFTType = itk::FrequencyExpandViaInverseFFTImageFilter<ComplexImageType>;
  auto expandViaInverseFFTFilter = ExpandViaInverseFFTType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    expandViaInverseFFTFilter, FrequencyExpandViaInverseFFTImageFilter, ImageToImageFilter);

  using ShrinkViaInverseFFTType = itk::FrequencyShrinkViaInverseFFTImageFilter<ComplexImageType>;
  auto shrinkViaInverseFFTFilter = ShrinkViaInverseFFTType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    shrinkViaInverseFFTFilter, FrequencyShrinkViaInverseFFTImageFilter, ImageToImageFilter);

  unsigned int dimension = 3;
  if (argc == 4)
  {
    dimension = std::stoi(argv[3]);
  }

  if (dimension == 2)
  {
    return runFrequencyExpandAndShrinkTest<2>(inputImage, outputImage);
  }
  else if (dimension == 3)
  {
    return runFrequencyExpandAndShrinkTest<3>(inputImage, outputImage);
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}

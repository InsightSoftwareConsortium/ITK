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

  typedef double                           PixelType;
  typedef itk::Image<PixelType, Dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;

  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();

  // Calculate mean value and subtract
  typedef itk::ZeroDCImageFilter<ImageType> ZeroDCFilterType;
  typename ZeroDCFilterType::Pointer        zeroDCFilter = ZeroDCFilterType::New();
  zeroDCFilter->SetInput(reader->GetOutput());
  zeroDCFilter->Update();

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  typename FFTFilterType::Pointer               fftFilter = FFTFilterType::New();
  fftFilter->SetInput(zeroDCFilter->GetOutput());
  fftFilter->Update();

  typedef typename FFTFilterType::OutputImageType                 ComplexImageType;
  typedef itk::InverseFFTImageFilter<ComplexImageType, ImageType> InverseFFTFilterType;
  size_t                                                          resizeFactor = 2;

  /*********** EXPAND ***************/
  typedef itk::FrequencyExpandImageFilter<ComplexImageType> ExpandType;
  typename ExpandType::Pointer                              expandFilter = ExpandType::New();
  expandFilter->SetInput(fftFilter->GetOutput());
  expandFilter->SetExpandFactors(resizeFactor);
  expandFilter->Update();

  typedef itk::FrequencyExpandViaInverseFFTImageFilter<ComplexImageType> ExpandViaInverseFFTType;
  typename ExpandViaInverseFFTType::Pointer expandViaInverseFFTFilter = ExpandViaInverseFFTType::New();
  expandViaInverseFFTFilter->SetInput(fftFilter->GetOutput());

  typename ExpandViaInverseFFTType::ExpandFactorsType expandFactors;
  expandFactors.Fill(resizeFactor);
  expandViaInverseFFTFilter->SetExpandFactors(resizeFactor);
  TEST_SET_GET_VALUE(expandFactors, expandViaInverseFFTFilter->GetExpandFactors());

  expandViaInverseFFTFilter->SetExpandFactors(expandFactors);
  TEST_SET_GET_VALUE(expandFactors, expandViaInverseFFTFilter->GetExpandFactors());

  TRY_EXPECT_NO_EXCEPTION(expandViaInverseFFTFilter->Update());

  // #ifdef ITK_VISUALIZE_TESTS
  //   typename InverseFFTFilterType::Pointer inverseFFTExpand1 = InverseFFTFilterType::New();
  //   inverseFFTExpand1->SetInput(expandFilter->GetOutput());
  //   inverseFFTExpand1->Update();
  //   typename InverseFFTFilterType::Pointer inverseFFTExpand2 = InverseFFTFilterType::New();
  //   inverseFFTExpand2->SetInput(expandViaInverseFFTFilter->GetOutput());
  //   inverseFFTExpand2->Update();
  //   itk::Testing::ViewImage(inverseFFTExpand1->GetOutput(), "Expand via frequency manipulation");
  //   itk::Testing::ViewImage(inverseFFTExpand2->GetOutput(), "Expand ViaInverseFFT");
  // #endif

  /*********** SHRINK ***************/
  typedef itk::FrequencyShrinkImageFilter<ComplexImageType> ShrinkType;
  typename ShrinkType::Pointer                              shrinkFilter = ShrinkType::New();
  shrinkFilter->SetInput(expandFilter->GetOutput());
  shrinkFilter->SetShrinkFactors(resizeFactor);

  TRY_EXPECT_NO_EXCEPTION(shrinkFilter->Update());

  typedef itk::FrequencyShrinkViaInverseFFTImageFilter<ComplexImageType> ShrinkViaInverseFFTType;
  typename ShrinkViaInverseFFTType::Pointer shrinkViaInverseFFTFilter = ShrinkViaInverseFFTType::New();
  shrinkViaInverseFFTFilter->SetInput(expandViaInverseFFTFilter->GetOutput());

  typename ShrinkViaInverseFFTType::ShrinkFactorsType shrinkFactors;
  shrinkFactors.Fill(resizeFactor);
  shrinkViaInverseFFTFilter->SetShrinkFactors(resizeFactor);
  TEST_SET_GET_VALUE(shrinkFactors, shrinkViaInverseFFTFilter->GetShrinkFactors());

  shrinkViaInverseFFTFilter->SetShrinkFactors(shrinkFactors);
  TEST_SET_GET_VALUE(shrinkFactors, shrinkViaInverseFFTFilter->GetShrinkFactors());

  TRY_EXPECT_NO_EXCEPTION(shrinkViaInverseFFTFilter->Update());


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

  typename InverseFFTFilterType::Pointer inverseFFT1 = InverseFFTFilterType::New();
  inverseFFT1->SetInput(shrinkFilter->GetOutput());
  inverseFFT1->Update();
  typename InverseFFTFilterType::Pointer inverseFFT2 = InverseFFTFilterType::New();
  inverseFFT2->SetInput(shrinkViaInverseFFTFilter->GetOutput());
  inverseFFT2->Update();

  // Comparison
  // Via direct frequency manipulation.
  typedef itk::Testing::ComparisonImageFilter<ImageType, ImageType> DifferenceFilterType;
  typename DifferenceFilterType::Pointer                            differenceFilter = DifferenceFilterType::New();
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

#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(zeroDCFilter->GetOutput(), "Original");
  itk::Testing::ViewImage(inverseFFT1->GetOutput(), "ExpandAndShrink via frequency manipulation");
  itk::Testing::ViewImage(inverseFFT2->GetOutput(), "ExpandAndShrink ViaInverseFFT");
#endif

  // Write output
  typedef itk::Image<float, Dimension>                    FloatImageType;
  typedef itk::CastImageFilter<ImageType, FloatImageType> CastType;
  typename CastType::Pointer                              castFilter = CastType::New();
  castFilter->SetInput(inverseFFT2->GetOutput());

  typedef itk::ImageFileWriter<FloatImageType> WriterType;
  typename WriterType::Pointer                 writer = WriterType::New();
  writer->SetFileName(outputImage);
  writer->SetInput(castFilter->GetOutput());

  TRY_EXPECT_NO_EXCEPTION(writer->Update());

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

  const unsigned int                            ImageDimension = 3;
  typedef double                                PixelType;
  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  typedef FFTFilterType::OutputImageType        ComplexImageType;

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  typedef itk::FrequencyExpandViaInverseFFTImageFilter<ComplexImageType> ExpandViaInverseFFTType;
  ExpandViaInverseFFTType::Pointer expandViaInverseFFTFilter = ExpandViaInverseFFTType::New();

  EXERCISE_BASIC_OBJECT_METHODS(expandViaInverseFFTFilter, FrequencyExpandViaInverseFFTImageFilter, ImageToImageFilter);

  typedef itk::FrequencyShrinkViaInverseFFTImageFilter<ComplexImageType> ShrinkViaInverseFFTType;
  ShrinkViaInverseFFTType::Pointer shrinkViaInverseFFTFilter = ShrinkViaInverseFFTType::New();

  EXERCISE_BASIC_OBJECT_METHODS(shrinkViaInverseFFTFilter, FrequencyShrinkViaInverseFFTImageFilter, ImageToImageFilter);

  unsigned int dimension = 3;
  if (argc == 4)
  {
    dimension = atoi(argv[3]);
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

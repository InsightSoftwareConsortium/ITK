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
#include "itkShrinkImageFilter.h"
#include "itkZeroDCImageFilter.h"
#include "itkComplexToComplexFFTImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include <itkCastImageFilter.h>
#include "itkIsotropicWaveletTestUtilities.h"
#include "itkAddImageFilter.h"
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

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  typename FFTFilterType::Pointer               fftFilter = FFTFilterType::New();
  fftFilter->SetInput(zeroDCFilter->GetOutput());
  typedef typename FFTFilterType::OutputImageType ComplexImageType;

  // ShinkFrequency
  typedef itk::FrequencyShrinkImageFilter<ComplexImageType> ShrinkType;
  typename ShrinkType::Pointer                              shrinkFilter = ShrinkType::New();

  unsigned int                           shrinkFactor = 2;
  typename ShrinkType::ShrinkFactorsType shrinkFactors;
  shrinkFactors.Fill(shrinkFactor);
  for (unsigned int i = 0; i < shrinkFactors.Size(); ++i)
  {
    shrinkFilter->SetShrinkFactor(i, shrinkFactors[i]);
  }
  TEST_SET_GET_VALUE(shrinkFactors, shrinkFilter->GetShrinkFactors());

  shrinkFactor = 2;
  shrinkFactors.Fill(shrinkFactor);
  shrinkFilter->SetShrinkFactors(shrinkFactors);
  TEST_SET_GET_VALUE(shrinkFactors, shrinkFilter->GetShrinkFactors());

  shrinkFilter->SetInput(fftFilter->GetOutput());

  shrinkFilter->Update();

  // InverseFFT
  typedef itk::InverseFFTImageFilter<ComplexImageType, ImageType> InverseFFTFilterType;
  typename InverseFFTFilterType::Pointer                          inverseFFT = InverseFFTFilterType::New();
  inverseFFT->SetInput(shrinkFilter->GetOutput());

  inverseFFT->Update();

  // Test Hermitian property (sym)
  bool fftIsHermitian = itk::Testing::ComplexImageIsHermitian(fftFilter->GetOutput());
  bool shrinkIsHermitian = itk::Testing::ComplexImageIsHermitian(shrinkFilter->GetOutput());
  if (!fftIsHermitian)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "fft is not Hermitian" << std::endl;
    return EXIT_FAILURE;
  }
  if (!shrinkIsHermitian)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "shrink is not Hermitian" << std::endl;
    return EXIT_FAILURE;
  }

  // Test Hermitian property
  {
    // Simmetry and Hermitian test: ComplexInverseFFT will generate output with zero imaginary part.
    // Test hermitian properties for even Images. Odd real images are not even hermitian after
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
    // Check that complex part is almost 0 after FFT and complex inverse FFT.
    {
      typedef itk::ComplexToComplexFFTImageFilter<ComplexImageType> ComplexFFTType;
      typename ComplexFFTType::Pointer                              complexInverseFFT = ComplexFFTType::New();
      complexInverseFFT->SetTransformDirection(ComplexFFTType::INVERSE);
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
      typedef itk::ComplexToComplexFFTImageFilter<ComplexImageType> ComplexFFTType;
      typename ComplexFFTType::Pointer                              complexInverseFFT = ComplexFFTType::New();
      complexInverseFFT->SetTransformDirection(ComplexFFTType::INVERSE);
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

  if (shrinkOrigin != fftOrigin)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in Origin (has changed afterShrink): " << std::endl;
    std::cerr << "Expected: " << fftOrigin << ", but got " << shrinkOrigin << std::endl;
    return EXIT_FAILURE;
  }

  if (shrinkSpacing != fftSpacing * 0.5)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in Spacing (should be half afterShrink): " << std::endl;
    std::cerr << "Expected: " << fftSpacing * 0.5 << ", but got " << shrinkSpacing << std::endl;
    return EXIT_FAILURE;
  }


  //
  // Test with frequency band filter.
  //
  typename ShrinkType::Pointer shrinkNoIntersectionFilter = ShrinkType::New();
  shrinkNoIntersectionFilter->SetInput(fftFilter->GetOutput());
  bool lowFreqThresholdPassing = true;
  bool highFreqThresholdPassing = false;
  shrinkNoIntersectionFilter->GetFrequencyBandFilter()->SetPassBand(lowFreqThresholdPassing, highFreqThresholdPassing);

  typename ShrinkType::Pointer shrinkIntersectionPassFilter = ShrinkType::New();
  shrinkIntersectionPassFilter->SetInput(fftFilter->GetOutput());
  shrinkIntersectionPassFilter->GetFrequencyBandFilter()->SetFrequencyThresholdsInRadians(itk::Math::pi_over_2,
                                                                                          itk::Math::pi_over_2);
  shrinkIntersectionPassFilter->GetFrequencyBandFilter()->SetPassBand(true, true);

  typedef itk::AddImageFilter<ComplexImageType, ComplexImageType> AddFilterType;
  typename AddFilterType::Pointer                                 addFilter = AddFilterType::New();
  addFilter->SetInput1(shrinkNoIntersectionFilter->GetOutput());
  addFilter->SetInput2(shrinkIntersectionPassFilter->GetOutput());
  typename InverseFFTFilterType::Pointer inverseFFTAdd = InverseFFTFilterType::New();
  inverseFFTAdd->SetInput(addFilter->GetOutput());

  typedef itk::Testing::ComparisonImageFilter<ImageType, ImageType> DifferenceFilterType;
  typename DifferenceFilterType::Pointer                            differenceFilter = DifferenceFilterType::New();
  differenceFilter->SetToleranceRadius(0);
  differenceFilter->SetDifferenceThreshold(0);
  differenceFilter->SetValidInput(inverseFFT->GetOutput());
  differenceFilter->SetTestInput(inverseFFTAdd->GetOutput());
  differenceFilter->Update();

  unsigned int numberOfDiffPixels = differenceFilter->GetNumberOfPixelsWithDifferences();
  if (numberOfDiffPixels > 0)
  {
    std::cerr << "Test failed! " << std::endl;
    std::cerr << "Expected images to be equal, but got " << numberOfDiffPixels << "unequal pixels" << std::endl;
    return EXIT_FAILURE;
  }


  // Write output
  typedef itk::Image<float, Dimension>                    FloatImageType;
  typedef itk::CastImageFilter<ImageType, FloatImageType> CastType;
  typename CastType::Pointer                              castFilter = CastType::New();

  castFilter->SetInput(inverseFFT->GetOutput());

  typedef itk::ImageFileWriter<FloatImageType> WriterType;
  typename WriterType::Pointer                 writer = WriterType::New();
  writer->SetFileName(outputImage);
  writer->SetInput(castFilter->GetOutput());

  TRY_EXPECT_NO_EXCEPTION(writer->Update());


#ifdef ITK_VISUALIZE_TESTS
  itk::Testing::ViewImage(zeroDCFilter->GetOutput(), "Original");
  itk::Testing::ViewImage(inverseFFT->GetOutput(), "FrequencyShrinker");
  // Compare with regular shrink filter.
  typedef itk::ShrinkImageFilter<ImageType, ImageType> RegularShrinkType;
  typename RegularShrinkType::Pointer                  regularShrinkFilter = RegularShrinkType::New();
  regularShrinkFilter->SetInput(zeroDCFilter->GetOutput());
  regularShrinkFilter->SetShrinkFactors(2);
  regularShrinkFilter->Update();
  itk::Testing::ViewImage(regularShrinkFilter->GetOutput(), "Regular shrinker");

  // Complex to real
  typedef itk::ComplexToRealImageFilter<ComplexImageType, ImageType> ComplexToRealFilter;
  typename ComplexToRealFilter::Pointer                              complexToRealFilter = ComplexToRealFilter::New();
  complexToRealFilter->SetInput(fftFilter->GetOutput());
  complexToRealFilter->Update();
  itk::Testing::ViewImage(complexToRealFilter->GetOutput(), "ComplexToReal. Original");
  typename ComplexToRealFilter::Pointer complexToRealFilterShrink = ComplexToRealFilter::New();
  complexToRealFilterShrink->SetInput(shrinkFilter->GetOutput());
  complexToRealFilterShrink->Update();
  itk::Testing::ViewImage(complexToRealFilterShrink->GetOutput(), "ComplexToReal. Shrinked");

  // Complex to imaginary
  // typedef itk::ComplexToImaginaryImageFilter< ComplexImageType, ImageType > ComplexToImaginaryFilter;
  // typename ComplexToImaginaryFilter::Pointer complexToImaginaryFilter = ComplexToImaginaryFilter::New();
  // complexToImaginaryFilter->SetInput( fftFilter->GetOutput() );
  // complexToImaginaryFilter->Update();
  // itk::Testing::ViewImage( complexToImaginaryFilter->GetOutput(), "ComplexToImaginary. Original" );
  // typename ComplexToImaginaryFilter::Pointer complexToImaginaryFilterShrink = ComplexToImaginaryFilter::New();
  // complexToImaginaryFilterShrink->SetInput( shrinkFilter->GetOutput() );
  // complexToImaginaryFilterShrink->Update();
  // itk::Testing::ViewImage( complexToImaginaryFilterShrink->GetOutput(), "ComplexToImaginary. Shrinked" );

#endif

  return EXIT_SUCCESS;
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

  const unsigned int                            ImageDimension = 3;
  typedef double                                PixelType;
  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  typedef FFTFilterType::OutputImageType        ComplexImageType;

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  typedef itk::FrequencyShrinkImageFilter<ComplexImageType> ShrinkType;
  ShrinkType::Pointer                                       shrinkFilter = ShrinkType::New();

  EXERCISE_BASIC_OBJECT_METHODS(shrinkFilter, FrequencyShrinkImageFilter, ImageToImageFilter);


  unsigned int dimension = 3;
  if (argc == 4)
  {
    dimension = atoi(argv[3]);
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

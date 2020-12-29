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
#include "itkFrequencyExpandImageFilter.h"
#include "itkExpandWithZerosImageFilter.h"
#include "itkComplexToComplexFFTImageFilter.h"
#include "itkImageRegionConstIterator.h"
#include "itkZeroDCImageFilter.h"
#include "itkIsotropicWaveletTestUtilities.h"
#include "itkCastImageFilter.h"
#include "itkTestingMacros.h"

#include <memory>
#include <string>
#include <cmath>

// Visualize for dev/debug purposes. Set in cmake file. Requires VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

template <unsigned int VDimension>
int
runFrequencyExpandTest(const std::string & inputImage, const std::string & outputImage)
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
  // fftFilter->SetInput(subtractFilter->GetOutput());
  fftFilter->SetInput(zeroDCFilter->GetOutput());

  using ComplexImageType = typename FFTFilterType::OutputImageType;

  // ExpandFrequency
  using ExpandType = itk::FrequencyExpandImageFilter<ComplexImageType>;
  auto expandFilter = ExpandType::New();
  expandFilter->SetInput(fftFilter->GetOutput());

  unsigned int                           expandFactor = 2;
  typename ExpandType::ExpandFactorsType expandFactors;
  expandFactors.Fill(expandFactor);
  expandFilter->SetExpandFactors(expandFactors);
  ITK_TEST_SET_GET_VALUE(expandFactors, expandFilter->GetExpandFactors());
  expandFilter->Update();

  // Test size and metadata
  typename ComplexImageType::PointType   fftOrigin = fftFilter->GetOutput()->GetOrigin();
  typename ComplexImageType::SpacingType fftSpacing = fftFilter->GetOutput()->GetSpacing();
  typename ComplexImageType::PointType   expandOrigin = expandFilter->GetOutput()->GetOrigin();
  typename ComplexImageType::SpacingType expandSpacing = expandFilter->GetOutput()->GetSpacing();

  if (expandOrigin != fftOrigin)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in Origin (has changed afterExpand): " << std::endl;
    std::cerr << "Expected: " << fftOrigin << ", but got " << expandOrigin << std::endl;
    testPassed = false;
  }

  if (expandSpacing != fftSpacing / expandFactor)
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in Spacing : " << std::endl;
    std::cerr << "Expected: " << fftSpacing * expandFactor << ", but got " << expandSpacing << std::endl;
    testPassed = false;
  }

  // InverseFFT
  using InverseFFTFilterType = itk::InverseFFTImageFilter<ComplexImageType, ImageType>;
  auto inverseFFT = InverseFFTFilterType::New();
  inverseFFT->SetInput(expandFilter->GetOutput());
  inverseFFT->Update();

  /***************** Hermitian property (sym) *****************************/
  bool fftIsHermitian = itk::Testing::ComplexImageIsHermitian(fftFilter->GetOutput());
  bool expandIsHermitian = itk::Testing::ComplexImageIsHermitian(expandFilter->GetOutput());
  if (!fftIsHermitian)
  {
    std::cerr << "fft is not Hermitian" << std::endl;
    // return EXIT_FAILURE;
  }
  if (!expandIsHermitian)
  {
    std::cerr << "expand is not Hermitian" << std::endl;
    // return EXIT_FAILURE;
  }
  /***************** Hermitian property *****************************/
  // Test Hermitian properties for even Images. Odd real images are not even Hermitian after FFT.
  itk::FixedArray<bool, Dimension> inputSizeIsEven;
  bool                             imageIsEven = true;
  for (unsigned int dim = 0; dim < Dimension; ++dim)
  {
    inputSizeIsEven[dim] = (reader->GetOutput()->GetLargestPossibleRegion().GetSize()[dim] % 2 == 0);
    if (inputSizeIsEven[dim] == false)
    {
      imageIsEven = false;
    }
  }

  if (imageIsEven)
  {
    // Simmetry and Hermitian test: ComplexInverseFFT will generate output with zero imaginary part.
    // Check that complex part is almost 0 after FFT and complex inverse FFT.
    {
      std::cout << "Even Image?: " << imageIsEven << std::endl;
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
                     "hermitian. #Not_zero_imag_value Pixels: "
                  << notZeroComplexError << " accumSquareDifference: " << accumSqDiff << std::endl;
      }
    }
    // Check that complex part is almost 0 filter is correct after expand
    {
      using ComplexFFTType = itk::ComplexToComplexFFTImageFilter<ComplexImageType>;
      auto complexInverseFFT = ComplexFFTType::New();
      complexInverseFFT->SetTransformDirection(ComplexFFTType::TransformDirectionEnum::INVERSE);
      complexInverseFFT->SetInput(expandFilter->GetOutput());
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
        std::cout << "Dev note: After the EXPAND filter the image is not "
                     "Hermitian. #Not_zero_imag_value Pixels: "
                  << notZeroComplexError << " accumSquareDifference: " << accumSqDiff << std::endl;
      }
    }
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
  itk::ViewImage<ImageType>::View(inverseFFT->GetOutput(), "FrequencyExpander");
  // Compare with regular expand filter.
  using RegularExpandType = itk::ExpandWithZerosImageFilter<ImageType, ImageType>;
  auto regularExpandFilter = RegularExpandType::New();
  regularExpandFilter->SetInput(reader->GetOutput());
  regularExpandFilter->SetExpandFactors(2);
  regularExpandFilter->Update();
  itk::ViewImage<ImageType>::View(regularExpandFilter->GetOutput(), "Regular expander (adding zeros)");

  // Complex to real
  // using ComplexToRealFilter = itk::ComplexToRealImageFilter<ComplexImageType, ImageType>;
  // auto complexToRealFilter = ComplexToRealFilter::New();
  // complexToRealFilter->SetInput(fftFilter->GetOutput() );
  // complexToRealFilter->Update();
  // itk::ViewImage<ImageType>::View(complexToRealFilter->GetOutput());
  // auto complexToRealFilterExpand = ComplexToRealFilter::New();
  // complexToRealFilterExpand->SetInput(expandFilter->GetOutput() );
  // complexToRealFilterExpand->Update();
  // itk::ViewImage<ImageType>::View(complexToRealFilterExpand->GetOutput());
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
itkFrequencyExpandTest(int argc, char * argv[])
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
  using FrequencyExpandImageFilterType = itk::FrequencyExpandImageFilter<ComplexImageType>;
  auto expandFilter = FrequencyExpandImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(expandFilter, FrequencyExpandImageFilter, ImageToImageFilter);

  unsigned int dimension = 3;
  if (argc == 4)
  {
    dimension = std::stoi(argv[3]);
  }

  if (dimension == 2)
  {
    return runFrequencyExpandTest<2>(inputImage, outputImage);
  }
  else if (dimension == 3)
  {
    return runFrequencyExpandTest<3>(inputImage, outputImage);
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}

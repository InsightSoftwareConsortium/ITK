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
  // fftFilter->SetInput(subtractFilter->GetOutput());
  fftFilter->SetInput(zeroDCFilter->GetOutput());

  typedef typename FFTFilterType::OutputImageType ComplexImageType;

  // ExpandFrequency
  typedef itk::FrequencyExpandImageFilter<ComplexImageType> ExpandType;
  typename ExpandType::Pointer                              expandFilter = ExpandType::New();
  expandFilter->SetInput(fftFilter->GetOutput());

  unsigned int                           expandFactor = 2;
  typename ExpandType::ExpandFactorsType expandFactors;
  expandFactors.Fill(expandFactor);
  expandFilter->SetExpandFactors(expandFactors);
  TEST_SET_GET_VALUE(expandFactors, expandFilter->GetExpandFactors());
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
  typedef itk::InverseFFTImageFilter<ComplexImageType, ImageType> InverseFFTFilterType;
  typename InverseFFTFilterType::Pointer                          inverseFFT = InverseFFTFilterType::New();
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
  {
    // Simmetry and Hermitian test: ComplexInverseFFT will generate output with zero imaginary part.
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
    // Check that complex part is almost 0 after FFT and complex inverse FFT.
    {
      std::cout << "Even Image?: " << imageIsEven << std::endl;
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
                     "hermitian. #Not_zero_imag_value Pixels: "
                  << notZeroComplexError << " accumSquareDifference: " << accumSqDiff << std::endl;
      }
    }
    // Check that complex part is almost 0 filter is correct after expand
    {
      typedef itk::ComplexToComplexFFTImageFilter<ComplexImageType> ComplexFFTType;
      typename ComplexFFTType::Pointer                              complexInverseFFT = ComplexFFTType::New();
      complexInverseFFT->SetTransformDirection(ComplexFFTType::INVERSE);
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
  itk::Testing::ViewImage(inverseFFT->GetOutput(), "FrequencyExpander");
  // Compare with regular expand filter.
  typedef itk::ExpandWithZerosImageFilter<ImageType, ImageType> RegularExpandType;
  typename RegularExpandType::Pointer                           regularExpandFilter = RegularExpandType::New();
  regularExpandFilter->SetInput(reader->GetOutput());
  regularExpandFilter->SetExpandFactors(2);
  regularExpandFilter->Update();
  itk::Testing::ViewImage(regularExpandFilter->GetOutput(), "Regular expander (adding zeros)");

  // Complex to real
  // typedef itk::ComplexToRealImageFilter<ComplexImageType, ImageType> ComplexToRealFilter;
  // ComplexToRealFilter::Pointer complexToRealFilter = ComplexToRealFilter::New();
  // complexToRealFilter->SetInput(fftFilter->GetOutput() );
  // complexToRealFilter->Update();
  // itk::Testing::ViewImage(complexToRealFilter->GetOutput());
  // ComplexToRealFilter::Pointer complexToRealFilterExpand = ComplexToRealFilter::New();
  // complexToRealFilterExpand->SetInput(expandFilter->GetOutput() );
  // complexToRealFilterExpand->Update();
  // itk::Testing::ViewImage(complexToRealFilterExpand->GetOutput());
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

  const unsigned int                            ImageDimension = 3;
  typedef double                                PixelType;
  typedef itk::Image<PixelType, ImageDimension> ImageType;
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  typedef FFTFilterType::OutputImageType        ComplexImageType;

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  typedef itk::FrequencyExpandImageFilter<ComplexImageType> FrequencyExpandImageFilterType;
  FrequencyExpandImageFilterType::Pointer                   expandFilter = FrequencyExpandImageFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS(expandFilter, FrequencyExpandImageFilter, ImageToImageFilter);

  unsigned int dimension = 3;
  if (argc == 4)
  {
    dimension = atoi(argv[3]);
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

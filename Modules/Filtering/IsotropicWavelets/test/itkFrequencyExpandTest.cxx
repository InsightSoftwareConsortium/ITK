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
#include <memory>
#include <string>
#include <cmath>
#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkInverseFFTImageFilter.h"
#include <itkComplexToRealImageFilter.h>
#include <itkFrequencyExpandImageFilter.h>
#include <itkExpandImageFilter.h>
#include <itkComplexToComplexFFTImageFilter.h>
#include <itkImageRegionConstIterator.h>
#include <itkZeroDCImageFilter.h>
#include "itkIsotropicWaveletTestUtilities.h"
#include <itkCastImageFilter.h>
using namespace std;
using namespace itk;

// Visualize for dev/debug purposes. Set in cmake file. Require VTK
#ifdef ITK_VISUALIZE_TESTS
#  include "itkViewImage.h"
#endif

template <unsigned int N>
int
runFrequencyExpandTest(const std::string & inputImage, const std::string & outputImage)
{
  const unsigned int                       dimension = N;
  typedef double                           PixelType;
  typedef itk::Image<PixelType, dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;
  typename ReaderType::Pointer             reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();
  reader->UpdateLargestPossibleRegion();

  /***** Calculate mean value and substract: ****/
  typedef itk::ZeroDCImageFilter<ImageType> ZeroDCFilterType;
  typename ZeroDCFilterType::Pointer        zeroDCFilter = ZeroDCFilterType::New();
  zeroDCFilter->SetInput(reader->GetOutput());
  zeroDCFilter->Update();
  /**********************************************/

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
  expandFilter->SetExpandFactors(2);
  expandFilter->Update();

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
    // Simmetry and hermitian test: ComplexInverseFFT will generate output with zero imaginary part.
    // Test hermitian properties for even Images. Odd real images are not even hermitian after FFT.
    FixedArray<bool, dimension> inputSizeIsEven;
    bool                        imageIsEven(true);
    for (unsigned int dim = 0; dim < dimension; ++dim)
    {
      inputSizeIsEven[dim] = (reader->GetOutput()->GetLargestPossibleRegion().GetSize()[dim] % 2 == 0);
      if (inputSizeIsEven[dim] == false)
        imageIsEven = false;
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
      unsigned int not_zero_complex_error = 0;
      double       accum_square_difference(0);
      while (!complexInverseIt.IsAtEnd())
      {
        typename ComplexImageType::PixelType::value_type imag_value = complexInverseIt.Get().imag();
        bool not_equal = itk::Math::NotAlmostEquals<typename ComplexImageType::PixelType::value_type>(imag_value, 0.0);
        if (not_equal)
        {
          ++not_zero_complex_error;
          accum_square_difference += imag_value * imag_value;
        }

        ++complexInverseIt;
      }
      // accum_square_difference /= not_zero_complex_error;
      if (not_zero_complex_error > 0)
      {
        std::cout << "Dev note: After the FFT filter the image is not hermitian. #Not_zero_imag_value Pixels: "
                  << not_zero_complex_error << " accumSquareDifference: " << accum_square_difference << std::endl;
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
      unsigned int not_zero_complex_error = 0;
      double       accum_square_difference(0);
      while (!complexInverseIt.IsAtEnd())
      {
        typename ComplexImageType::PixelType::value_type imag_value = complexInverseIt.Get().imag();
        bool not_equal = itk::Math::NotAlmostEquals<typename ComplexImageType::PixelType::value_type>(imag_value, 0.0);
        if (not_equal)
        {
          ++not_zero_complex_error;
          accum_square_difference += imag_value * imag_value;
        }
        ++complexInverseIt;
      }
      // accum_square_difference /= not_zero_complex_error;
      if (not_zero_complex_error > 0)
      {
        std::cout << "Dev note: After the EXPAND filter the image is not hermitian. #Not_zero_imag_value Pixels: "
                  << not_zero_complex_error << " accumSquareDifference: " << accum_square_difference << std::endl;
      }
    }
  }
  /*************End Hermitian property *****************************/

  // Write Output for comparisson
  typedef itk::Image<float, dimension>                    FloatImageType;
  typedef itk::CastImageFilter<ImageType, FloatImageType> CastType;
  typename CastType::Pointer                              castFilter = CastType::New();
  castFilter->SetInput(inverseFFT->GetOutput());

  typedef itk::ImageFileWriter<FloatImageType> WriterType;
  typename WriterType::Pointer                 writer = WriterType::New();
  writer->SetFileName(outputImage);
  writer->SetInput(castFilter->GetOutput());

  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error writing the FrequencyExpand image: " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
  }

#ifdef ITK_VISUALIZE_TESTS
  Testing::ViewImage(zeroDCFilter->GetOutput(), "Original");
  Testing::ViewImage(inverseFFT->GetOutput(), "FrequencyExpander");
  // //Compare with regular expand filter.
  typedef itk::ExpandImageFilter<ImageType, ImageType> RegularExpandType;
  typename RegularExpandType::Pointer                  regularExpandFilter = RegularExpandType::New();
  regularExpandFilter->SetInput(reader->GetOutput());
  regularExpandFilter->SetExpandFactors(2);
  regularExpandFilter->Update();
  Testing::ViewImage(regularExpandFilter->GetOutput(), "Regular expander");

  // Complex to real
  // typedef itk::ComplexToRealImageFilter<ComplexImageType, ImageType> ComplexToRealFilter;
  // ComplexToRealFilter::Pointer complexToRealFilter = ComplexToRealFilter::New();
  // complexToRealFilter->SetInput(fftFilter->GetOutput() );
  // complexToRealFilter->Update();
  // Testing::ViewImage(complexToRealFilter->GetOutput());
  // ComplexToRealFilter::Pointer complexToRealFilterExpand = ComplexToRealFilter::New();
  // complexToRealFilterExpand->SetInput(expandFilter->GetOutput() );
  // complexToRealFilterExpand->Update();
  // Testing::ViewImage(complexToRealFilterExpand->GetOutput());
#endif

  return EXIT_SUCCESS;
}

int
itkFrequencyExpandTest(int argc, char * argv[])
{
  if (argc < 3 || argc > 4)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage [dimension]" << std::endl;
    return EXIT_FAILURE;
  }
  const string inputImage = argv[1];
  const string outputImage = argv[2];

  unsigned int dimension = 3;
  if (argc == 4)
    dimension = atoi(argv[3]);
  if (dimension == 2)
    return runFrequencyExpandTest<2>(inputImage, outputImage);
  else if (dimension == 3)
    return runFrequencyExpandTest<3>(inputImage, outputImage);
  else
  {
    std::cerr << "Error: only 2 or 3 dimensions allowed, " << dimension << " selected." << std::endl;
    return EXIT_FAILURE;
  }
}

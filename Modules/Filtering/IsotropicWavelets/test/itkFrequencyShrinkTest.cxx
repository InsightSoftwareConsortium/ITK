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
#include <itkFrequencyShrinkImageFilter.h>
#include <itkShrinkImageFilter.h>
#include <itkSubtractImageFilter.h>
#include <itkComplexToComplexFFTImageFilter.h>
#include <itkImageRegionConstIterator.h>
#include <itkStatisticsImageFilter.h>
using namespace std;
using namespace itk;

// Visualize for dev/debug purposes. Set in cmake file. Require VTK
#if ITK_VISUALIZE_TESTS != 0
#  include "itkViewImage.h"
#endif

int
itkFrequencyShrinkTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Usage: " << argv[0] << " inputImage outputImage" << std::endl;
    return EXIT_FAILURE;
  }
  const string inputImage = argv[1];
  const string outputImage = argv[2];

  const unsigned int                       dimension = 3;
  typedef float                            PixelType;
  typedef itk::Image<PixelType, dimension> ImageType;
  typedef itk::ImageFileReader<ImageType>  ReaderType;
  ReaderType::Pointer                      reader = ReaderType::New();
  reader->SetFileName(inputImage);
  reader->Update();
  reader->UpdateLargestPossibleRegion();

  /***** Calculate mean value and substract: ****/
  typedef itk::StatisticsImageFilter<ImageType> StatisticsFilterType;
  StatisticsFilterType::Pointer                 statisticsFilter = StatisticsFilterType::New();
  statisticsFilter->SetInput(reader->GetOutput());
  statisticsFilter->Update();
  typedef itk::SubtractImageFilter<ImageType> SubtractFilterType;
  SubtractFilterType::Pointer                 subtractFilter = SubtractFilterType::New();
  subtractFilter->SetInput1(reader->GetOutput());
  subtractFilter->SetConstant2(statisticsFilter->GetMean());
  subtractFilter->Update();
  /**********************************************/

  // Perform FFT on input image.
  typedef itk::ForwardFFTImageFilter<ImageType> FFTFilterType;
  FFTFilterType::Pointer                        fftFilter = FFTFilterType::New();
  fftFilter->SetInput(subtractFilter->GetOutput());
  typedef FFTFilterType::OutputImageType ComplexImageType;

  // ShinkFrequency
  typedef itk::FrequencyShrinkImageFilter<ComplexImageType> ShrinkType;
  ShrinkType::Pointer                                       shrinkFilter = ShrinkType::New();
  shrinkFilter->SetInput(fftFilter->GetOutput());
  shrinkFilter->SetShrinkFactors(2);
  shrinkFilter->Update();

  // InverseFFT
  typedef itk::InverseFFTImageFilter<ComplexImageType, ImageType> InverseFFTFilterType;
  InverseFFTFilterType::Pointer                                   inverseFFT = InverseFFTFilterType::New();
  inverseFFT->SetInput(shrinkFilter->GetOutput());
  inverseFFT->Update();

  /***************** Hermitian property *****************************/
  {
    // Simmetry and hermitian test: ComplexInverseFFT will generate output with zero imaginary part.
    // Test hermitian properties for even Images. Odd real images are not even hermitian after
    FixedArray<bool, dimension> inputSizeIsEven;
    bool                        imageIsEven(true);
    for (unsigned int dim = 0; dim < dimension; ++dim)
    {
      inputSizeIsEven[dim] = (subtractFilter->GetOutput()->GetLargestPossibleRegion().GetSize()[dim] % 2 == 0);
      if (inputSizeIsEven[dim] == false)
        imageIsEven = false;
    }
    std::cout << "Image Even? " << imageIsEven << std::endl;
    // Check that complex part is almost 0 after FFT and complex inverse FFT.
    {
      typedef itk::ComplexToComplexFFTImageFilter<ComplexImageType> ComplexFFTType;
      ComplexFFTType::Pointer                                       complexInverseFFT = ComplexFFTType::New();
      complexInverseFFT->SetTransformDirection(ComplexFFTType::INVERSE);
      complexInverseFFT->SetInput(fftFilter->GetOutput());
      complexInverseFFT->Update();

      itk::ImageRegionConstIterator<ComplexImageType> complexInverseIt(
        complexInverseFFT->GetOutput(), complexInverseFFT->GetOutput()->GetLargestPossibleRegion());
      complexInverseIt.GoToBegin();
      unsigned int not_zero_complex_error = 0;
      while (!complexInverseIt.IsAtEnd())
      {
        bool not_equal =
          itk::Math::NotAlmostEquals<ComplexImageType::PixelType::value_type>(complexInverseIt.Get().imag(), 0.0);
        if (not_equal)
          ++not_zero_complex_error;
        ++complexInverseIt;
      }
      if (not_zero_complex_error > 0)
      {
        std::cout << "Dev Note: After the FFT filter the image is not hermitian." << std::endl;
      }
    }
    // Check that complex part is almost 0 filter is correct after shrink
    {
      typedef itk::ComplexToComplexFFTImageFilter<ComplexImageType> ComplexFFTType;
      ComplexFFTType::Pointer                                       complexInverseFFT = ComplexFFTType::New();
      complexInverseFFT->SetTransformDirection(ComplexFFTType::INVERSE);
      complexInverseFFT->SetInput(shrinkFilter->GetOutput());
      complexInverseFFT->Update();

      itk::ImageRegionConstIterator<ComplexImageType> complexInverseIt(
        complexInverseFFT->GetOutput(), complexInverseFFT->GetOutput()->GetLargestPossibleRegion());
      complexInverseIt.GoToBegin();
      unsigned int not_zero_complex_error = 0;
      while (!complexInverseIt.IsAtEnd())
      {
        bool not_equal =
          itk::Math::NotAlmostEquals<ComplexImageType::PixelType::value_type>(complexInverseIt.Get().imag(), 0.0);
        if (not_equal)
          ++not_zero_complex_error;
        ++complexInverseIt;
      }
      if (not_zero_complex_error > 0)
      {
        std::cout << "Dev note: After the SHRINK filter the image is not hermitian" << std::endl;
      }
    }
  }
  /*************End Hermitian property *****************************/

  // Write Output for comparisson
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer                     writer = WriterType::New();
  writer->SetFileName(outputImage);
  writer->SetInput(inverseFFT->GetOutput());

  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error writing the FrequencyShrink image: " << std::endl;
    std::cerr << error << std::endl;
    return EXIT_FAILURE;
  }

#if ITK_VISUALIZE_TESTS != 0
  Testing::ViewImage(subtractFilter->GetOutput(), "Original");
  Testing::ViewImage(inverseFFT->GetOutput(), "FrequencyShrinker");
  // Compare with regular shrink filter.
  typedef itk::ShrinkImageFilter<ImageType, ImageType> RegularShrinkType;
  RegularShrinkType::Pointer                           regularShrinkFilter = RegularShrinkType::New();
  regularShrinkFilter->SetInput(reader->GetOutput());
  regularShrinkFilter->SetShrinkFactors(2);
  regularShrinkFilter->Update();
  Testing::ViewImage(regularShrinkFilter->GetOutput(), "Regular shrinker");

  // Complex to real
  typedef itk::ComplexToRealImageFilter<ComplexImageType, ImageType> ComplexToRealFilter;
  ComplexToRealFilter::Pointer                                       complexToRealFilter = ComplexToRealFilter::New();
  complexToRealFilter->SetInput(fftFilter->GetOutput());
  complexToRealFilter->Update();
  Testing::ViewImage(complexToRealFilter->GetOutput(), "ComplexToReal. Original");
  ComplexToRealFilter::Pointer complexToRealFilterShrink = ComplexToRealFilter::New();
  complexToRealFilterShrink->SetInput(shrinkFilter->GetOutput());
  complexToRealFilterShrink->Update();
  Testing::ViewImage(complexToRealFilterShrink->GetOutput(), "ComplexToReal. Shrinked");
#endif

  return EXIT_SUCCESS;
}

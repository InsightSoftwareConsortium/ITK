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

#include <complex>
#include <string>

#include "itkComplexToRealImageFilter.h"
#include "itkComposeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkComplexToComplex1DFFTImageFilter.h"
#include "itkVnlComplexToComplex1DFFTImageFilter.h"
#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)
#  include "itkFFTWComplexToComplex1DFFTImageFilter.h"
#endif

#include "itkTestingMacros.h"

template <typename FFTType>
int
doTest(const char * inputRealFullImage, const char * inputImaginaryFullImage, const char * outputImage)
{
  using ComplexImageType = typename FFTType::InputImageType;
  using ImageType = itk::Image<typename itk::NumericTraits<typename ComplexImageType::PixelType>::ValueType,
                               ComplexImageType::ImageDimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  using JoinFilterType = itk::ComposeImageFilter<ImageType, ComplexImageType>;
  using ToRealFilterType = itk::ComplexToRealImageFilter<ComplexImageType, ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  auto readerReal = ReaderType::New();
  auto readerImag = ReaderType::New();
  auto fft = FFTType::New();
  auto joinFilter = JoinFilterType::New();
  auto toReal = ToRealFilterType::New();
  auto writer = WriterType::New();

  readerReal->SetFileName(inputRealFullImage);
  readerImag->SetFileName(inputImaginaryFullImage);
  joinFilter->SetInput1(readerReal->GetOutput());
  joinFilter->SetInput2(readerImag->GetOutput());

  auto transformDirection = FFTType::INVERSE;
  fft->SetTransformDirection(transformDirection);
  ITK_TEST_SET_GET_VALUE(transformDirection, fft->GetTransformDirection());

  fft->SetInput(joinFilter->GetOutput());
  toReal->SetInput(fft->GetOutput());
  writer->SetInput(toReal->GetOutput());
  writer->SetFileName(outputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}

int
itkComplexToComplex1DFFTImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImageRealFull inputImageImaginaryFull outputImage [backend]" << std::endl;
    std::cerr << "backend implementation options:" << std::endl;
    std::cerr << "  0 default" << std::endl;
    std::cerr << "  1 VNL" << std::endl;
    std::cerr << "  2 FFTW" << std::endl;
    std::cerr << std::flush;
    return EXIT_FAILURE;
  }

  using PixelType = double;
  const unsigned int Dimension = 2;
  using ComplexImageType = itk::Image<std::complex<PixelType>, Dimension>;

  int backend = 0;
  if (argc > 4)
  {
    backend = std::stoi(argv[4]);
  }

  if (backend == 0)
  {
    using FFTInverseType = itk::ComplexToComplex1DFFTImageFilter<ComplexImageType, ComplexImageType>;
    auto inverse = FFTInverseType::New();

    if (inverse == nullptr)
    {
      std::cerr << "Failed to register a backend for ComplexToComplex1DFFTImageFilter" << std::endl;
      return EXIT_FAILURE;
    }

    ITK_EXERCISE_BASIC_OBJECT_METHODS(inverse, ComplexToComplex1DFFTImageFilter, ImageToImageFilter);

    return doTest<FFTInverseType>(argv[1], argv[2], argv[3]);
  }
  else if (backend == 1)
  {
    using FFTInverseType = itk::VnlComplexToComplex1DFFTImageFilter<ComplexImageType, ComplexImageType>;
    return doTest<FFTInverseType>(argv[1], argv[2], argv[3]);
  }
  else if (backend == 2)
  {
#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)
    using FFTInverseType = itk::FFTWComplexToComplex1DFFTImageFilter<ComplexImageType, ComplexImageType>;
    return doTest<FFTInverseType>(argv[1], argv[2], argv[3]);
#endif
  }

  std::cerr << "Backend " << backend << " (" << argv[4] << ") not implemented" << std::endl;
  return EXIT_FAILURE;
}

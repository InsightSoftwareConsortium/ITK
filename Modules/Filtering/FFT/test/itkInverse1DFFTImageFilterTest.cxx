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

#include "itkComposeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkInverse1DFFTImageFilter.h"
#include "itkVnlInverse1DFFTImageFilter.h"
#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)
#  include "itkFFTWInverse1DFFTImageFilter.h"
#endif
#include "itkTestingMacros.h"

template <typename FFTType>
int
doTest(const char * inputRealFullImage, const char * inputImaginaryFullImage, const char * outputImage)
{
  using ImageType = typename FFTType::OutputImageType;
  using ComplexImageType = typename FFTType::InputImageType;

  using ReaderType = itk::ImageFileReader<ImageType>;
  using JoinFilterType = itk::ComposeImageFilter<ImageType, ComplexImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  auto readerReal = ReaderType::New();
  auto readerImag = ReaderType::New();
  auto fft = FFTType::New();
  auto joinFilter = JoinFilterType::New();
  auto writer = WriterType::New();

  readerReal->SetFileName(inputRealFullImage);
  readerImag->SetFileName(inputImaginaryFullImage);
  joinFilter->SetInput1(readerReal->GetOutput());
  joinFilter->SetInput2(readerImag->GetOutput());
  fft->SetInput(joinFilter->GetOutput());
  writer->SetInput(fft->GetOutput());
  writer->SetFileName(outputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}

int
itkInverse1DFFTImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImagePrefix outputImage [backend]" << std::endl;
    std::cerr << "backend implementation options:" << std::endl;
    std::cerr << "  0 default" << std::endl;
    std::cerr << "  1 VNL" << std::endl;
    std::cerr << "  2 FFTW" << std::endl;
    std::cerr << std::flush;
    return EXIT_FAILURE;
  }

  using PixelType = double;
  const unsigned int Dimension = 2;

  using ImageType = itk::Image<PixelType, Dimension>;
  using ComplexImageType = itk::Image<std::complex<PixelType>, Dimension>;

  int backend = 0;
  if (argc > 3)
  {
    for (size_t idx = 0; idx < static_cast<size_t>(argc); ++idx)
    {
      std::cout << argv[idx] << std::endl;
    }
    backend = std::stoi(argv[4]);
  }

  if (backend == 0)
  {
    using FFTInverseType = itk::Inverse1DFFTImageFilter<ComplexImageType, ImageType>;

    // Instantiate a filter to exercise basic object methods
    auto fft = FFTInverseType::New();
    ITK_EXERCISE_BASIC_OBJECT_METHODS(fft, Inverse1DFFTImageFilter, ImageToImageFilter);

    itk::SizeValueType sizeGreatestPrimeFactor = 2;
    ITK_TEST_SET_GET_VALUE(sizeGreatestPrimeFactor, fft->GetSizeGreatestPrimeFactor());


    return doTest<FFTInverseType>(argv[1], argv[2], argv[3]);
  }
  else if (backend == 1)
  {
    using FFTInverseType = itk::VnlInverse1DFFTImageFilter<ComplexImageType, ImageType>;

    // Instantiate a filter to exercise basic object methods
    auto fft = FFTInverseType::New();
    ITK_EXERCISE_BASIC_OBJECT_METHODS(fft, VnlInverse1DFFTImageFilter, Inverse1DFFTImageFilter);


    return doTest<FFTInverseType>(argv[1], argv[2], argv[3]);
  }
  else if (backend == 2)
  {
#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)
    using FFTInverseType = itk::FFTWInverse1DFFTImageFilter<ComplexImageType, ImageType>;

    // Instantiate a filter to exercise basic object methods
    auto fft = FFTInverseType::New();
    ITK_EXERCISE_BASIC_OBJECT_METHODS(fft, FFTWInverse1DFFTImageFilter, Inverse1DFFTImageFilter);


    return doTest<FFTInverseType>(argv[1], argv[2], argv[3]);
#endif
  }

  std::cerr << "Backend " << backend << " (" << argv[4] << ") not implemented" << std::endl;
  return EXIT_FAILURE;
}

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

#include <complex>
#include <string>

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkForward1DFFTImageFilter.h"
#include "itkInverse1DFFTImageFilter.h"

#include "itkVnlForward1DFFTImageFilter.h"
#include "itkVnlInverse1DFFTImageFilter.h"
#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)
#  include "itkFFTWForward1DFFTImageFilter.h"
#  include "itkFFTWInverse1DFFTImageFilter.h"
#endif
#include "itkTestingMacros.h"

template <typename FFTForwardType, typename FFTInverseType>
int
doTest(const char * inputImage, const char * outputImage)
{
  const unsigned int direction = 1;

  using ImageType = typename FFTForwardType::InputImageType;

  using ReaderType = itk::ImageFileReader<ImageType>;
  auto reader = ReaderType::New();
  reader->SetFileName(inputImage);

  auto fftForward = FFTForwardType::New();
  fftForward->SetInput(reader->GetOutput());
  fftForward->SetDirection(direction);

  auto fftInverse = FFTInverseType::New();
  fftInverse->SetInput(fftForward->GetOutput());
  fftInverse->SetDirection(direction);

  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetInput(fftInverse->GetOutput());
  writer->SetFileName(outputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  fftForward.Print(std::cout);
  fftInverse.Print(std::cout);

  return EXIT_SUCCESS;
}

int
itkFFT1DImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImage outputImage [backend]" << std::endl;
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
    backend = std::stoi(argv[3]);
  }

  if (backend == 0)
  {
    using FFTForwardType = itk::Forward1DFFTImageFilter<ImageType, ComplexImageType>;
    using FFTInverseType = itk::Inverse1DFFTImageFilter<ComplexImageType, ImageType>;
    return doTest<FFTForwardType, FFTInverseType>(argv[1], argv[2]);
  }
  else if (backend == 1)
  {
    using FFTForwardType = itk::VnlForward1DFFTImageFilter<ImageType, ComplexImageType>;
    using FFTInverseType = itk::VnlInverse1DFFTImageFilter<ComplexImageType, ImageType>;
    return doTest<FFTForwardType, FFTInverseType>(argv[1], argv[2]);
  }
  else if (backend == 2)
  {
#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)
    using FFTForwardType = itk::FFTWForward1DFFTImageFilter<ImageType, ComplexImageType>;
    using FFTInverseType = itk::FFTWInverse1DFFTImageFilter<ComplexImageType, ImageType>;
    return doTest<FFTForwardType, FFTInverseType>(argv[1], argv[2]);
#endif
  }

  std::cerr << "Backend " << backend << " (" << argv[3] << ") not implemented" << std::endl;
  return EXIT_FAILURE;
}

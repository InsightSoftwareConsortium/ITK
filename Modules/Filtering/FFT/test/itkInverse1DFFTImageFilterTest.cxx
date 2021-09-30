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

#include "itkComposeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkInverse1DFFTImageFilter.h"
#include "itkVnlInverse1DFFTImageFilter.h"
#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)
#  include "itkFFTWInverse1DFFTImageFilter.h"
#endif
#if defined(ITKUltrasound_USE_clFFT)
#  include "itkOpenCLInverse1DFFTImageFilter.h"
#endif

template <typename FFTType>
int
doTest(const char * inputRealFullImage, const char * inputImaginaryFullImage, const char * outputImage)
{
  using ImageType = typename FFTType::OutputImageType;
  using ComplexImageType = typename FFTType::InputImageType;

  using ReaderType = itk::ImageFileReader<ImageType>;
  using JoinFilterType = itk::ComposeImageFilter<ImageType, ComplexImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  typename ReaderType::Pointer     readerReal = ReaderType::New();
  typename ReaderType::Pointer     readerImag = ReaderType::New();
  typename FFTType::Pointer        fft = FFTType::New();
  typename JoinFilterType::Pointer joinFilter = JoinFilterType::New();
  typename WriterType::Pointer     writer = WriterType::New();

  readerReal->SetFileName(inputRealFullImage);
  readerImag->SetFileName(inputImaginaryFullImage);
  joinFilter->SetInput1(readerReal->GetOutput());
  joinFilter->SetInput2(readerImag->GetOutput());
  fft->SetInput(joinFilter->GetOutput());
  writer->SetInput(fft->GetOutput());
  writer->SetFileName(outputImage);

  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & excep)
  {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
  }

  fft.Print(std::cout);

  return EXIT_SUCCESS;
}

int
itkInverse1DFFTImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImagePrefix outputImage [backend]\n";
    std::cerr << "backend implementation options:\n";
    std::cerr << "  0 default\n";
    std::cerr << "  1 VNL\n";
    std::cerr << "  2 FFTW\n";
    std::cerr << "  3 OpenCL via clFFT\n";
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
    for (size_t idx = 0; idx < argc; ++idx)
    {
      std::cout << argv[idx] << std::endl;
    }
    backend = std::stoi(argv[4]);
  }

  if (backend == 0)
  {
    using FFTInverseType = itk::Inverse1DFFTImageFilter<ComplexImageType, ImageType>;
    return doTest<FFTInverseType>(argv[1], argv[2], argv[3]);
  }
  else if (backend == 1)
  {
    using FFTInverseType = itk::VnlInverse1DFFTImageFilter<ComplexImageType, ImageType>;
    return doTest<FFTInverseType>(argv[1], argv[2], argv[3]);
  }
  else if (backend == 2)
  {
#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)
    using FFTInverseType = itk::FFTWInverse1DFFTImageFilter<ComplexImageType, ImageType>;
    return doTest<FFTInverseType>(argv[1], argv[2], argv[3]);
#endif
  }
  else if (backend == 3)
  {
#if defined(ITKUltrasound_USE_clFFT)
    using FFTInverseType = itk::OpenCLInverse1DFFTImageFilter<ComplexImageType, ImageType>;
    return doTest<FFTInverseType>(argv[1], argv[2], argv[3]);
#endif
  }

  std::cerr << "Backend " << backend << " (" << argv[4] << ") not implemented" << std::endl;
  return EXIT_FAILURE;
}

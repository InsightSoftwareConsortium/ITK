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

#include "itkComplexToRealImageFilter.h"
#include "itkComposeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkComplexToComplex1DFFTImageFilter.h"
#include "itkVnlComplexToComplex1DFFTImageFilter.h"
#if defined(ITK_USE_FFTWD) || defined(ITK_USE_FFTWF)
#  include "itkFFTWComplexToComplex1DFFTImageFilter.h"
#endif
#if defined(ITKUltrasound_USE_clFFT)
#  include "itkOpenCLComplexToComplex1DFFTImageFilter.h"
#endif

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

  typename ReaderType::Pointer       readerReal = ReaderType::New();
  typename ReaderType::Pointer       readerImag = ReaderType::New();
  typename FFTType::Pointer          fft = FFTType::New();
  typename JoinFilterType::Pointer   joinFilter = JoinFilterType::New();
  typename ToRealFilterType::Pointer toReal = ToRealFilterType::New();
  typename WriterType::Pointer       writer = WriterType::New();

  readerReal->SetFileName(inputRealFullImage);
  readerImag->SetFileName(inputImaginaryFullImage);
  joinFilter->SetInput1(readerReal->GetOutput());
  joinFilter->SetInput2(readerImag->GetOutput());
  fft->SetTransformDirection(FFTType::INVERSE);
  fft->SetInput(joinFilter->GetOutput());
  toReal->SetInput(fft->GetOutput());
  writer->SetInput(toReal->GetOutput());
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
itkComplexToComplex1DFFTImageFilterTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImageRealFull inputImageImaginaryFull outputImage [backend]\n";
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
  using ComplexImageType = itk::Image<std::complex<PixelType>, Dimension>;

  int backend = 0;
  if (argc > 4)
  {
    backend = std::stoi(argv[4]);
  }

  if (backend == 0)
  {
    using FFTInverseType = itk::ComplexToComplex1DFFTImageFilter<ComplexImageType, ComplexImageType>;
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
  else if (backend == 3)
  {
#if defined(ITKUltrasound_USE_clFFT)
    using FFTInverseType = itk::OpenCLComplexToComplex1DFFTImageFilter<ComplexImageType, ComplexImageType>;
    return doTest<FFTInverseType>(argv[1], argv[2], argv[3]);
#endif
  }

  std::cerr << "Backend " << backend << " (" << argv[4] << ") not implemented" << std::endl;
  return EXIT_FAILURE;
}

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

#include "itkLogGaborFreqImageSource.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkComplexToRealImageAdaptor.h"
#include "itkComposeImageFilter.h"
#include "itkComplexToImaginaryImageFilter.h"
#include "itkInverseFFTImageFilter.h"

int
itkLogGaborFreqImageSourceTest(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Usage: " << argv[0] << " InputImage FilterImage OutputImage InputFFT OutputFFT" << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputImageFileName = argv[1];
  const char * filterImageFileName = argv[2];
  const char * outputImageFileName = argv[3];
  const char * inputFFTImageFileName = argv[4];
  const char * outputFFTImageFileName = argv[5];

  const unsigned int Dimension = 3;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ComplexPixelType = std::complex<PixelType>;
  using ComplexImageType = itk::Image<ComplexPixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputImageFileName);

  using FFTFilterType = itk::ForwardFFTImageFilter<ImageType, ComplexImageType>;
  FFTFilterType::Pointer fftFilter = FFTFilterType::New();
  fftFilter->SetInput(reader->GetOutput());

  try
  {
    fftFilter->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error: " << error << std::endl;
    return EXIT_FAILURE;
  }


  using ComplexToRealType = itk::ComplexToRealImageAdaptor<ComplexImageType, PixelType>;
  ComplexToRealType::Pointer complexToReal = ComplexToRealType::New();
  complexToReal->SetImage(fftFilter->GetOutput());

  using LogGaborSourceType = itk::LogGaborFreqImageSource<ImageType>;
  LogGaborSourceType::Pointer logGaborSource = LogGaborSourceType::New();
  ImageType::ConstPointer     inputImage = reader->GetOutput();
  logGaborSource->SetSize(inputImage->GetLargestPossibleRegion().GetSize());
  logGaborSource->SetSpacing(inputImage->GetSpacing());
  logGaborSource->SetDirection(inputImage->GetDirection());
  logGaborSource->SetOrigin(inputImage->GetOrigin());
  logGaborSource->SetSigma(0.7);
  LogGaborSourceType::ArrayType wavelengths;
  wavelengths[0] = 9.0;
  wavelengths[1] = 12.0;
  wavelengths[2] = 7.0;
  logGaborSource->SetWavelengths(wavelengths);
  std::cout << logGaborSource << std::endl;

  using MultiplyFilterType = itk::MultiplyImageFilter<ComplexToRealType, ImageType, ImageType>;
  MultiplyFilterType::Pointer multiplyFilter = MultiplyFilterType::New();
  multiplyFilter->SetInput1(complexToReal);
  multiplyFilter->SetInput2(logGaborSource->GetOutput());

  using ComplexToImaginaryFilterType = itk::ComplexToImaginaryImageFilter<ComplexImageType, ImageType>;
  ComplexToImaginaryFilterType::Pointer complexToImaginaryFilter = ComplexToImaginaryFilterType::New();
  complexToImaginaryFilter->SetInput(fftFilter->GetOutput());

  using ComposeFilterType = itk::ComposeImageFilter<ImageType, ComplexImageType>;
  ComposeFilterType::Pointer composeFilter = ComposeFilterType::New();
  composeFilter->SetInput(0, multiplyFilter->GetOutput());
  composeFilter->SetInput(1, complexToImaginaryFilter->GetOutput());

  using InverseFFTFilterType = itk::InverseFFTImageFilter<ComplexImageType, ImageType>;
  InverseFFTFilterType::Pointer inverseFFTImageFilter = InverseFFTFilterType::New();
  inverseFFTImageFilter->SetInput(composeFilter->GetOutput());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput(logGaborSource->GetOutput());
  writer->SetFileName(filterImageFileName);
  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error: " << error << std::endl;
    return EXIT_FAILURE;
  }

  writer->SetInput(inverseFFTImageFilter->GetOutput());
  writer->SetFileName(outputImageFileName);
  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error: " << error << std::endl;
    return EXIT_FAILURE;
  }

  using ComplexWriterType = itk::ImageFileWriter<ComplexImageType>;
  ComplexWriterType::Pointer complexWriter = ComplexWriterType::New();
  complexWriter->SetInput(fftFilter->GetOutput());
  complexWriter->SetFileName(inputFFTImageFileName);
  try
  {
    complexWriter->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error: " << error << std::endl;
    return EXIT_FAILURE;
  }

  complexWriter->SetInput(composeFilter->GetOutput());
  complexWriter->SetFileName(outputFFTImageFileName);
  try
  {
    complexWriter->Update();
  }
  catch (itk::ExceptionObject & error)
  {
    std::cerr << "Error: " << error << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

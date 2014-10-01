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
#include "itkButterworthFilterFreqImageSource.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkForwardFFTImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkComplexToRealImageAdaptor.h"
#include "itkComposeImageFilter.h"
#include "itkComplexToImaginaryImageFilter.h"
#include "itkInverseFFTImageFilter.h"

int
itkButterworthFilterFreqImageSourceTest(int argc, char * argv[])
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

  typedef float                                   PixelType;
  typedef itk::Image<PixelType, Dimension>        ImageType;
  typedef std::complex<PixelType>                 ComplexPixelType;
  typedef itk::Image<ComplexPixelType, Dimension> ComplexImageType;

  typedef itk::ImageFileReader<ImageType> ReaderType;
  ReaderType::Pointer                     reader = ReaderType::New();
  reader->SetFileName(inputImageFileName);

  typedef itk::ForwardFFTImageFilter<ImageType, ComplexImageType> FFTFilterType;
  FFTFilterType::Pointer                                          fftFilter = FFTFilterType::New();
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


  typedef itk::ComplexToRealImageAdaptor<ComplexImageType, PixelType> ComplexToRealType;
  ComplexToRealType::Pointer                                          complexToReal = ComplexToRealType::New();
  complexToReal->SetImage(fftFilter->GetOutput());

  typedef itk::ButterworthFilterFreqImageSource<ImageType> ButterworthSourceType;
  ButterworthSourceType::Pointer                           butterworthSource = ButterworthSourceType::New();
  ImageType::ConstPointer                                  inputImage = reader->GetOutput();
  butterworthSource->SetSize(inputImage->GetLargestPossibleRegion().GetSize());
  butterworthSource->SetSpacing(inputImage->GetSpacing());
  butterworthSource->SetDirection(inputImage->GetDirection());
  butterworthSource->SetOrigin(inputImage->GetOrigin());
  std::cout << butterworthSource << std::endl;

  typedef itk::MultiplyImageFilter<ComplexToRealType, ImageType, ImageType> MultiplyFilterType;
  MultiplyFilterType::Pointer                                               multiplyFilter = MultiplyFilterType::New();
  multiplyFilter->SetInput1(complexToReal);
  multiplyFilter->SetInput2(butterworthSource->GetOutput());

  typedef itk::ComplexToImaginaryImageFilter<ComplexImageType, ImageType> ComplexToImaginaryFilterType;
  ComplexToImaginaryFilterType::Pointer complexToImaginaryFilter = ComplexToImaginaryFilterType::New();
  complexToImaginaryFilter->SetInput(fftFilter->GetOutput());

  typedef itk::ComposeImageFilter<ImageType, ComplexImageType> ComposeFilterType;
  ComposeFilterType::Pointer                                   composeFilter = ComposeFilterType::New();
  composeFilter->SetInput(0, multiplyFilter->GetOutput());
  composeFilter->SetInput(1, complexToImaginaryFilter->GetOutput());

  typedef itk::InverseFFTImageFilter<ComplexImageType, ImageType> InverseFFTFilterType;
  InverseFFTFilterType::Pointer                                   inverseFFTImageFilter = InverseFFTFilterType::New();
  inverseFFTImageFilter->SetInput(composeFilter->GetOutput());

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer                     writer = WriterType::New();
  writer->SetInput(butterworthSource->GetOutput());
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

  typedef itk::ImageFileWriter<ComplexImageType> ComplexWriterType;
  ComplexWriterType::Pointer                     complexWriter = ComplexWriterType::New();
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

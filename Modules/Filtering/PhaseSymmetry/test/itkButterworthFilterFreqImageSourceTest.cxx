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
#include "itkImageFileWriter.h"
#include "itkComplexToComplexFFTImageFilter.h"
#include "itkMultiplyImageFilter.h"

int
itkButterworthFilterFreqImageSourceTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << argv[0] << " OutputImage" << std::endl;
    return EXIT_FAILURE;
  }
  const char * outputImageFileName = argv[1];

  const unsigned int Dimension = 3;

  typedef float                                   PixelType;
  typedef itk::Image<PixelType, Dimension>        ImageType;
  typedef std::complex<PixelType>                 ComplexPixelType;
  typedef itk::Image<ComplexPixelType, Dimension> ComplexImageType;

  ImageType::Pointer   inputImage = ImageType::New();
  ImageType::IndexType inputIndex;
  inputIndex.Fill(0);
  ImageType::SizeType inputSize;
  inputSize.Fill(256);
  ImageType::RegionType inputRegion;
  inputRegion.SetIndex(inputIndex);
  inputRegion.SetSize(inputSize);
  inputImage->SetRegions(inputRegion);
  ImageType::SpacingType inputSpacing;
  inputSpacing.Fill(0.5);
  inputImage->SetSpacing(inputSpacing);
  inputImage->Allocate(true); // Initialize to zero

  ImageType::IndexType index;
  index[0] = 127;
  index[1] = 127;
  inputImage->SetPixel(index, 0.25);
  index[0] = 128;
  inputImage->SetPixel(index, 0.25);
  index[0] = 127;
  index[1] = 128;
  inputImage->SetPixel(index, 0.25);
  index[0] = 128;
  inputImage->SetPixel(index, 0.25);

  typedef itk::ButterworthFilterFreqImageSource<ImageType> ButterworthSourceType;
  ButterworthSourceType::Pointer                           butterworthSource = ButterworthSourceType::New();

  typedef itk::ComplexToComplexFFTImageFilter<ComplexImageType> FFTFilterType;
  FFTFilterType::Pointer                                        fftFilter = FFTFilterType::New();

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer                     writer = WriterType::New();
  writer->SetInput(butterworthSource->GetOutput());
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

  return EXIT_SUCCESS;
}

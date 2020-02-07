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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkTestingMacros.h"

int
itkPromoteDimensionImageTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " inputImage outputImage " << std::endl;
    return -1;
  }

  const char * inputFilename = argv[1];
  const char * outputFilename = argv[2];

  using CharPixelType = unsigned char; // IO
  using RealPixelType = double;        // Operations

  constexpr unsigned int InDimension = 2;
  constexpr unsigned int OutDimension = 3;

  using InCharImageType = itk::Image<CharPixelType, InDimension>;
  using OutCharImageType = itk::Image<CharPixelType, OutDimension>;
  using RealImageType = itk::Image<RealPixelType, InDimension>;

  using ReaderType = itk::ImageFileReader<InCharImageType>;
  using WriterType = itk::ImageFileWriter<OutCharImageType>;

  using CastToRealFilterType = itk::CastImageFilter<InCharImageType, RealImageType>;
  using CastToCharFilterType = itk::CastImageFilter<RealImageType, OutCharImageType>;

  using RescaleFilter = itk::RescaleIntensityImageFilter<RealImageType, RealImageType>;

  // Setting the IO
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  CastToRealFilterType::Pointer toReal = CastToRealFilterType::New();
  CastToCharFilterType::Pointer toChar = CastToCharFilterType::New();
  RescaleFilter::Pointer        rescale = RescaleFilter::New();

  // Setting the ITK pipeline filter

  reader->SetFileName(inputFilename);
  writer->SetFileName(outputFilename);

  // The output of an edge filter is 0 or 1
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);

  toReal->SetInput(reader->GetOutput());
  rescale->SetInput(toReal->GetOutput());
  toChar->SetInput(rescale->GetOutput());
  writer->SetInput(toChar->GetOutput());

  try
  {
    writer->Update();
    // toChar->GetOutput()->Print(std::cout);
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return -1;
  }

  return EXIT_SUCCESS;
}

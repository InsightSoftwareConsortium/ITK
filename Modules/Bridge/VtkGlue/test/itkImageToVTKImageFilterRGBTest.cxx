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

#include "itkImageToVTKImageFilter.h"

#include "itkImageFileReader.h"
#include "itkRGBPixel.h"

int
itkImageToVTKImageFilterRGBTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Usage: ";
    std::cerr << argv[0];
    std::cerr << " <InputFileName>";
    std::cerr << std::endl;

    return EXIT_FAILURE;
  }
  const char * inputFileName = argv[1];

  constexpr unsigned int Dimension = 2;
  using PixelComponentType = unsigned char;
  using PixelType = itk::RGBPixel<PixelComponentType>;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputFileName);

  using ConnectorType = itk::ImageToVTKImageFilter<ImageType>;
  ConnectorType::Pointer connector = ConnectorType::New();
  connector->SetInput(reader->GetOutput());

  connector->UpdateLargestPossibleRegion();

  connector->GetOutput()->Print(std::cout);
  connector->GetImporter()->Print(std::cout);
  connector->GetExporter()->Print(std::cout);

  connector->Print(std::cout);

  connector->Update();

  return EXIT_SUCCESS;
}

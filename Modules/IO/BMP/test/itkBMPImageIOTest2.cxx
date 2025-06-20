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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"
#include <fstream>


// Specific ImageIO test

int
itkBMPImageIOTest2(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " input output" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using ComponentType = unsigned char;

  using PixelType = itk::RGBAPixel<ComponentType>;
  using ImageType = itk::Image<PixelType, Dimension>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  auto reader = ReaderType::New();

  reader->SetFileName(argv[1]);
  reader->UpdateOutputInformation();

  std::cout << "PixelType: " << itk::ImageIOBase::GetPixelTypeAsString(reader->GetImageIO()->GetPixelType())
            << std::endl;
  std::cout << "ComponentType: " << itk::ImageIOBase::GetComponentTypeAsString(reader->GetImageIO()->GetComponentType())
            << std::endl;
  std::cout << "NumberOfComponents: " << reader->GetImageIO()->GetNumberOfComponents() << std::endl;

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  const ImageType::Pointer image = reader->GetOutput();


  image->Print(std::cout);

  const ImageType::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "LargestPossibleRegion " << region;

  // Print the IO
  reader->GetImageIO()->Print(std::cout);

  // Generate test image
  const itk::ImageFileWriter<ImageType>::Pointer writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput(reader->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  // Print the IO
  writer->GetImageIO()->Print(std::cout);

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}

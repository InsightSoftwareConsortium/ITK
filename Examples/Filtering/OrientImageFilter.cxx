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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkOrientImageFilter.h"

int
main(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0]
              << "  inputImageFile outputImageFile "
                 "desiredPositiveCoordinateOrientation"
              << std::endl;
    return EXIT_FAILURE;
  }

  // Get the inputs from the command line in C++ style
  const std::string inputImageFile = argv[1];
  const std::string outputImageFile = argv[2];

  const itk::AnatomicalOrientation desiredCoordinateOrientation{
    itk::AnatomicalOrientation::CreateFromPositiveStringEncoding(argv[3])
  };

  if (desiredCoordinateOrientation ==
      itk::AnatomicalOrientation::PositiveEnum::INVALID)
  {
    std::cerr << "Invalid desiredPositiveCoordinateOrientation: " << argv[3]
              << std::endl;
    std::cerr << "Valid values are of the form LPS, RIP, etc.";
    std::cerr << "Where each letter is either L or R, P or A, I or S."
              << std::endl;
    return EXIT_FAILURE;
  }

  // Define the image and reader types
  using PixelType = unsigned short;
  using ImageType = itk::Image<PixelType, 3>;
  using ReaderType = itk::ImageFileReader<ImageType>;

  // Create and set up a reader
  auto reader = ReaderType::New();
  reader->SetFileName(inputImageFile);

  reader->UpdateOutputInformation();

  auto pixelType = reader->GetImageIO()->GetPixelType();
  auto componentType = reader->GetImageIO()->GetComponentType();

  if (pixelType != itk::IOPixelEnum::SCALAR ||
      componentType != itk::IOComponentEnum::USHORT)
  {
    itkGenericOutputMacro(
      "The input image is being converted to scalar unsigned short.");
  }

  auto inputDirection = reader->GetOutput()->GetDirection();

  std::cout << "Input image direction: " << std::endl;
  inputDirection.PrintSelf(std::cout, itk::Indent(2));

  std::cout << "The input image has the following orientation: " << std::endl;
  std::cout << '\t' << itk::AnatomicalOrientation(inputDirection)
            << std::endl;

  std::cout << "The re-orienting to approximately the desired orientation: "
            << std::endl;
  std::cout << '\t' << desiredCoordinateOrientation << std::endl;


  // Create and set up an OrientImageFilter
  using OrientImageFilterType = itk::OrientImageFilter<ImageType, ImageType>;
  auto orienter = OrientImageFilterType::New();
  orienter->SetInput(reader->GetOutput());
  orienter->UseImageDirectionOn();
  orienter->SetDesiredCoordinateOrientation(desiredCoordinateOrientation);


  // Create and set up a writer
  using WriterType = itk::ImageFileWriter<ImageType>;
  auto writer = WriterType::New();
  writer->SetFileName(outputImageFile);
  writer->SetInput(orienter->GetOutput());

  writer->Update();

  return EXIT_SUCCESS;
}

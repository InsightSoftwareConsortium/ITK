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

#include "itkRGBPixel.h"
#include "itkVectorImage.h"

namespace
{
template <typename TImageType>
int
ReadWrite(const std::string & inputImage, const std::string & outputImage, bool compress)
{
  auto image = itk::ReadImage<TImageType>(inputImage);
  ITK_TRY_EXPECT_NO_EXCEPTION(itk::WriteImage(image, outputImage, compress));
  return EXIT_SUCCESS;
}

template <unsigned int Dimension>
int
internalMain(const std::string &       inputImage,
             const std::string &       outputImage,
             itk::ImageIOBase::Pointer imageIO,
             bool                      compress)
{
  const unsigned int numberOfComponents = imageIO->GetNumberOfComponents();
  using IOPixelType = itk::IOPixelEnum;
  const IOPixelType pixelType = imageIO->GetPixelType();

  switch (pixelType)
  {
    case IOPixelType::SCALAR:
      ITK_TEST_EXPECT_EQUAL(numberOfComponents, 1);
      return ReadWrite<itk::Image<float, Dimension>>(inputImage, outputImage, compress);

    case IOPixelType::RGB:
      ITK_TEST_EXPECT_EQUAL(numberOfComponents, 3);
      return ReadWrite<itk::Image<itk::RGBPixel<unsigned char>, Dimension>>(inputImage, outputImage, compress);

    case IOPixelType::VECTOR:
      return ReadWrite<itk::VectorImage<float, Dimension>>(inputImage, outputImage, compress);

    default:
      std::cerr << "Test does not support pixel type of " << itk::ImageIOBase::GetPixelTypeAsString(pixelType)
                << std::endl;
      return EXIT_FAILURE;
  }
}

} // namespace

int
itkVTIImageIOReadWriteTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " <InputImage> <OutputImage> [compress=false]" << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputImage = argv[1];
  const char * outputImage = argv[2];
  bool         compress = false;
  if (argc > 3)
  {
    compress = std::stoi(argv[3]);
  }

  using ReaderType = itk::ImageFileReader<itk::Image<float, 3>>;
  auto reader = ReaderType::New();
  reader->SetFileName(inputImage);
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->UpdateOutputInformation());

  auto imageIO = reader->GetImageIO();
  imageIO->SetFileName(inputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(imageIO->ReadImageInformation());

  std::cout << imageIO << std::endl;

  const unsigned int dimension = imageIO->GetNumberOfDimensions();

  switch (dimension)
  {
    case 2:
      return internalMain<2>(inputImage, outputImage, imageIO, compress);
    case 3:
      return internalMain<3>(inputImage, outputImage, imageIO, compress);
    default:
      std::cerr << "Test only supports dimensions 2 and 3. Detected dimension " << dimension << std::endl;
      return EXIT_FAILURE;
  }
}

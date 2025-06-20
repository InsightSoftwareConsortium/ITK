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
#include "itkGDCMImageIO.h"

#include "itkRGBPixel.h"
#include "itkVectorImage.h"

namespace
{

template <typename TImageType>
int
ReadWrite(const std::string & inputImage, const std::string & outputImage)
{
  using ImageType = TImageType;

  auto reader = itk::ImageFileReader<ImageType>::New();
  reader->SetFileName(inputImage);

  auto gdcmImageIO = itk::GDCMImageIO::New();
  reader->SetImageIO(gdcmImageIO);
  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  auto image = reader->GetOutput();
  image->GetMetaDataDictionary().Clear();

  auto writer = itk::ImageFileWriter<ImageType>::New();

  writer->SetInput(image);
  writer->SetFileName(outputImage);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());
  return EXIT_SUCCESS;
}

template <unsigned int Dimension>
int
internalMain(const std::string &       inputImage,
             const std::string &       outputImage,
             const std::string &       expectedPixelType,
             itk::GDCMImageIO::Pointer gdcmImageIO)
{
  const unsigned int numberOfComponents = gdcmImageIO->GetNumberOfComponents();
  using IOPixelType = itk::IOPixelEnum;
  const IOPixelType pixelType = gdcmImageIO->GetPixelType();

  switch (pixelType)
  {
    case IOPixelType::SCALAR:
      ITK_TEST_EXPECT_EQUAL(numberOfComponents, 1);
      ITK_TEST_EXPECT_EQUAL(itk::ImageIOBase::GetPixelTypeAsString(pixelType), expectedPixelType);
      return ReadWrite<itk::Image<float, Dimension>>(inputImage, outputImage);

    case IOPixelType::RGB:
      ITK_TEST_EXPECT_EQUAL(numberOfComponents, 3);
      ITK_TEST_EXPECT_EQUAL(itk::ImageIOBase::GetPixelTypeAsString(pixelType), expectedPixelType);
      return ReadWrite<itk::Image<itk::RGBPixel<unsigned char>, Dimension>>(inputImage, outputImage);

    case IOPixelType::VECTOR:
      ITK_TEST_EXPECT_EQUAL(itk::ImageIOBase::GetPixelTypeAsString(pixelType), expectedPixelType);
      return ReadWrite<itk::VectorImage<float, Dimension>>(inputImage, outputImage);

    default:
      std::cerr << "Test does not support pixel type of " << itk::ImageIOBase::GetPixelTypeAsString(pixelType)
                << std::endl;
      return EXIT_FAILURE;
  }
}

} // namespace

int
itkGDCMImageReadWriteTest(int argc, char * argv[])
{
  if (argc < 4)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " InputImage OutputImage expectedPixelType" << std::endl;
    return EXIT_FAILURE;
  }
  const char * inputImage = argv[1];
  const char * outputImage = argv[2];
  const char * expectedPixelType = argv[3];

  auto gdcmImageIO = itk::GDCMImageIO::New();
  gdcmImageIO->SetFileName(inputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(gdcmImageIO->ReadImageInformation());

  std::cout << gdcmImageIO << std::endl;

  const unsigned int dimension = gdcmImageIO->GetNumberOfDimensions();

  switch (dimension)
  {
    case 2:
      return internalMain<2>(inputImage, outputImage, expectedPixelType, gdcmImageIO);
    case 3:
      return internalMain<3>(inputImage, outputImage, expectedPixelType, gdcmImageIO);
    default:
      std::cerr << "Test only supports dimensions 2 and 3. Detected dimension " << dimension << std::endl;
      return EXIT_FAILURE;
  }
}

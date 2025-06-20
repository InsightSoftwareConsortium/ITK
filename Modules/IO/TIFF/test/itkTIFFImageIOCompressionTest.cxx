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
#include "itkRGBPixel.h"
#include "itkRGBAPixel.h"
#include "itkTIFFImageIO.h"
#include "itkTestingMacros.h"
#include <fstream>

// Specific ImageIO test


namespace
{

template <typename TImage>
int
itkTIFFImageIOCompressionTestHelper(int, char * argv[], int JPEGQuality)
{
  using ImageType = TImage;
  using ReaderType = itk::ImageFileReader<ImageType>;
  using WriterType = itk::ImageFileWriter<ImageType>;

  auto reader = ReaderType::New();
  auto writer = WriterType::New();


  const itk::TIFFImageIO::Pointer imageIO = itk::TIFFImageIO::New();
  ITK_TRY_EXPECT_NO_EXCEPTION(imageIO->SetCompressor(""));
  ITK_TEST_EXPECT_EQUAL(imageIO->GetCompressor(), "");

  ITK_TRY_EXPECT_NO_EXCEPTION(imageIO->SetCompressor("JPEG"));
  ITK_TEST_EXPECT_EQUAL(imageIO->GetCompressor(), "JPEG");

  ITK_TRY_EXPECT_NO_EXCEPTION(imageIO->SetCompressor("PackBits"));
  ITK_TEST_EXPECT_EQUAL(imageIO->GetCompressor(), "PackBits");

  ITK_TRY_EXPECT_NO_EXCEPTION(imageIO->SetCompressor("LZW"));
  ITK_TEST_EXPECT_EQUAL(imageIO->GetCompressor(), "LZW");

  ITK_TRY_EXPECT_NO_EXCEPTION(imageIO->SetCompressor("SomethingThatDoesNotExist"));
  ITK_TEST_EXPECT_EQUAL(imageIO->GetCompressor(), "");

  ITK_TRY_EXPECT_NO_EXCEPTION(imageIO->SetCompressor("Deflate"));
  ITK_TEST_EXPECT_EQUAL(imageIO->GetCompressor(), "Deflate");

  ITK_TRY_EXPECT_NO_EXCEPTION(imageIO->SetCompressor("AdobeDeflate"));
  ITK_TEST_EXPECT_EQUAL(imageIO->GetCompressor(), "AdobeDeflate");


  imageIO->SetCompressionLevel(2);
  ITK_TEST_EXPECT_EQUAL(imageIO->GetCompressionLevel(), 2);

  imageIO->SetCompressionLevel(110);
  ITK_TEST_EXPECT_EQUAL(imageIO->GetCompressionLevel(), 100);

  const itk::TIFFImageIO::Pointer io = itk::TIFFImageIO::New();
  reader->SetFileName(argv[1]);
  reader->SetImageIO(io);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());

  const std::string compression = argv[3];

  // Write out a compressed version
  writer->SetInput(reader->GetOutput());
  writer->SetFileName(argv[2]);

  if (compression == "Deflate")
  {

    io->SetCompressionToDeflate();

    ITK_TEST_EXPECT_EQUAL(compression, io->GetCompressor());
    ITK_TEST_EXPECT_TRUE(io->GetUseCompression());
  }
  else if (compression == "AdobeDeflate")
  {
    std::cout << "Using AdobeDeflate\n";
    io->SetCompressionToAdobeDeflate();

    ITK_TEST_EXPECT_EQUAL(compression, io->GetCompressor());
    ITK_TEST_EXPECT_TRUE(io->GetUseCompression());
  }
  else if (compression == "LZW")
  {
    io->SetCompressionToLZW();

    ITK_TEST_EXPECT_EQUAL(compression, io->GetCompressor());
    ITK_TEST_EXPECT_TRUE(io->GetUseCompression());
  }
  else if (compression == "JPEG")
  {

    io->SetCompressionToJPEG();
    io->SetJPEGQuality(JPEGQuality);

    ITK_TEST_EXPECT_EQUAL(compression, io->GetCompressor());
    ITK_TEST_EXPECT_EQUAL(io->GetCompressionLevel(), JPEGQuality);
    ITK_TEST_EXPECT_TRUE(io->GetUseCompression());
  }
  else if (compression == "PackBits")
  {

    io->SetCompressionToPackBits();

    ITK_TEST_EXPECT_EQUAL(compression, io->GetCompressor());
    ITK_TEST_EXPECT_TRUE(io->GetUseCompression());
  }
  else if (compression == "NoCompression")
  {

    io->SetCompressionToNoCompression();

    ITK_TEST_EXPECT_TRUE(!io->GetUseCompression());
  }
  else
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Unknown compression type: " << compression << std::endl;
    return EXIT_FAILURE;
  }

  writer->SetImageIO(io);
  writer->UseCompressionOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());

  return EXIT_SUCCESS;
}
} // namespace

int
itkTIFFImageIOCompressionTest(int argc, char * argv[])
{
  int JPEGQuality = 75;
  if (argc < 4)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputFile"
              << " outputFile compression [JPEGQuality]" << std::endl;
    return EXIT_FAILURE;
  }

  if (argc > 4)
  {
    JPEGQuality = std::stoi(argv[4]);
  }

  const std::string inputFilename = argv[1];

  using ScalarPixelType = itk::IOComponentEnum;

  const itk::TIFFImageIO::Pointer imageIO = itk::TIFFImageIO::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(imageIO, TIFFImageIO, ImageIOBase);

  imageIO->SetFileName(inputFilename);
  imageIO->ReadImageInformation();

  std::cout << "Input Filename: " << inputFilename << std::endl;
  std::cout << "Output Filename: " << argv[2] << std::endl;
  std::cout << "Compression: " << argv[3] << std::endl;
  std::cout << "JPEGQuality: " << JPEGQuality << std::endl;

  std::cout << " Pixel type (string): " << itk::ImageIOBase::GetPixelTypeAsString(imageIO->GetPixelType()) << std::endl;

  const ScalarPixelType componentType = imageIO->GetComponentType();
  std::cout << " Component Type is " << itk::ImageIOBase::GetComponentTypeAsString(componentType) << std::endl;

  std::cout << " Component size: " << imageIO->GetComponentSize() << std::endl;

  const size_t numDimensions = imageIO->GetNumberOfDimensions();
  std::cout << " Number of dimensions: " << numDimensions << std::endl;

  switch (imageIO->GetPixelType())
  {
    case itk::IOPixelEnum::SCALAR:
      switch (componentType)
      {
        case itk::IOComponentEnum::UCHAR:
        {
          using PixelType = unsigned char;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::CHAR:
        {
          using PixelType = char;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::USHORT:
        {
          using PixelType = unsigned short;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::SHORT:
        {
          using PixelType = short;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::FLOAT:
        {
          using PixelType = float;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE:
        default:
          std::cout << "unknown component type" << std::endl;
          break;
      }
      break;
    case itk::IOPixelEnum::RGB:
      switch (componentType)
      {
        case itk::IOComponentEnum::UCHAR:
        {
          using PixelType = itk::RGBPixel<unsigned char>;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::CHAR:
        {
          using PixelType = itk::RGBPixel<char>;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::USHORT:
        {
          using PixelType = itk::RGBPixel<unsigned short>;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::SHORT:
        {
          using PixelType = itk::RGBPixel<short>;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::FLOAT:
        {
          using PixelType = itk::RGBPixel<float>;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE:
        default:
          std::cout << "unknown component type" << std::endl;
          break;
      }
      break;
    case itk::IOPixelEnum::RGBA:
      switch (componentType)
      {
        case itk::IOComponentEnum::UCHAR:
        {
          using PixelType = itk::RGBAPixel<unsigned char>;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::CHAR:
        {
          using PixelType = itk::RGBAPixel<char>;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::USHORT:
        {
          using PixelType = itk::RGBAPixel<unsigned short>;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::SHORT:
        {
          using PixelType = itk::RGBAPixel<short>;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::FLOAT:
        {
          using PixelType = itk::RGBAPixel<float>;
          return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2>>(argc, argv, JPEGQuality);
        }
        case itk::IOComponentEnum::UNKNOWNCOMPONENTTYPE:
        default:
          std::cout << "unknown component type" << std::endl;
          break;
      }
      break;
    default:
      std::cout << "unknown pixel type" << std::endl;
      break;
  }

  return EXIT_FAILURE;
}

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

template< typename TImage >
int itkTIFFImageIOCompressionTestHelper( int, char * argv[], int JPEGQuality )
{
  using ImageType = TImage;
  using ReaderType = itk::ImageFileReader< ImageType >;
  using WriterType = itk::ImageFileWriter< ImageType >;

  typename ReaderType::Pointer reader = ReaderType::New();
  typename WriterType::Pointer writer = WriterType::New();


  itk::TIFFImageIO::Pointer imageIO = itk::TIFFImageIO::New();
  TRY_EXPECT_NO_EXCEPTION( imageIO->SetCompressor("") );
  TEST_EXPECT_EQUAL( imageIO->GetCompressor(), "" );

  TRY_EXPECT_NO_EXCEPTION( imageIO->SetCompressor("JPEG") );
  TEST_EXPECT_EQUAL( imageIO->GetCompressor(), "JPEG" );

  TRY_EXPECT_NO_EXCEPTION( imageIO->SetCompressor("PackBits") );
  TEST_EXPECT_EQUAL( imageIO->GetCompressor(), "PackBits" );

  TRY_EXPECT_NO_EXCEPTION( imageIO->SetCompressor("LZW") );
  TEST_EXPECT_EQUAL( imageIO->GetCompressor(), "LZW" );

  TRY_EXPECT_NO_EXCEPTION( imageIO->SetCompressor("SomethingThatDoesNotExist") );
  TEST_EXPECT_EQUAL( imageIO->GetCompressor(), "" );

  TRY_EXPECT_NO_EXCEPTION( imageIO->SetCompressor("Deflate") );
  TEST_EXPECT_EQUAL( imageIO->GetCompressor(), "Deflate" );

  imageIO->SetCompressionLevel(2);
  TEST_EXPECT_EQUAL( imageIO->GetCompressionLevel(), 2 );

  imageIO->SetCompressionLevel(110);
  TEST_EXPECT_EQUAL( imageIO->GetCompressionLevel(), 100 );

  itk::TIFFImageIO::Pointer io = itk::TIFFImageIO::New();
  reader->SetFileName( argv[1] );
  reader->SetImageIO( io );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );

  std::string compression = argv[3];

  // Write out a compressed version
  writer->SetInput( reader->GetOutput() );
  writer->SetFileName( argv[2] );

  if( compression == "Deflate" )
    {

    io->SetCompressionToDeflate();

    TEST_EXPECT_EQUAL(compression, io->GetCompressor());
    TEST_EXPECT_TRUE(io->GetUseCompression());
    }
  else if( compression == "LZW" )
    {
    io->SetCompressionToLZW();

    TEST_EXPECT_EQUAL(compression, io->GetCompressor());
    TEST_EXPECT_TRUE(io->GetUseCompression());
    }
  else if( compression == "JPEG" )
    {

    io->SetCompressionToJPEG();
    io->SetJPEGQuality( JPEGQuality );

    TEST_EXPECT_EQUAL(compression, io->GetCompressor());
    TEST_EXPECT_EQUAL(io->GetCompressionLevel(), JPEGQuality);
    TEST_EXPECT_TRUE(io->GetUseCompression());
    }
  else if( compression == "PackBits" )
    {

    io->SetCompressionToPackBits();

    TEST_EXPECT_EQUAL(compression, io->GetCompressor());
    TEST_EXPECT_TRUE(io->GetUseCompression());
    }
  else if( compression == "NoCompression" )
    {

    io->SetCompressionToNoCompression();

    TEST_EXPECT_TRUE(!io->GetUseCompression());
    }
  else
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Unknown compression type: " << compression << std::endl;
    return EXIT_FAILURE;
    }

  writer->SetImageIO( io );
  writer->UseCompressionOn();

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
}

int itkTIFFImageIOCompressionTest( int argc, char* argv[] )
{
  int JPEGQuality = 75;
  if (argc < 4 )
    {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
      << " inputFile"
      << " outputFile"
      << "compression"
      << "[JPEGQuality]"
      << std::endl;
    return EXIT_FAILURE;
    }

  if( argc > 4 )
    {
    JPEGQuality = std::stoi( argv[4] );
    }

  std::string inputFilename = argv[1];

  using ScalarPixelType = itk::ImageIOBase::IOComponentType;

  itk::TIFFImageIO::Pointer imageIO = itk::TIFFImageIO::New();

  EXERCISE_BASIC_OBJECT_METHODS( imageIO, TIFFImageIO, ImageIOBase );

  imageIO->SetFileName( inputFilename );
  imageIO->ReadImageInformation();

  std::cout << "Input Filename: " << inputFilename << std::endl;
  std::cout << "Output Filename: " << argv[2] << std::endl;
  std::cout << "Compression: " << argv[3] << std::endl;
  std::cout << "JPEGQuality: " << JPEGQuality << std::endl;

  std::cout << " Pixel type (string): "
    << imageIO->GetPixelTypeAsString( imageIO->GetPixelType() ) << std::endl;

  const ScalarPixelType componentType = imageIO->GetComponentType();
  std::cout << " Component Type is "
    << imageIO->GetComponentTypeAsString( componentType ) << std::endl;

  std::cout << " Component size: " << imageIO->GetComponentSize() << std::endl;

  const size_t numDimensions = imageIO->GetNumberOfDimensions();
  std::cout << " Number of dimensions: " << numDimensions << std::endl;

  switch( imageIO->GetPixelType() )
    {
    case itk::ImageIOBase::SCALAR:
      switch( componentType )
        {
        case itk::ImageIOBase::UCHAR:
        {
        using PixelType = unsigned char;
        return itkTIFFImageIOCompressionTestHelper< itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::CHAR:
        {
        using PixelType = char;
        return itkTIFFImageIOCompressionTestHelper< itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::USHORT:
        {
        using PixelType = unsigned short;
        return itkTIFFImageIOCompressionTestHelper< itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::SHORT:
        {
        using PixelType = short;
        return itkTIFFImageIOCompressionTestHelper< itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::FLOAT:
        {
        using PixelType = float;
        return itkTIFFImageIOCompressionTestHelper< itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::UNKNOWNCOMPONENTTYPE:
        default:
          std::cout << "unknown component type" << std::endl;
          break;
        }
      break;
    case itk::ImageIOBase::RGB:
      switch( componentType )
        {
        case itk::ImageIOBase::UCHAR:
        {
        using PixelType = itk::RGBPixel< unsigned char >;
        return itkTIFFImageIOCompressionTestHelper< itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::CHAR:
        {
        using PixelType = itk::RGBPixel< char >;
        return itkTIFFImageIOCompressionTestHelper< itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::USHORT:
        {
        using PixelType = itk::RGBPixel< unsigned short >;
        return itkTIFFImageIOCompressionTestHelper< itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::SHORT:
        {
        using PixelType = itk::RGBPixel<short>;
        return itkTIFFImageIOCompressionTestHelper< itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::FLOAT:
        {
        using PixelType = itk::RGBPixel< float >;
        return itkTIFFImageIOCompressionTestHelper< itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::UNKNOWNCOMPONENTTYPE:
        default:
          std::cout << "unknown component type" << std::endl;
          break;
        }
      break;
    case itk::ImageIOBase::RGBA:
      switch( componentType )
        {
        case itk::ImageIOBase::UCHAR:
        {
        using PixelType = itk::RGBAPixel< unsigned char >;
        return itkTIFFImageIOCompressionTestHelper< itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::CHAR:
        {
        using PixelType = itk::RGBAPixel< char >;
        return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::USHORT:
        {
        using PixelType = itk::RGBAPixel< unsigned short >;
        return itkTIFFImageIOCompressionTestHelper<itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::SHORT:
        {
        using PixelType = itk::RGBAPixel< short >;
        return itkTIFFImageIOCompressionTestHelper< itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::FLOAT:
        {
        using PixelType = itk::RGBAPixel< float >;
        return itkTIFFImageIOCompressionTestHelper< itk::Image<PixelType, 2> >( argc, argv, JPEGQuality );
        }
        case itk::ImageIOBase::UNKNOWNCOMPONENTTYPE:
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

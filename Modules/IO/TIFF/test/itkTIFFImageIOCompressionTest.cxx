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
#include <fstream>
#include "itkTIFFImageIO.h"

#define SPECIFIC_IMAGEIO_MODULE_TEST
namespace
{
template<typename T> int DoIt( int, char * argv[], typename T::Pointer)
{
  typename itk::ImageFileReader<T>::Pointer reader
    = itk::ImageFileReader<T>::New();
  typename itk::ImageFileWriter<T>::Pointer writer
    = itk::ImageFileWriter<T>::New();

  itk::TIFFImageIO::Pointer io = itk::TIFFImageIO::New();
  reader->SetFileName(argv[1]);
  reader->SetImageIO(io);

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  std::string compression = argv[3];

  // Write out a compressed version
  writer->SetInput( reader->GetOutput() );
  writer->SetFileName(argv[2]);
  if (compression == "Deflate")
    {
    io->SetCompressionToDeflate();
    }
  else if (compression == "LZW")
    {
    io->SetCompressionToLZW();
    }
  else if (compression == "JPEG")
    {
    io->SetCompressionToJPEG();
    }
  else if (compression == "PackBits")
    {
    io->SetCompressionToPackBits();
    }
  else if (compression == "NoCompression")
    {
    io->SetCompressionToNoCompression();
    }
  else
    {
    std::cerr << "ERROR: Unknown compression: " << compression << std::endl;
    return EXIT_FAILURE;
    }

  writer->SetImageIO(io);
  writer->UseCompressionOn();
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "Unexpected exception in ImageFileWriter " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}
}
int itkTIFFImageIOCompressionTest( int argc, char* argv[] )
{
  if (argc < 4 )
    {
    std::cerr << "Usage: " << argv[0]
              << " InputFile OutputFile compression" << std::endl;
    return EXIT_FAILURE;
    }
  std::string inputFilename = argv[1];

  typedef itk::ImageIOBase::IOComponentType ScalarPixelType;

  itk::TIFFImageIO::Pointer imageIO =
    itk::TIFFImageIO::New();

  imageIO->SetFileName(inputFilename);
  imageIO->ReadImageInformation();

  std::cout << "Input Filename: " << inputFilename << std::endl;
  std::cout << "Output Filename: " << argv[2] << std::endl;
  std::cout << "Compression: " << argv[3] << std::endl;

  std::cout << "  Pixel type (string): "
            << imageIO->GetPixelTypeAsString(imageIO->GetPixelType())
            << std::endl;

  const ScalarPixelType componentType = imageIO->GetComponentType();
  std::cout << "  Component Type is "
            << imageIO->GetComponentTypeAsString(componentType)
            << std::endl;

  std::cout << "  Component size: "
            << imageIO->GetComponentSize()
            << std::endl;

  const size_t numDimensions =  imageIO->GetNumberOfDimensions();
  std::cout << "  Number of dimensions: "
            << numDimensions
            << std::endl;

  switch( imageIO->GetPixelType() )
    {
    case itk::ImageIOBase::SCALAR:
      switch( componentType )
        {
        case itk::ImageIOBase::UCHAR:
        {
        typedef unsigned char PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
        }
        case itk::ImageIOBase::CHAR:
        {
        typedef char PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
        }
        case itk::ImageIOBase::USHORT:
        {
        typedef unsigned short PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
        }
        case itk::ImageIOBase::SHORT:
        {
        typedef short PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
        }
        case itk::ImageIOBase::FLOAT:
        {
        typedef float PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
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
        typedef itk::RGBPixel<unsigned char> PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
        }
        case itk::ImageIOBase::CHAR:
        {
        typedef itk::RGBPixel<char> PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
        }
        case itk::ImageIOBase::USHORT:
        {
        typedef itk::RGBPixel<unsigned short> PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
        }
        case itk::ImageIOBase::SHORT:
        {
        typedef itk::RGBPixel<short> PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
        }
        case itk::ImageIOBase::FLOAT:
        {
        typedef itk::RGBPixel<float> PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
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
        typedef itk::RGBAPixel<unsigned char> PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
        }
        case itk::ImageIOBase::CHAR:
        {
        typedef itk::RGBAPixel<char> PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
        }
        case itk::ImageIOBase::USHORT:
        {
        typedef itk::RGBAPixel<unsigned short> PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
        }
        case itk::ImageIOBase::SHORT:
        {
        typedef itk::RGBAPixel<short> PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
        }
        case itk::ImageIOBase::FLOAT:
        {
        typedef itk::RGBAPixel<float> PixelType;
        itk::Image<PixelType, 2>::Pointer dummy;
        return DoIt<itk::Image<PixelType, 2> >( argc, argv, dummy );
        break;
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

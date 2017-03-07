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

#include <iostream>
#include <algorithm>
#include "itkPNGImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"


#define SPECIFIC_IMAGEIO_MODULE_TEST

namespace
{
int CompareExtensions(itk::ImageIOBase::ArrayOfExtensionsType& a1, itk::ImageIOBase::ArrayOfExtensionsType& a2)
{
  std::sort( a1.begin(), a1.end() );
  std::sort( a2.begin(), a2.end() );
  if( a1 == a2 )
    {
    return EXIT_SUCCESS;
    }
  else
    {
    return EXIT_FAILURE;
    }
}
} // end anonymous namespace

int itkPNGImageIOTest2( int argc, char * argv[] )
{
  // Test the reading of an image as grayscale image and writing of grayscale image
  if( argc < 5 )
    {
    std::cerr << "Usage: " << argv[0]
      << " input"
      << " output"
      << " useCompression"
      << " compressionLevel"
      << " expandRGBPalette" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int                          Dimension = 2;
  typedef unsigned char                       PixelType;

  // We are converting read data into grayscale pixel image
  typedef itk::Image< PixelType, Dimension >  ImageType;
  typedef itk::ImageFileReader< ImageType >   ReaderType;
  typedef itk::ImageFileWriter< ImageType >   WriterType;


  // Read the input image
  itk::PNGImageIO::Pointer io = itk::PNGImageIO::New();

  EXERCISE_BASIC_OBJECT_METHODS( io, PNGImageIO, ImageIOBase );

  // Exercise exception cases
  size_t sizeOfActualIORegion = io->GetIORegion().GetNumberOfPixels() *
    ( io->GetComponentSize() * io->GetNumberOfComponents() );
  char *loadBuffer = new char[sizeOfActualIORegion];

  TRY_EXPECT_EXCEPTION( io->Read( loadBuffer ) );


  bool useCompression = static_cast< bool >( argv[3] );
  TEST_SET_GET_BOOLEAN( io, UseCompression, useCompression );

  int compressionLevel = atoi( argv[4] );
  io->SetCompressionLevel( compressionLevel );
  TEST_SET_GET_VALUE( compressionLevel, io->GetCompressionLevel() );

  bool expandRGBPalette = static_cast< bool >( atoi( argv[5] ) );
  TEST_SET_GET_BOOLEAN( io, ExpandRGBPalette, expandRGBPalette );

  if( io->CanReadFile( "" ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "No filename specified." << std::endl;
    std::cout << "CanReadFile: "
      << "Expected false but got true" << std::endl;
    }

  if( io->CanStreamRead() )
    {
    std::cout << "itk::PNGImageIO can stream read" << std::endl;
    }
  else
    {
    std::cout << "itk::PNGImageIO cannot stream read" << std::endl;
    }

  // Check supported file extensions
  // Expecting ".png" and ".PNG"
  itk::ImageIOBase::ArrayOfExtensionsType expectedExtensions;
  expectedExtensions.push_back( ".png" );
  expectedExtensions.push_back( ".PNG" );

  // Read extensions
  itk::ImageIOBase::ArrayOfExtensionsType readExtensions = io->GetSupportedReadExtensions();
  if( CompareExtensions( readExtensions, expectedExtensions ) )
    {
    std::cout << "Test failed!" << std::endl;
    std::cerr << "Unexpected list of supported read extension." << std::endl;
    return EXIT_FAILURE;
    }
  // Write extensions
  itk::ImageIOBase::ArrayOfExtensionsType writeExtensions = io->GetSupportedWriteExtensions();
  if( CompareExtensions( writeExtensions, expectedExtensions) )
    {
    std::cout << "Test failed!" << std::endl;
    std::cerr << "Unexpected list of supported write extension." << std::endl;
    return EXIT_FAILURE;
    }

  if( !io->SupportsDimension( Dimension ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "itk::PNGImageIO does not support dimension: " << Dimension << std::endl;
    return EXIT_FAILURE;
    }

  if( !io->CanReadFile( argv[1] ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "itk::PNGImageIO cannot read file "
      << io->GetFileName() << std::endl;
    return EXIT_FAILURE;
    }

  // Actually reading an RGBA image
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->SetImageIO( io );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );

  if( io->GetExpandRGBPalette() )
    {
      std::cout << "If palette image, expanding to RGB. " << std::endl;
    }
  else
    {
      std::cout << "If palette image, trying to read as scalar. " << std::endl;
    }

  if( !io->GetExpandRGBPalette() && io->GetIsReadAsScalarPlusPalette() )
    {
    std::cout << "Image read as Scalar." << std::endl;
    itk::PNGImageIO::PaletteType palette = io->GetColorPalette();
    std::cout << "PaletteType: " << std::endl;
    for( unsigned int i = 0; i < palette.size(); ++i )
      {
      std::cout << "[" << i << "]:" << palette[i] << std::endl;
      }
    }
  else
    {
      std::cout << "Image read as grayscale." << std::endl;
    }


  // Exercise other methods
  itk::ImageIOBase::SizeType pixelStride = io->GetPixelStride();
  std::cout << "PixelStride: "
    << itk::NumericTraits< itk::ImageIOBase::SizeType >::PrintType( pixelStride )
    << std::endl;


  ImageType::Pointer inputImage = reader->GetOutput();

  // Write the grayscale output image
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( inputImage );
  writer->SetImageIO( io );
  writer->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  // Release memory
  delete[] loadBuffer;

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}

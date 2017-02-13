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
#include "itkBMPImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageRegionConstIterator.h"
#include "itkTestingMacros.h"
#include <fstream>


#define SPECIFIC_IMAGEIO_MODULE_TEST

/* This test checks that a RLE-compressed bitmap and an
 * uncompressed bitmap representing the same grayscale image
 * contains the same data.
 */
int itkBMPImageIOTest5( int argc, char* argv[] )
{

  if( argc != 3 )
    {
    std::cerr << "Usage: " << argv[0]
      << " compressedImage"
      << " uncompressedImage"
      << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int    Dimension = 2;
  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, Dimension >          ImageType;
  typedef itk::ImageFileReader< ImageType >           ReaderType;
  typedef itk::ImageRegionConstIterator< ImageType >  IteratorType;


  ReaderType::Pointer compressedImageReader = ReaderType::New();

  itk::BMPImageIO::Pointer compressedImageIO = itk::BMPImageIO::New();

  EXERCISE_BASIC_OBJECT_METHODS( compressedImageIO, BMPImageIO, ImageIOBase );

  compressedImageReader->SetImageIO( compressedImageIO );
  compressedImageReader->SetFileName( argv[1] );
  compressedImageIO->SetFileName( argv[1] );

  ReaderType::Pointer uncompressedImageReader = ReaderType::New();

  itk::BMPImageIO::Pointer uncompressedImageIO = itk::BMPImageIO::New();

  EXERCISE_BASIC_OBJECT_METHODS( uncompressedImageIO, BMPImageIO, ImageIOBase );

  uncompressedImageReader->SetImageIO( uncompressedImageIO );
  uncompressedImageReader->SetFileName( argv[2] );
  uncompressedImageIO->SetFileName( argv[2] );

  if( compressedImageIO->CanReadFile( "" ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "No filename specified." << std::endl;
    std::cout << "CanReadFile: "
      << "Expected false but got true" << std::endl;
    return EXIT_FAILURE;
    }

  if( !compressedImageIO->CanReadFile( compressedImageIO->GetFileName() ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "itk::BMPImageIO cannot read file "
      << uncompressedImageIO->GetFileName() << std::endl;
    return EXIT_FAILURE;
    }

  if( uncompressedImageIO->CanReadFile( "" ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "No filename specified." << std::endl;
    std::cout << "CanReadFile: "
      << "Expected false but got true" << std::endl;
    return EXIT_FAILURE;
    }

  if( !uncompressedImageIO->CanReadFile( uncompressedImageIO->GetFileName() ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "itk::BMPImageIO cannot read file "
      << uncompressedImageIO->GetFileName() << std::endl;
    return EXIT_FAILURE;
    }

  TRY_EXPECT_NO_EXCEPTION( compressedImageReader->Update() );

  TRY_EXPECT_NO_EXCEPTION( uncompressedImageReader->Update() );


  if( compressedImageIO->GetBMPCompression() != 1 )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Expecting a RLE-compressed image, got an uncompressed one ("
      << argv[1] << ")." << std::endl;
    return EXIT_FAILURE;
    }

  if( uncompressedImageIO->GetBMPCompression() != 0 )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Expecting an uncompressed image, got an RLE-compressed one ("
      << argv[2] << ")." << std::endl;
    return EXIT_FAILURE;
    }


  ImageType::RegionType compressedImageRegion =
    compressedImageReader->GetOutput()->GetLargestPossibleRegion();
  ImageType::RegionType uncompressedImageRegion =
    uncompressedImageReader->GetOutput()->GetLargestPossibleRegion();

  if( compressedImageRegion != uncompressedImageRegion )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "The images must have the same size." << std::endl;
    return EXIT_FAILURE;
    }

  IteratorType it1( compressedImageReader->GetOutput(),
    compressedImageRegion );
  IteratorType it2( uncompressedImageReader->GetOutput(),
    uncompressedImageRegion );

  it1.GoToBegin();
  it2.GoToBegin();
  while( !it1.IsAtEnd() )
    {
    if( it1.Value() != it2.Value() )
      {
      std::cout << "Test failed!" << std::endl;
      std::cout << "An image stored in a lower-left bitmap is different than \
                   the same image stored in a upper-left bitmap." << std::endl;
      return EXIT_FAILURE;
      }

    ++it1;
    ++it2;
    }

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}

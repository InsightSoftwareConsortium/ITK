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
#include "itkRGBPixel.h"
#include "itkImageFileReader.h"
#include "itkImageRegionConstIterator.h"
#include "itkTestingMacros.h"
#include <fstream>


#define SPECIFIC_IMAGEIO_MODULE_TEST

/* This test checks that a lower-left bitmap and an upper-left bitmap
 * representing the same RGB image contain the same data.
 */
int itkBMPImageIOTest4( int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " lowerLeftImage upperLeftImage" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int    Dimension = 2;
  typedef unsigned char ComponentType;

  typedef itk::RGBPixel< ComponentType >              PixelType;
  typedef itk::Image< PixelType, Dimension >          ImageType;
  typedef itk::ImageFileReader< ImageType >           ReaderType;
  typedef itk::ImageRegionConstIterator< ImageType >  IteratorType;


  ReaderType::Pointer lowerLeftImageReader = ReaderType::New();

  itk::BMPImageIO::Pointer lowerLeftImageIO = itk::BMPImageIO::New();

  EXERCISE_BASIC_OBJECT_METHODS( lowerLeftImageIO, BMPImageIO, ImageIOBase );

  lowerLeftImageReader->SetImageIO( lowerLeftImageIO );
  lowerLeftImageReader->SetFileName( argv[1] );

  ReaderType::Pointer upperLeftImageReader = ReaderType::New();

  itk::BMPImageIO::Pointer upperLeftImageIO = itk::BMPImageIO::New();

  EXERCISE_BASIC_OBJECT_METHODS( lowerLeftImageIO, BMPImageIO, ImageIOBase );

  upperLeftImageReader->SetImageIO( upperLeftImageIO );
  upperLeftImageReader->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( lowerLeftImageReader->Update() );

  TRY_EXPECT_NO_EXCEPTION( upperLeftImageReader->Update() );


  if( !lowerLeftImageIO->GetFileLowerLeft() )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Expecting a lower-left bitmap, got an upper-left." << std::endl;
    return EXIT_FAILURE;
    }

  if( upperLeftImageIO->GetFileLowerLeft() )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Expecting an upper-left bitmap, got a lower-left." << std::endl;
    return EXIT_FAILURE;
    }


  ImageType::RegionType lowerLeftImageRegion =
    lowerLeftImageReader->GetOutput()->GetLargestPossibleRegion();
  ImageType::RegionType upperLeftImageRegion =
    upperLeftImageReader->GetOutput()->GetLargestPossibleRegion();

  if( lowerLeftImageRegion != upperLeftImageRegion )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "The images must have the same size." << std::endl;
    return EXIT_FAILURE;
    }

  IteratorType it1( lowerLeftImageReader->GetOutput(), lowerLeftImageRegion );
  IteratorType it2( upperLeftImageReader->GetOutput(), upperLeftImageRegion );

  it1.GoToBegin();
  it2.GoToBegin();
  while( !it1.IsAtEnd() )
    {
    if( !(it1.Value() == it2.Value()) )
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

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
#include "itkMath.h"
#include "itkTestingMacros.h"

int itkTIFFImageIOTest2( int argc, char* argv[] )
{

  if( argc != 2 )
    {
    std::cerr << "Usage: " << argv[0] << " outputFilename" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int                          Dimension = 2;
  typedef unsigned char                       PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  ImageType::Pointer image = ImageType::New();

  ImageType::RegionType region;
  ImageType::IndexType  start;
  ImageType::SizeType   size;

  size[0] = 157;
  size[1] = 129;

  start[0] = 0;
  start[1] = 0;

  region.SetSize( size );
  region.SetIndex( start );

  image->SetRegions( region );
  image->Allocate( true ); // initialize buffer to zero

  ImageType::SpacingType spacing;

  spacing[0] = 3.1415;
  spacing[1] = 6.2830;

  image->SetSpacing( spacing );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[1] );

  writer->SetInput( image );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );

  const ImageType * readImage = reader->GetOutput();

  ImageType::SpacingType readSpacing = readImage->GetSpacing();

  const double tolerance = 1e-5;

  for( unsigned int i = 0; i < ImageType::SpacingType::Dimension; ++i )
    {
    if( !itk::Math::FloatAlmostEqual( spacing[i], readSpacing[i], 10, tolerance ) )
      {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error while testing spacing at index: " << i << std::endl;
      std::cerr << "Expected: " << spacing[i] << ", but got: " << readSpacing[i] << std::endl;
      return EXIT_FAILURE;
      }
    }

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}

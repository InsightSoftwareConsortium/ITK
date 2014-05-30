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

int itkJPEGImageIOTest2( int argc, char* argv[] )
{

  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " outputFilename " << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;
  typedef unsigned char PixelType;

  typedef itk::Image< PixelType, Dimension > ImageType;

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
  image->Allocate(true); // initialize buffer
                                                // to zero

  ImageType::SpacingType spacing;

  spacing[0] = 3.1415;
  spacing[1] = 6.2830;

  image->SetSpacing( spacing );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[1] );

  writer->SetInput( image );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  const ImageType * readImage = reader->GetOutput();

  ImageType::SpacingType readSpacing = readImage->GetSpacing();

  const double tolerance = 1e-1;

  if( std::abs( readSpacing[0] - spacing[0] ) > tolerance )
    {
    std::cerr << "Spacing read/write failed !" << std::endl;
    std::cerr << "Expected spacing = " << spacing << std::endl;
    std::cerr << "Found    spacing = " << readSpacing << std::endl;
    return EXIT_FAILURE;
    }

  if( std::abs( readSpacing[1] - spacing[1] ) > tolerance )
    {
    std::cerr << "Spacing read/write failed !" << std::endl;
    std::cerr << "Expected spacing = " << spacing << std::endl;
    std::cerr << "Found    spacing = " << readSpacing << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Expected spacing = " << spacing << std::endl;
  std::cout << "Found    spacing = " << readSpacing << std::endl;
  std::cout << "Test PASSED !" << std::endl;

  return EXIT_SUCCESS;
}

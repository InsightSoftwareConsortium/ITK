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
#include "itkPNGImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkPNGImageIOTest( int argc, char * argv[] )
{
  // Test the reading of an image as RGB image and writing of RGB image

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

  const unsigned long                         Dimension = 2;
  typedef unsigned char                       PixelType;

  // We are converting read data into RGB pixel image
  typedef itk::RGBPixel< PixelType >            RGBPixelType;
  typedef itk::Image< RGBPixelType, Dimension > RGBImageType;


  // Read in the image
  itk::PNGImageIO::Pointer io = itk::PNGImageIO::New();

  EXERCISE_BASIC_OBJECT_METHODS( io, PNGImageIO, ImageIOBase );


  // Exercise exception cases
  size_t sizeOfActualIORegion = io->GetIORegion().GetNumberOfPixels() *
    ( io->GetComponentSize() * io->GetNumberOfComponents() );
  char *loadBuffer = new char[sizeOfActualIORegion];

  TRY_EXPECT_EXCEPTION( io->Read( loadBuffer ) );


  bool useCompression = static_cast< bool >( atoi( argv[3] ) );
  TEST_SET_GET_BOOLEAN( io, UseCompression, useCompression );

  int compressionLevel = atoi( argv[4] );
  io->SetCompressionLevel( compressionLevel );
  TEST_SET_GET_VALUE( compressionLevel, io->GetCompressionLevel() );

  bool expandRGBPalette = static_cast< bool >( argv[5] );
  TEST_SET_GET_BOOLEAN( io, ExpandRGBPalette, expandRGBPalette );

  if( io->CanReadFile( "" ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "No filename specified." << std::endl;
    std::cout << "CanReadFile: "
      << "Expected false but got true" << std::endl;
    return EXIT_FAILURE;
    }

  if( !io->SupportsDimension( Dimension ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "itk::PNGImageIO does not support dimension: " << Dimension << std::endl;
    return EXIT_FAILURE;
    }

  if( io->CanStreamRead() )
    {
    std::cout << "itk::PNGImageIO can stream read" << std::endl;
    }
  else
    {
    std::cout << "itk::PNGImageIO cannot stream read" << std::endl;
    }


  if( !io->CanReadFile( argv[1] ) )
    {
    std::cerr << "Test failed!" << std::endl;
    std::cout << "itk::PNGImageIO cannot read file "
      << io->GetFileName() << std::endl;
    return EXIT_FAILURE;
    }
  itk::ImageFileReader< RGBImageType >::Pointer reader =
    itk::ImageFileReader< RGBImageType >::New();
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
    std::cout << "Palette: " << std::endl;
    for( unsigned int i = 0; i < palette.size(); ++i )
      {
      std::cout << "[" << i << "]:" << palette[i] << std::endl;
      }
    }
  else
    {
      std::cout << "Image read as RGB." << std::endl;
    }

  // Try writing
  itk::ImageFileWriter< RGBImageType >::Pointer writer =
    itk::ImageFileWriter< RGBImageType >::New();
  writer->SetInput( reader->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->SetImageIO( io );

  TRY_EXPECT_NO_EXCEPTION( writer->Write() );


  // Release memory
  delete[] loadBuffer;

  // Exercise other methods
  itk::ImageIOBase::SizeType pixelStride = io->GetPixelStride();
  std::cout << "PixelStride: "
    << itk::NumericTraits< itk::ImageIOBase::SizeType >::PrintType( pixelStride )
    << std::endl;

  //
  // Try writing out several kinds of images using png.
  // The images to test are as follows:
  // - 3D non-degenerate volume: this covers all images greater than or
  // equal to 3D. The writer should write out the first slice.
  // - 3D degenerate volume: The writer should write out the first
  // slice.
  // - 2D image: The writer should write it out correctly.
  // - 2D degenerate image: The writer should write out the image.
  // - 1D image: The writer should write it out as a 2D image.
  //

  typedef itk::Image< unsigned short, 3 > ImageType3D;
  typedef itk::Image< unsigned short, 2 > ImageType2D;
  typedef itk::Image< unsigned short, 1 > ImageType1D;

  //
  // 3D non-degenerate volume
  //

  ImageType3D::Pointer volume = ImageType3D::New();

  ImageType3D::SizeType size3D;
  size3D.Fill( 10 );
  ImageType3D::IndexType start3D;
  start3D.Fill( 0 );
  ImageType3D::RegionType region3D;
  region3D.SetSize( size3D );
  region3D.SetIndex( start3D );

  volume->SetRegions( region3D );
  volume->Allocate();
  volume->FillBuffer( 0 );

  typedef itk::ImageFileWriter< ImageType3D > WriterType3D;
  WriterType3D::Pointer writer3D = WriterType3D::New();
  writer3D->SetFileName( argv[2] );
  writer3D->SetImageIO( io );

  writer3D->SetInput( volume );

  TRY_EXPECT_NO_EXCEPTION( writer3D->Update() );


  //
  // 3D degenerate volume
  //

  ImageType3D::Pointer degenerateVolume = ImageType3D::New();
  // Collapse the first dimension.
  size3D[0] = 1;
  region3D.SetSize( size3D );
  degenerateVolume->SetRegions( region3D );
  degenerateVolume->Allocate();
  degenerateVolume->FillBuffer( 0 );

  writer3D->SetFileName( argv[2] );
  writer3D->SetImageIO( io );
  writer3D->SetInput( degenerateVolume );

  TRY_EXPECT_NO_EXCEPTION( writer3D->Update() );


  //
  // 2D non-degenerate volume
  //
  ImageType2D::Pointer image = ImageType2D::New();

  ImageType2D::SizeType size2D;
  size2D.Fill( 10 );
  ImageType2D::IndexType start2D;
  start2D.Fill( 0 );
  ImageType2D::RegionType region2D;
  region2D.SetSize( size2D );
  region2D.SetIndex( start2D );

  image->SetRegions( region2D );
  image->Allocate();
  image->FillBuffer( 0 );

  typedef itk::ImageFileWriter< ImageType2D > WriterType2D;
  WriterType2D::Pointer writer2D = WriterType2D::New();

  writer2D->SetFileName( argv[2] );
  writer2D->SetImageIO(io);
  writer2D->SetInput( image );

  TRY_EXPECT_NO_EXCEPTION( writer2D->Update() );


  //
  // 2D degenerate volume
  //
  ImageType2D::Pointer degenerateImage = ImageType2D::New();

  // Collapse the first dimension
  size2D[0] = 1;
  region2D.SetSize( size2D );
  degenerateImage->SetRegions( region2D );
  degenerateImage->Allocate();
  degenerateImage->FillBuffer( 0 );

  writer2D->SetFileName( argv[2] );
  writer2D->SetImageIO( io );
  writer2D->SetInput( degenerateImage );

  TRY_EXPECT_NO_EXCEPTION( writer2D->Update() );

  //
  // 1D image
  //
  ImageType1D::Pointer line = ImageType1D::New();

  ImageType1D::SizeType size1D;
  size1D.Fill( 10 );
  ImageType1D::IndexType start1D;
  start1D.Fill( 0 );
  ImageType1D::RegionType region1D;
  region1D.SetSize( size1D );
  region1D.SetIndex( start1D );
  line->SetRegions( region1D );

  line->Allocate();
  line->FillBuffer( 0 );

  typedef itk::ImageFileWriter< ImageType1D > WriterType1D;
  WriterType1D::Pointer writer1D = WriterType1D::New();
  writer1D->SetFileName( argv[2] );
  writer1D->SetImageIO( io );
  writer1D->SetInput( line );

  TRY_EXPECT_NO_EXCEPTION( writer1D->Update() );


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}

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

#define SPECIFIC_IMAGEIO_MODULE_TEST

int itkPNGImageIOTest(int argc, char * argv[])
{
  // This test is usually run with the data file
  // Insight/Testing/Data/Input/cthead1.png
  if( argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " input output\n";
    return EXIT_FAILURE;
    }

  // We are converting read data into RGB pixel image
  typedef itk::RGBPixel<unsigned char> RGBPixelType;
  typedef itk::Image<RGBPixelType,2>   RGBImageType;

  // Read in the image
  itk::PNGImageIO::Pointer io;
  io = itk::PNGImageIO::New();

  itk::ImageFileReader<RGBImageType>::Pointer reader;
  reader = itk::ImageFileReader<RGBImageType>::New();
  reader->SetFileName(argv[1]);
  reader->SetImageIO(io);
  reader->Update();

  itk::ImageFileWriter<RGBImageType>::Pointer writer;
  writer = itk::ImageFileWriter<RGBImageType>::New();
  writer->SetInput(reader->GetOutput());
  writer->SetFileName(argv[2]);
  writer->SetImageIO(io);
  writer->Write();


  // Try writing out several kinds of images using png.
  // The images to test are as follows:
  // - 3D non-degenerate volume: this covers all images greater than or
  // equal to 3D.  The writer should write out the first slice.
  // - 3D degenerate volume: The writer should write out the first
  // slice.
  // - 2D image: The writer should write it out correctly.
  // - 2D degenerate image: The writer should write out the image.
  // - 1D image: The writer should write it out as a 2D image.

  typedef itk::Image< unsigned short, 3 > ImageType3D;
  typedef itk::Image< unsigned short, 2 > ImageType2D;
  typedef itk::Image< unsigned short, 1 > ImageType1D;

  //----------------------------------------------------------------//
  // 3D non-degenerate volume.
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
  writer3D->SetImageIO(io);
  writer3D->SetInput( volume );
  try
    {
    writer3D->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  //----------------------------------------------------------------//
  // 3D degenerate volume.
  ImageType3D::Pointer degenerateVolume = ImageType3D::New();
  // Collapse the first dimension.
  size3D[0] = 1;
  region3D.SetSize( size3D );
  degenerateVolume->SetRegions( region3D );
  degenerateVolume->Allocate();
  degenerateVolume->FillBuffer( 0 );

  writer3D->SetFileName( argv[2] );
  writer3D->SetImageIO(io);
  writer3D->SetInput( degenerateVolume );
  try
    {
    writer3D->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }


  //----------------------------------------------------------------//
  // 2D non-degenerate volume.
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
  try
    {
    writer2D->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  //----------------------------------------------------------------//
  // 2D degenerate volume.
  ImageType2D::Pointer degenerateImage = ImageType2D::New();
  // Collapse the first dimension.
  size2D[0] = 1;
  region2D.SetSize( size2D );
  degenerateImage->SetRegions( region2D );
  degenerateImage->Allocate();
  degenerateImage->FillBuffer( 0 );

  writer2D->SetFileName( argv[2] );
  writer2D->SetImageIO(io);
  writer2D->SetInput( degenerateImage );
  try
    {
    writer2D->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  //----------------------------------------------------------------//
  // 1D image.
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
  writer1D->SetImageIO(io);
  writer1D->SetInput( line );
  try
    {
    writer1D->Update();
    }
  catch (itk::ExceptionObject &e)
    {
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

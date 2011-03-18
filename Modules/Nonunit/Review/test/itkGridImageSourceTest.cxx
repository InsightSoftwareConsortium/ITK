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
#include "itkGridImageSource.h"
#include "itkImageFileWriter.h"
#include "itkBSplineKernelFunction.h"
#include "itkSimpleFilterWatcher.h"

int itkGridImageSourceTest0( int, char *argv[] )
{
  typedef float PixelType;
  const unsigned int ImageDimension = 3;
  typedef itk::Image<PixelType, ImageDimension> ImageType;

  // Instantiate the filter
  typedef itk::GridImageSource<ImageType> GridSourceType;
  GridSourceType::Pointer gridImage = GridSourceType::New();

  double scale = 255.0;
  ImageType::SizeType      size;
  ImageType::PointType     origin;
  ImageType::SpacingType   spacing;
  ImageType::DirectionType direction;

  GridSourceType::ArrayType     gridSpacing;
  GridSourceType::ArrayType     gridOffset;
  GridSourceType::ArrayType     sigma;
  GridSourceType::BoolArrayType which;

  // Specify image parameters
  origin.Fill( 0.0 );
  size.Fill( 64 );
  spacing.Fill( 1.0 );
  direction.SetIdentity();
//  direction(1,1)=-1.0;

  // Specify grid parameters
  gridSpacing.Fill( 8.0 );
  gridOffset.Fill( 0.0 );
  sigma.Fill( 3 );
  which.Fill( true );

  // Specify 0th order B-spline function (Box function)
  typedef itk::BSplineKernelFunction<0> KernelType;
  KernelType::Pointer kernel = KernelType::New();

  itk::SimpleFilterWatcher watcher(gridImage, "gridImage");

  // Set parameters
  gridImage->SetKernelFunction( kernel );
  gridImage->SetSpacing( spacing );
  gridImage->SetOrigin( origin );
  gridImage->SetDirection( direction );
  gridImage->SetSize( size );

  gridImage->SetGridSpacing( gridSpacing );
  gridImage->SetGridOffset( gridOffset );
  gridImage->SetWhichDimensions( which );
  gridImage->SetSigma( sigma );
  gridImage->SetScale( scale );

  try
    {
    gridImage->Update();
    }
  catch (itk::ExceptionObject & err)
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[1] );
  writer->SetInput( gridImage->GetOutput() );
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & err)
    {
    std::cout << "Unexpected exception caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int itkGridImageSourceTest1( int, char *argv[] )
{
  typedef float PixelType;
  const unsigned int ImageDimension = 3;
  typedef itk::Image<PixelType, ImageDimension> ImageType;

  // Instantiate the filter
  typedef itk::GridImageSource<ImageType> GridSourceType;
  GridSourceType::Pointer gridImage = GridSourceType::New();

  double scale = 255.0;
  ImageType::SizeType size;
  ImageType::PointType origin;
  ImageType::SpacingType spacing;
  GridSourceType::ArrayType gridSpacing;
  GridSourceType::ArrayType gridOffset;
  GridSourceType::ArrayType sigma;
  GridSourceType::BoolArrayType which;

  // Specify image parameters
  origin.Fill( 0.0 );
  size.Fill( 64 );
  spacing.Fill( 1.0 );

  // Specify grid parameters
  gridSpacing.Fill( 16.0 );
  gridOffset.Fill( 0.0 );
  sigma.Fill( 3 );
  which.Fill( true );

  // Specify 0th order B-spline function (Box function)
  typedef itk::BSplineKernelFunction<3> KernelType;
  KernelType::Pointer kernel = KernelType::New();

  // Set parameters
  gridImage->SetKernelFunction( kernel );
  gridImage->SetSpacing( spacing );
  gridImage->SetOrigin( origin );
  gridImage->SetSize( size );
  gridImage->SetGridSpacing( gridSpacing );
  gridImage->SetGridOffset( gridOffset );
  gridImage->SetWhichDimensions( which );
  gridImage->SetSigma( sigma );
  gridImage->SetScale( scale );

  try
    {
    gridImage->Update();
    }
  catch (itk::ExceptionObject & err)
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[1] );
  writer->SetInput( gridImage->GetOutput() );
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & err)
    {
    std::cout << "Unexpected exception caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int itkGridImageSourceTest2( int, char *argv[] )
{
  typedef float PixelType;
  const unsigned int ImageDimension = 3;
  typedef itk::Image<PixelType, ImageDimension> ImageType;

  // Instantiate the filter
  typedef itk::GridImageSource<ImageType> GridSourceType;
  GridSourceType::Pointer gridImage = GridSourceType::New();

  double scale = 255.0;
  ImageType::SizeType size;
  ImageType::PointType origin;
  ImageType::SpacingType spacing;
  GridSourceType::ArrayType gridSpacing;
  GridSourceType::ArrayType gridOffset;
  GridSourceType::ArrayType sigma;
  GridSourceType::BoolArrayType which;

  // Specify image parameters
  origin.Fill( 0.0 );
  size.Fill( 32 );
  spacing.Fill( 1.0 );

  // Specify grid parameters
  gridSpacing.Fill( 4.0 );
  gridOffset.Fill( 0.0 );
  sigma.Fill( 3 );
  which.Fill( true );
  which[ImageDimension-1] = false;

  // Set parameters
  gridImage->SetSpacing( spacing );
  gridImage->SetOrigin( origin );
  gridImage->SetSize( size );
  gridImage->SetGridSpacing( gridSpacing );
  gridImage->SetGridOffset( gridOffset );
  gridImage->SetWhichDimensions( which );
  gridImage->SetSigma( sigma );
  gridImage->SetScale( scale );

  try
    {
    gridImage->Update();
    }
  catch (itk::ExceptionObject & err)
    {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[1] );
  writer->SetInput( gridImage->GetOutput() );
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & err)
    {
    std::cout << "Unexpected exception caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int itkGridImageSourceTest3( int, char *argv[] )
{
  typedef float PixelType;
  const unsigned int ImageDimension = 3;
  typedef itk::Image<PixelType, ImageDimension> ImageType;

  // Instantiate the filter
  typedef itk::GridImageSource<ImageType> GridSourceType;
  GridSourceType::Pointer gridImage = GridSourceType::New();

  double scale = 255.0;
  ImageType::SizeType size;
  ImageType::PointType origin;
  ImageType::SpacingType spacing;
  GridSourceType::ArrayType gridSpacing;
  GridSourceType::ArrayType gridOffset;
  GridSourceType::ArrayType sigma;
  GridSourceType::BoolArrayType which;

  // Specify image parameters
  origin.Fill( 0.0 );
  size.Fill( 64 );
  spacing.Fill( 1.0 );

  // Specify grid parameters
  gridOffset.Fill( 0.0 );
  gridSpacing[0] = 32.0;
  gridSpacing[1] = 16.0;
  gridSpacing[2] = 16.0;
  sigma[0] = 1.0;
  sigma[1] = 5.0;
  sigma[2] = 6.0;

  which.Fill( true );

  // Set parameters
  gridImage->SetSpacing( spacing );
  gridImage->SetOrigin( origin );
  gridImage->SetSize( size );
  gridImage->SetGridSpacing( gridSpacing );
  gridImage->SetGridOffset( gridOffset );
  gridImage->SetWhichDimensions( which );
  gridImage->SetSigma( sigma );
  gridImage->SetScale( scale );

  try
    {
    gridImage->Update();
    }
  catch (itk::ExceptionObject & err)
    {
    std::cout << "Unexpected exception caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[1] );
  writer->SetInput( gridImage->GetOutput() );
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject & err)
    {
    std::cout << "Unexpected exception caught !" << std::endl;
    std::cout << err << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

int itkGridImageSourceTest( int argc, char *argv[] )
{
  if ( argc != 3 )
    {
    std::cout << "Usage: " << argv[0] << " outputImage whichTest" << std::endl;
    return EXIT_FAILURE;
    }

  int test;
  if ( atoi( argv[2] ) == 0 )
    {
    test = itkGridImageSourceTest0( argc, argv );
    }
  else if ( atoi( argv[2] ) == 1 )
    {
    test = itkGridImageSourceTest1( argc, argv );
    }
  else if ( atoi( argv[2] ) == 2 )
    {
    test = itkGridImageSourceTest2( argc, argv );
    }
  else
    {
    test = itkGridImageSourceTest3( argc, argv );
    }

  return test;
}

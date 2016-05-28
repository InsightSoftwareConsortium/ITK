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

#include "itkGaborImageSource.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"


int itkGaborImageSourceTest0( int, char *argv[] )
{
  typedef float PixelType;
  const unsigned int ImageDimension = 2;
  typedef itk::Image<PixelType, ImageDimension> ImageType;

  // Instantiate the filter
  typedef itk::GaborImageSource<ImageType> GaborSourceType;
  GaborSourceType::Pointer gaborImage = GaborSourceType::New();
  gaborImage->Print(std::cout);

  GaborSourceType::ArrayType sigma;
  sigma[0] = 2.0;
  sigma[1] = 5.0;

  ImageType::SizeType size;
  size.Fill( 64*4 );

  gaborImage->SetSize( size );

  gaborImage->SetSigma( sigma );
  gaborImage->SetFrequency( 0.1 );
  gaborImage->SetCalculateImaginaryPart( false );

  try
    {
    gaborImage->Update();
    gaborImage->Print(std::cout);
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
  writer->SetInput( gaborImage->GetOutput() );
  writer->Update();

  return EXIT_SUCCESS;
}

int itkGaborImageSourceTest1( int, char *argv[] )
{
  typedef float PixelType;
  const unsigned int ImageDimension = 3;
  typedef itk::Image<PixelType, ImageDimension> ImageType;

  // Instantiate the filter
  typedef itk::GaborImageSource<ImageType> GaborSourceType;
  GaborSourceType::Pointer gaborImage = GaborSourceType::New();

  GaborSourceType::ArrayType sigma;
  sigma[0] = 2.0;
  sigma[1] = 10.0;
  sigma[2] = 10.0;

  gaborImage->Print(std::cout);

  gaborImage->SetSigma( sigma );
  gaborImage->SetFrequency( 0.1 );
  gaborImage->SetCalculateImaginaryPart( true );

  try
    {
    gaborImage->Update();
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
  writer->SetInput( gaborImage->GetOutput() );
  writer->Update();

  gaborImage->Print(std::cout);

  return EXIT_SUCCESS;
}


int itkGaborImageSourceTest( int argc, char *argv[] )
{
  if ( argc < 3 )
    {
    std::cout << "Usage: " << argv[0] << " outputImage whichTest" << std::endl;
    return EXIT_FAILURE;
    }

  int test;
  if ( atoi( argv[2] ) == 0 )
    {
    test = itkGaborImageSourceTest0( argc, argv );
    }
  else
    {
    test = itkGaborImageSourceTest1( argc, argv );
    }

  return test;
}

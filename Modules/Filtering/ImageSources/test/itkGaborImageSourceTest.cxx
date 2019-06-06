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


template< int ImageDimension >
int itkGaborImageSourceTestHelper( char* outputFilename, bool calculcateImaginaryPart )
{
  using PixelType = float;
  using ImageType = itk::Image< PixelType, ImageDimension >;

  // Instantiate the filter
  using GaborSourceType = itk::GaborImageSource< ImageType >;
  typename GaborSourceType::Pointer gaborImage = GaborSourceType::New();

  if( ImageDimension == 2 )
    {
    typename ImageType::SizeType size;
    size.Fill( 64*4 );
    gaborImage->SetSize( size );
    }

  typename GaborSourceType::ArrayType sigma;
  if( ImageDimension == 2 )
    {
    sigma[0] = 2.0;
    sigma[1] = 5.0;
    }
  else
    {
    sigma[0] = 2.0;
    sigma[1] = 10.0;
    sigma[2] = 10.0;
    }
  gaborImage->SetSigma( sigma );
  ITK_TEST_SET_GET_VALUE( sigma, gaborImage->GetSigma() );

  typename GaborSourceType::ArrayType mean = 0.1;
  gaborImage->SetMean( mean );
  ITK_TEST_SET_GET_VALUE( mean, gaborImage->GetMean() );

  double frequency = 0.1;
  gaborImage->SetFrequency( frequency );
  ITK_TEST_SET_GET_VALUE( frequency, gaborImage->GetFrequency() );

  gaborImage->SetCalculateImaginaryPart( calculcateImaginaryPart );
  ITK_TEST_SET_GET_VALUE( calculcateImaginaryPart, gaborImage->GetCalculateImaginaryPart() );
  if( calculcateImaginaryPart )
    {
    gaborImage->CalculateImaginaryPartOn();
    ITK_TEST_SET_GET_VALUE( true, gaborImage->GetCalculateImaginaryPart() );
    }
  else
    {
    gaborImage->CalculateImaginaryPartOff();
    ITK_TEST_SET_GET_VALUE( false, gaborImage->GetCalculateImaginaryPart() );
    }

  ITK_TRY_EXPECT_NO_EXCEPTION( gaborImage->Update() );

  using WriterType = itk::ImageFileWriter< ImageType >;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( outputFilename );
  writer->SetInput( gaborImage->GetOutput() );

  ITK_TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}

int itkGaborImageSourceTest( int argc, char *argv[] )
{
  if ( argc < 3 )
    {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " outputImage whichTest" << std::endl;
    return EXIT_FAILURE;
    }


  constexpr unsigned int ImageDimension = 2;
  using PixelType = float;

  using ImageType = itk::Image< PixelType, ImageDimension >;

  // Instantiate the filter
  using GaborSourceType = itk::GaborImageSource< ImageType >;
  GaborSourceType::Pointer gaborImage = GaborSourceType::New();

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  ITK_EXERCISE_BASIC_OBJECT_METHODS( gaborImage, GaborImageSource, GenerateImageSource );


  int testStatus = EXIT_SUCCESS;
  if( std::stoi( argv[2] ) == 0 )
    {
    testStatus = itkGaborImageSourceTestHelper< 2 >( argv[1], false );
    }
  else
    {
    testStatus = itkGaborImageSourceTestHelper< 3 >( argv[1], true );
    }

  std::cout << "Test finished" << std::endl;
  return testStatus;
}

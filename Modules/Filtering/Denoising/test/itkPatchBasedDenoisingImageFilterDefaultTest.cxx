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

#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkStdStreamLogOutput.h"
#include "itkPatchBasedDenoisingImageFilter.h"
#include "itkTestingMacros.h"


template< typename ImageT >
int doDenoising( const std::string & inputFileName, const std::string & outputFileName )
{
  typedef itk::ImageFileReader< ImageT > ReaderType;

  typedef itk::PatchBasedDenoisingImageFilter< ImageT, ImageT > FilterType;

  typedef typename FilterType::OutputImageType OutputImageType;

  typedef itk::ImageFileWriter< OutputImageType > WriterType;

  // Read the noisy image to be denoised
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputFileName );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );


  // Create filter and initialize
  typename FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );

  // Use 2 threads for consistency
  filter->SetNumberOfThreads( 2 );

  // Denoise the image
  TRY_EXPECT_NO_EXCEPTION( filter->Update() );


  // Write the denoised image to file
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( outputFileName );
  writer->SetInput( filter->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}

int itkPatchBasedDenoisingImageFilterDefaultTest( int argc, char * argv [] )
{
  if( argc < 3 )
  {
    std::cerr << "Missing command line arguments" << std::endl;
    std::cerr << "Usage : " << argv[0]
              << " inputImageFileName outputImageFileName"
              << " numDimensions"
              << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  typedef float                                                       PixelType;
  typedef itk::Image< PixelType, 3 >                                  ImageType;
  typedef itk::PatchBasedDenoisingImageFilter< ImageType, ImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, PatchBasedDenoisingImageFilter,
    PatchBasedDenoisingBaseImageFilter );


  const std::string inFileName( argv[1] );

  const std::string outFileName( argv[2] );

  const unsigned int numDimensions = atoi( argv[3] );

  typedef float PixelComponentType;

  typedef PixelComponentType                           OneComponentType;

  typedef itk::Image< OneComponentType, 2 > OneComponent2DImage;
  typedef itk::Image< OneComponentType, 3 > OneComponent3DImage;

  if( numDimensions == 2 )
    {
    return doDenoising< OneComponent2DImage >( inFileName, outFileName );
    }
  else if( numDimensions == 3 )
    {
    return doDenoising< OneComponent3DImage >( inFileName, outFileName );
    }
  else
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << numDimensions << " dimensions "
              << "isn't supported in this test driver."
              << std::endl;
    return EXIT_FAILURE;
    }

}

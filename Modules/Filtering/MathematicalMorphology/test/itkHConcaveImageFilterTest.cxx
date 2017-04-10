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

#include "itkHConcaveImageFilter.h"
#include "itkFilterWatcher.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

int itkHConcaveImageFilterTest( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0]
      << " inputImageFile"
      << " outputImageFile"
      << " height"
      << " fullyConnected" << std::endl;
    return EXIT_FAILURE;
    }

  //
  // The following code defines the input and output pixel types and their
  // associated image types.
  //
  const unsigned int Dimension = 2;

  typedef short         InputPixelType;
  typedef unsigned char OutputPixelType;

  typedef itk::Image< InputPixelType, Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >  OutputImageType;

  // Read the input image
  typedef itk::ImageFileReader< InputImageType  > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );


  // Define the itk::HConcaveImageFilter filter type
  typedef itk::HConcaveImageFilter<
                            InputImageType,
                            OutputImageType > HConcaveFilterType;

  // Create the filter
  HConcaveFilterType::Pointer hConcaveFilter = HConcaveFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( hConcaveFilter, HConcaveImageFilter,
    ImageToImageFilter );

  FilterWatcher watchConcave( hConcaveFilter, "HConcaveImageFilter" );

  // Set up the filter
  HConcaveFilterType::InputImagePixelType height =
    static_cast< HConcaveFilterType::InputImagePixelType >( atof( argv[3] ) );

  hConcaveFilter->SetHeight( height );
  TEST_SET_GET_VALUE( height, hConcaveFilter->GetHeight() );

  bool fullyConnected = static_cast< bool >( atof( argv[4] ) );
  TEST_SET_GET_BOOLEAN( hConcaveFilter, FullyConnected, fullyConnected );


  hConcaveFilter->SetInput( reader->GetOutput() );

  // Run the filter
  TRY_EXPECT_NO_EXCEPTION( hConcaveFilter->Update() );


  // Write the output
  typedef itk::ImageFileWriter< OutputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( hConcaveFilter->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}

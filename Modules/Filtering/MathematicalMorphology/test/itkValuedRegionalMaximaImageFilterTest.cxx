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

#include "itkValuedRegionalMaximaImageFilter.h"
#include "itkHConvexImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkAndImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"


// A test routine for regional extrema using flooding
int itkValuedRegionalMaximaImageFilterTest( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Missing parameters" << std::endl;
    std::cerr << "Usage: " << argv[0]
      << " inputImageFile"
      << " outputImageFile1"
      << " outputImageFile2"
      << " fullyConnected";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned char                       PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::ValuedRegionalMaximaImageFilter< ImageType, ImageType >
    FilterType;
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter,
    ValuedRegionalMaximaImageFilter,
    ValuedRegionalExtremaImageFilter );

  bool fullyConnected = atoi( argv[4] );
  TEST_SET_GET_BOOLEAN( filter, FullyConnected, fullyConnected );


  itk::SimpleFilterWatcher watcher( filter, "ValuedRegionalMaximaImageFilter" );

  filter->SetInput( reader->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );


  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  // Produce the same output with other filters
  typedef itk::HConvexImageFilter< ImageType, ImageType > ConvexFilterType;
  ConvexFilterType::Pointer convexFilter = ConvexFilterType::New();
  convexFilter->SetInput( reader->GetOutput() );
  convexFilter->SetFullyConnected( fullyConnected );
  convexFilter->SetHeight( 1 );

  // Convex gives maxima with value=1 and others with value = 0
  // Rescale the image so we have maxima = 255 other = 0
  typedef itk::RescaleIntensityImageFilter< ImageType, ImageType > RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetInput( convexFilter->GetOutput() );
  rescaler->SetOutputMaximum( 255 );
  rescaler->SetOutputMinimum( 0 );

  // in the input image, select the values of the pixel at the minima
  typedef itk::AndImageFilter< ImageType, ImageType, ImageType > AndFilterType;
  AndFilterType::Pointer andFilter = AndFilterType::New();
  andFilter->SetInput( 0, rescaler->GetOutput() );
  andFilter->SetInput( 1, reader->GetOutput() );

  WriterType::Pointer writer2 = WriterType::New();
  writer2->SetInput( andFilter->GetOutput() );
  writer2->SetFileName( argv[3] );
  writer2->Update();

  std::cerr << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}

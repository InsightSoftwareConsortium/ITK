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

#include "itkIsolatedWatershedImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"

#include <fstream>


int itkIsolatedWatershedImageFilterTest( int argc, char* argv[] )
{
  if( argc < 9 )
    {
    std::cerr << "Missing parameters" << std::endl;
    std::cerr << "Usage: " << argv[0]
      << " InputImage"
      << " OutputImage"
      << " seed1_x"
      << " seed1_y"
      << " seed2_x"
      << " seed2_y"
      << " threshold"
      << " isolatedValueTolerance";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned char                       PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  itk::ImageFileReader< ImageType >::Pointer reader =
    itk::ImageFileReader< ImageType >::New();

  reader->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );


  // Create the IsolatedWatershedImageFilter object
  typedef itk::IsolatedWatershedImageFilter< ImageType, ImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, IsolatedWatershedImageFilter,
    ImageToImageFilter );


  filter->SetInput( reader->GetOutput() );


  FilterType::IndexType seed1, seed2;
  seed1.Fill( 0 );
  seed2.Fill( 0 );

  // Test the seeds being outside the input image exception
  ImageType::Pointer inputImage = reader->GetOutput();

  ImageType::RegionType region = inputImage->GetLargestPossibleRegion();
  ImageType::IndexType offset;
  offset.Fill( 10 );

  seed1[0] = region.GetUpperIndex()[0] + offset[0];
  filter->SetSeed1( seed1 );

  TRY_EXPECT_EXCEPTION( filter->Update() );

  seed1.Fill( 0 );
  filter->SetSeed1( seed1 );

  seed2[1] = region.GetUpperIndex()[1] + offset[1];
  filter->SetSeed2( seed2 );

  TRY_EXPECT_EXCEPTION( filter->Update() );


  seed1[0] = atoi( argv[3] );
  seed1[1] = atoi( argv[4] );
  filter->SetSeed1( seed1 );
  TEST_SET_GET_VALUE( seed1, filter->GetSeed1() );

  seed2[0] = atoi( argv[5] );
  seed2[1] = atoi( argv[6] );
  filter->SetSeed2( seed2 );
  TEST_SET_GET_VALUE( seed2, filter->GetSeed2() );

  double threshold = atof( argv[7] );
  filter->SetThreshold( threshold );
  TEST_SET_GET_VALUE( threshold, filter->GetThreshold() );

  PixelType replaceValue1 = 255;
  filter->SetReplaceValue1( replaceValue1 );
  TEST_SET_GET_VALUE( replaceValue1, filter->GetReplaceValue1() );

  PixelType replaceValue2 = 127;
  filter->SetReplaceValue2( replaceValue2 );
  TEST_SET_GET_VALUE( replaceValue2, filter->GetReplaceValue2() );

  double upperValueLimit = 1.0;
  filter->SetUpperValueLimit( upperValueLimit );
  TEST_SET_GET_VALUE( upperValueLimit, filter->GetUpperValueLimit() );

  double isolatedValueTolerance = atof( argv[8] );
  filter->SetIsolatedValueTolerance( isolatedValueTolerance );
  TEST_SET_GET_VALUE( isolatedValueTolerance,
    filter->GetIsolatedValueTolerance() );

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );


  double isolatedValue = filter->GetIsolatedValue();
  std::cout << "IsolatedValue: " << isolatedValue << std::endl;

  // Write the filter output
  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}

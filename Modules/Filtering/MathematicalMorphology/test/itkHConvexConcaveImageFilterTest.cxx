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
#include "itkRescaleIntensityImageFilter.h"

#include "itkHConvexImageFilter.h"
#include "itkHConcaveImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkFilterWatcher.h"
#include "itkTestingMacros.h"

int itkHConvexConcaveImageFilterTest( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile";
    std::cerr << " outputImageFile height" << std::endl;
    return EXIT_FAILURE;
    }


  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  const unsigned int Dimension = 2;

  typedef float         InputPixelType;
  typedef float         OutputPixelType;
  typedef unsigned char WritePixelType;

  typedef itk::Image< InputPixelType,  Dimension > InputImageType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  typedef itk::Image< WritePixelType, Dimension >  WriteImageType;

  // Readers/writers
  typedef itk::ImageFileReader< InputImageType  > ReaderType;
  typedef itk::ImageFileWriter< WriteImageType >  WriterType;
  typedef itk::RescaleIntensityImageFilter< OutputImageType, WriteImageType >
                                                  RescaleType;

  // Define the itk::HConvexImageFilter filter type
  typedef itk::HConvexImageFilter<
                            InputImageType,
                            InputImageType > HConvexFilterType;
  // Define the itk::HConcaveImageFilter filter type
  typedef itk::HConcaveImageFilter<
                            InputImageType,
                            InputImageType > HConcaveFilterType;


  // Creation of reader and writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  RescaleType::Pointer rescaler = RescaleType::New();

  // Create the filters
  HConvexFilterType::Pointer hConvexFilter = HConvexFilterType::New();
  HConcaveFilterType::Pointer hConcaveFilter = HConcaveFilterType::New();

  FilterWatcher watchConvex( hConvexFilter, "HConvexImageFilter" );
  FilterWatcher watchConcave( hConcaveFilter, "HConcaveImageFilter" );

  // Set up the input and output files
  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  // Set up the filters
  hConvexFilter->SetInput( reader->GetOutput() );
  hConvexFilter->SetHeight( atof( argv[3] ) );

  hConcaveFilter->SetInput( reader->GetOutput() );
  hConcaveFilter->SetHeight( atof( argv[3] ) );

  // Create a filter to add the two images
  typedef itk::AddImageFilter<
                            InputImageType, InputImageType,
                            OutputImageType > AddFilterType;

  AddFilterType::Pointer add = AddFilterType::New();
  add->SetInput1( hConvexFilter->GetOutput() );
  add->SetInput2( hConcaveFilter->GetOutput() );

  // Run the filters
  rescaler->SetInput( add->GetOutput() );
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );

  writer->SetInput( rescaler->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}

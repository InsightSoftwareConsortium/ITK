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

#include "itkSimpleContourExtractorImageFilter.h"
#include "itkFilterWatcher.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkBoxImageFilter.h"
#include "itkTestingMacros.h"

int itkSimpleContourExtractorImageFilterTest( int argc, char* argv [] )
{
  if( argc < 3 )
    {
    std::cerr << "Missing arguments." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImage outputImage " << std::endl;
    return EXIT_FAILURE;
    }

  // Define the dimension of the images
  const unsigned int Dimension = 2;

  // Define the pixel type
  typedef unsigned char PixelType;

  // Declare the types of the images
  typedef itk::Image<PixelType, Dimension>  ImageType;

  // Declare the reader and writer
  typedef itk::ImageFileReader< ImageType > ReaderType;
  typedef itk::ImageFileWriter< ImageType > WriterType;


  // Declare the type for the morphology Filter
  typedef itk::SimpleContourExtractorImageFilter< ImageType, ImageType >
    FilterType;

  // Create the reader and writer
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  // Create the filter
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, SimpleContourExtractorImageFilter,
    BoxImageFilter );

  FilterWatcher watcher(filter, "filter");

  // Connect the pipeline
  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );

  FilterType::InputPixelType inputForegroundValue = 255;
  FilterType::InputPixelType inputBackgroundValue = 0;
  FilterType::OutputPixelType outputForegroundValue =
    itk::NumericTraits< FilterType::OutputPixelType >::max();
  FilterType::OutputPixelType outputBackgroundValue =
    itk::NumericTraits< FilterType::OutputPixelType >::ZeroValue();

  filter->SetInputForegroundValue( inputForegroundValue );

  TEST_SET_GET_VALUE( inputForegroundValue, filter->GetInputForegroundValue() );

  filter->SetInputBackgroundValue( inputBackgroundValue );

  TEST_SET_GET_VALUE( inputBackgroundValue, filter->GetInputBackgroundValue() );

  filter->SetOutputForegroundValue( outputForegroundValue );

  TEST_SET_GET_VALUE( outputForegroundValue, filter->GetOutputForegroundValue() );

  filter->SetOutputBackgroundValue( outputBackgroundValue );

  TEST_SET_GET_VALUE( outputBackgroundValue, filter->GetOutputBackgroundValue() );


  FilterType::InputSizeType radius;

  radius.Fill( 1 );

  filter->SetRadius( radius );

  // Exercise Print()
  filter->Print( std::cout );

  // Execute the filter
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception caught during pipeline Update\n"  << e;
    return EXIT_FAILURE;
    }

  // All objects should be automatically destroyed at this point

  return EXIT_SUCCESS;

}

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
#include "itkCannyEdgeDetectionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


int itkCannyEdgeDetectionImageFilterTest( int argc, char * argv[] )
{
  if(argc < 3)
    {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " InputImage OutputImage" << std::endl;
    return EXIT_FAILURE;
    }

  constexpr unsigned int Dimension = 2;
  using InputPixelType = float;
  using InputImage = itk::Image< InputPixelType, Dimension >;
  using OutputPixelType = unsigned char;
  using OutputImage = itk::Image< OutputPixelType, Dimension >;
  using CannyEdgeDetectionImageFilterType =
      itk::CannyEdgeDetectionImageFilter<InputImage, InputImage>;


  itk::ImageFileReader< InputImage >::Pointer reader =
    itk::ImageFileReader< InputImage >::New();
  reader->SetFileName( argv[1] );

  // Set up the filter
  CannyEdgeDetectionImageFilterType::Pointer filter = CannyEdgeDetectionImageFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, CannyEdgeDetectionImageFilter, ImageToImageFilter );

  itk::SimpleFilterWatcher watcher( filter );

  filter->SetInput( reader->GetOutput() );

  CannyEdgeDetectionImageFilterType::OutputImagePixelType upperThreshold = 30;
  filter->SetUpperThreshold( upperThreshold );
  TEST_SET_GET_VALUE( upperThreshold, filter->GetUpperThreshold() );

  CannyEdgeDetectionImageFilterType::OutputImagePixelType lowerThreshold = 15;
  filter->SetLowerThreshold( lowerThreshold );
  TEST_SET_GET_VALUE( lowerThreshold, filter->GetLowerThreshold() );

  CannyEdgeDetectionImageFilterType::ArrayType variance = 1.0f;
  filter->SetVariance( variance );
  TEST_SET_GET_VALUE( variance, filter->GetVariance() );

  CannyEdgeDetectionImageFilterType::ArrayType maximumError = .01f;
  filter->SetMaximumError( maximumError );
  TEST_SET_GET_VALUE( maximumError, filter->GetMaximumError() );


  itk::RescaleIntensityImageFilter<InputImage, OutputImage>::Pointer rescale =
    itk::RescaleIntensityImageFilter<InputImage, OutputImage>::New();

  rescale->SetInput( filter->GetOutput() );

  rescale->SetOutputMinimum( 0);
  rescale->SetOutputMaximum( 255 );

  itk::ImageFileWriter<OutputImage>::Pointer writer = itk::ImageFileWriter<OutputImage>::New();
  writer->SetInput( rescale->GetOutput() );
  writer->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}

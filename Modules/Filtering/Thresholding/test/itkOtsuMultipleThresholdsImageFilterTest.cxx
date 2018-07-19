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
#include "itkOtsuMultipleThresholdsImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int itkOtsuMultipleThresholdsImageFilterTest(int argc, char* argv[] )
{
  if( argc < 6 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImageFile outputImageFile";
    std::cerr << " numberOfHistogramBins";
    std::cerr << " numberOfThresholds";
    std::cerr << " labelOffset";
    std::cerr << " [valleyEmphasis]";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  using InputPixelType = short;
  using InternalPixelType = unsigned short;
  using OutputPixelType = unsigned char;

  using InputImageType = itk::Image< InputPixelType,  2 >;
  using InternalImageType = itk::Image< InternalPixelType, 2>;
  using OutputImageType = itk::Image< OutputPixelType, 2 >;

  using ReaderType = itk::ImageFileReader< InputImageType >;
  using FilterType = itk::OtsuMultipleThresholdsImageFilter< InputImageType, InternalImageType >;
  using RescaleType = itk::RescaleIntensityImageFilter< InternalImageType, OutputImageType >;
  using WriterType = itk::ImageFileWriter< OutputImageType >;

  ReaderType::Pointer reader = ReaderType::New();
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, OtsuMultipleThresholdsImageFilter,
    ImageToImageFilter );

  RescaleType::Pointer rescaler = RescaleType::New();
  WriterType::Pointer writer = WriterType::New();

  itk::SimpleFilterWatcher watcher(filter);

  // Set up the reader
  reader->SetFileName( argv[1] );

  // Set up the filter parameters
  filter->SetInput( reader->GetOutput() );

  auto numberOfHistogramBins = static_cast< itk::SizeValueType >(atoi( argv[3] ) );
  filter->SetNumberOfHistogramBins( numberOfHistogramBins );
  TEST_SET_GET_VALUE( numberOfHistogramBins, filter->GetNumberOfHistogramBins() );

  auto numberOfThresholds = static_cast< itk::SizeValueType >(atoi( argv[4] ) );
  filter->SetNumberOfThresholds( numberOfThresholds );
  TEST_SET_GET_VALUE( numberOfThresholds, filter->GetNumberOfThresholds() );

  auto labelOffset =  static_cast< FilterType::OutputPixelType >(atoi( argv[5] ) );
  filter->SetLabelOffset( labelOffset );
  TEST_SET_GET_VALUE( labelOffset, filter->GetLabelOffset() );

  if( argc > 6 )
    {
    bool valleyEmphasis =  static_cast< bool >( atoi( argv[6] ) );
    filter->SetValleyEmphasis( valleyEmphasis );
    TEST_SET_GET_VALUE( valleyEmphasis, filter->GetValleyEmphasis() );
    }

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );


  // Rescale the image so that it can be seen.  The output of the
  // filter contains labels that are numbered sequentially, so the
  // image looks nearly uniform unless there are a large number of labels.
  rescaler->SetInput( filter->GetOutput() );
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);

  // Write out the test image
  writer->SetFileName( argv[2] );
  writer->SetInput( rescaler->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  return EXIT_SUCCESS;
}

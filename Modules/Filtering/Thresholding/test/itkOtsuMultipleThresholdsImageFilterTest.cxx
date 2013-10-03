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
#include "itkFilterWatcher.h"

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

  typedef  short          InputPixelType;
  typedef  unsigned short InternalPixelType;
  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< InternalPixelType, 2>  InternalImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  typedef itk::OtsuMultipleThresholdsImageFilter< InputImageType, InternalImageType >  FilterType;
  typedef itk::RescaleIntensityImageFilter< InternalImageType, OutputImageType > RescaleType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  FilterType::Pointer filter = FilterType::New();
  RescaleType::Pointer rescaler = RescaleType::New();
  WriterType::Pointer writer = WriterType::New();

  FilterWatcher watcher(filter);

  // Set up the reader
  reader->SetFileName( argv[1] );

  // Set up the filter parameters.
  filter->SetInput( reader->GetOutput() );
  filter->SetNumberOfHistogramBins (atoi(argv[3]));
  filter->SetNumberOfThresholds( atoi(argv[4]) );
  filter->SetLabelOffset( atoi(argv[5]) );
  if( argc > 6 )
  {
    filter->SetValleyEmphasis(atoi(argv[6]));
  }
  try
    {
    filter->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  // Test GetMacros
  unsigned long numberOfHistogramBins = filter->GetNumberOfHistogramBins();
  std::cout << "filter->GetNumberOfHistogramBins(): "
            << numberOfHistogramBins
            << std::endl;
  unsigned long numberOfThresholds = filter->GetNumberOfThresholds();
  std::cout << "filter->GetNumberOfThresholds(): "
            << numberOfThresholds
            << std::endl;
  OutputPixelType labelOffset = filter->GetLabelOffset();
  std::cout << "filter->GetLabelOffset(): "
            << static_cast<itk::NumericTraits<OutputPixelType>::PrintType>( labelOffset )
            << std::endl;
  FilterType::ThresholdVectorType thresholds = filter->GetThresholds();
  std::cout << "filter->GetThresholds(): ";
  for (unsigned int i = 0; i < thresholds.size(); i++)
    {
    std::cout << itk::NumericTraits<FilterType::InputPixelType>::PrintType(thresholds[i]) << " ";
    }
  std::cout << std::endl;

  // Rescale the image so that it can be seen.  The output of the
  // filter contains labels that are numbered sequentially, so the
  // image looks nearly uniform unless there are a large number of labels.
  rescaler->SetInput( filter->GetOutput() );
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);

  // Write out the test image
  writer->SetFileName( argv[2] );
  writer->SetInput( rescaler->GetOutput() );
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

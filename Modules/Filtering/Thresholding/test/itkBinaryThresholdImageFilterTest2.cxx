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
#include "itkStatisticsImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"


int itkBinaryThresholdImageFilterTest2(int ac, char* av[] )
{
  if(ac < 4)
    {
    std::cerr << "Usage: " << av[0] <<" InputImage1 InputImage2 OutputImage\n";
    return -1;
    }

  // Threshold one image based on the statistics of another image
  //
  //

  // Define the dimension of the images
  const unsigned int ImageDimension = 2;

  // Declare the types of the images
  typedef itk::Image<unsigned char, ImageDimension> ImageType;
  typedef itk::Image<double, ImageDimension>        FloatImageType;

  // File reader and writer
  typedef itk::ImageFileReader<FloatImageType> ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( av[1] );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( av[2] );

  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( av[3] );

  // Declare the filter types
  typedef itk::StatisticsImageFilter<FloatImageType>  StatisticsType;
  typedef itk::BinaryThresholdImageFilter<FloatImageType, ImageType>  ThresholdType;

  // Create the filters
  StatisticsType::Pointer statistics = StatisticsType::New();
  ThresholdType::Pointer threshold = ThresholdType::New();

  // connect the standard pipeline connections
  statistics->SetInput( reader2->GetOutput() );
  threshold->SetInput( reader->GetOutput() );

  // print before assigning thresholds
  threshold->Print(std::cout);

  // now connect the inputs and outputs that are decorated scalars
  threshold->SetUpperThresholdInput( statistics->GetMeanOutput() );
  threshold->SetLowerThresholdInput( statistics->GetMinimumOutput() );

  // connect the writer
  writer->SetInput( threshold->GetOutput() );

  // Execute the filter
  try
    {
    writer->Update();
    }
  catch(...)
    {
    std::cerr << "Caught an unexpected exception. " << std::endl;
    std::cerr << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

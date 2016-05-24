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
#include "itkFilterWatcher.h"

#include "itkDoubleThresholdImageFilter.h"


int itkDoubleThresholdImageFilterTest( int argc, char * argv[] )
{
  if( argc < 7 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  ";
    std::cerr << " outputImageFile threshold1 threshold2 threshold3 threshold4 " << std::endl;
    return EXIT_FAILURE;
    }


  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  const unsigned int Dimension = 2;

  typedef unsigned char    InputPixelType;
  typedef unsigned char    OutputPixelType;
  typedef unsigned char    WritePixelType;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;
  typedef itk::Image< WritePixelType, Dimension >    WriteImageType;


  // readers/writers
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< WriteImageType >   WriterType;
  typedef itk::RescaleIntensityImageFilter<OutputImageType, WriteImageType>
                                                   RescaleType;

  // define the fillhole filter
  typedef itk::DoubleThresholdImageFilter<
                            InputImageType,
                            OutputImageType >  DoubleThresholdFilterType;


  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer  = WriterType::New();
  RescaleType::Pointer rescaler = RescaleType::New();

  // Create the filter
  DoubleThresholdFilterType::Pointer  threshold = DoubleThresholdFilterType::New();
  FilterWatcher watcher(threshold, "threshold");

  // Setup the input and output files
  reader->SetFileName( argv[1] );
  writer->SetFileName(  argv[2] );

  // Setup the fillhole method
  threshold->SetInput(  reader->GetOutput() );
  const OutputPixelType InsideValue = threshold->GetInsideValue();
  const OutputPixelType OutsideValue = threshold->GetOutsideValue();
  if (InsideValue == 255)
  {
    threshold->SetInsideValue( 255 );
  }
  if (OutsideValue == 0)
  {
    threshold->SetOutsideValue( 0 );
  }

  int thresholds[4];
  for(unsigned i = 0; i < 4; ++i)
    {
    thresholds[i] = atoi(argv[i+3]);
    }
  threshold->SetThreshold1( thresholds[0] );
  threshold->SetThreshold2( thresholds[1] );
  threshold->SetThreshold3( thresholds[2] );
  threshold->SetThreshold4( thresholds[3] );
  unsigned error = EXIT_SUCCESS;

  if(threshold->GetThreshold1() != thresholds[0])
    {
    std::cerr << "Expected " << thresholds[0] << "for threshold 1, found " <<
      threshold->GetThreshold1() << std::endl;
    error = EXIT_FAILURE;
    }
  if(threshold->GetThreshold2() != thresholds[1])
    {
    std::cerr << "Expected " << thresholds[1] << "for threshold 2, found " <<
      threshold->GetThreshold2() << std::endl;
    error = EXIT_FAILURE;
    }
  if(threshold->GetThreshold3() != thresholds[2])
    {
    std::cerr << "Expected " << thresholds[1] << "for threshold 3, found " <<
      threshold->GetThreshold3() << std::endl;
    error = EXIT_FAILURE;
    }
  if(threshold->GetThreshold4() != thresholds[3])
    {
    std::cerr << "Expected " << thresholds[1] << "for threshold 4, found " <<
      threshold->GetThreshold4() << std::endl;
    error = EXIT_FAILURE;
    }
  if (thresholds[0] <= thresholds[1] &&
      thresholds[1] <= thresholds[2] &&
      thresholds[2] <= thresholds[3])
    {
    std::cerr<<"Values inputed as Threshold meet the requirement"<<std::endl;
    }
  else
    {
    std::cerr << "Thresholds aren't monotonically ascending" << std::endl;
    error = EXIT_FAILURE;
    }

  threshold->SetFullyConnected(false);

  // Run the filter
  rescaler->SetInput( threshold->GetOutput() );
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();

  return error;
}

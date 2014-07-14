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
#include "itkRescaleIntensityImageFilter.h"

// This test is was written to test bug 9431
//
// It run a filter multiple times and expects the output to be the
// same when run with the same parameters. The two output images
// should be identical
int itkCannyEdgeDetectionImageFilterTest2(int argc, char * argv[] )
{
  if(argc < 4)
    {
    std::cerr << "Usage: " << argv[0] << " InputImage OutputImage1 OutputImage2\n";
    return -1;
    }

  const unsigned int Dimension = 2;
  typedef float                                    InputPixelType;
  typedef itk::Image< InputPixelType, Dimension >  InputImage;
  typedef unsigned char                            OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImage;

  itk::ImageFileReader<InputImage>::Pointer input
    = itk::ImageFileReader<InputImage>::New();
  input->SetFileName(argv[1]);

  // Set up filter
  itk::CannyEdgeDetectionImageFilter<InputImage, InputImage>::Pointer
    filter =
    itk::CannyEdgeDetectionImageFilter<InputImage, InputImage>::New();
  filter->SetInput(input->GetOutput());
  filter->SetVariance(1.0f);
  filter->SetMaximumError(.01f);

  filter->SetUpperThreshold(25);
  filter->SetLowerThreshold(10);


  itk::RescaleIntensityImageFilter<InputImage, OutputImage>::Pointer
    rescale =
    itk::RescaleIntensityImageFilter<InputImage, OutputImage>::New();
  rescale->SetInput(filter->GetOutput());
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);

  itk::ImageFileWriter<OutputImage>::Pointer writer;
  writer = itk::ImageFileWriter<OutputImage>::New();
  writer->SetInput( rescale->GetOutput() );


  try
    {
    // Generate test image
    writer->SetFileName( argv[2] );
    writer->Update();

    // set canny filter to another value
    filter->SetUpperThreshold(20);
    filter->SetLowerThreshold(5);

    rescale->Update();

    // set it back expecting the same results
    filter->SetUpperThreshold(25);
    filter->SetLowerThreshold(10);

    // Generate test image
    writer->SetFileName( argv[3] );
    writer->Update();


    }
  catch(itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

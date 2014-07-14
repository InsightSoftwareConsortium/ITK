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

int itkCannyEdgeDetectionImageFilterTest(int argc, char * argv[] )
{
  if(argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " InputImage OutputImage\n";
    return -1;
    }

  const unsigned int Dimension = 2;
  typedef float                                    InputPixelType;
  typedef itk::Image< InputPixelType, Dimension >  InputImage;
  typedef unsigned char                            OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImage;

  itk::ImageFileReader< InputImage >::Pointer input = itk::ImageFileReader< InputImage >::New();
  input->SetFileName(argv[1]);

  // Set up filter
  itk::CannyEdgeDetectionImageFilter<InputImage, InputImage>::Pointer
    filter =
    itk::CannyEdgeDetectionImageFilter<InputImage, InputImage>::New();
  itk::SimpleFilterWatcher watcher(filter);
  filter->SetInput(input->GetOutput());
  filter->SetUpperThreshold(30);
  filter->SetLowerThreshold(15);
  filter->SetVariance(1.0f);
  filter->SetMaximumError(.01f);

  itk::RescaleIntensityImageFilter<InputImage, OutputImage>::Pointer
    rescale =
    itk::RescaleIntensityImageFilter<InputImage, OutputImage>::New();
  rescale->SetInput(filter->GetOutput());
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);

  try
    {
    // Generate test image
    itk::ImageFileWriter<OutputImage>::Pointer writer;
      writer = itk::ImageFileWriter<OutputImage>::New();
      writer->SetInput( rescale->GetOutput() );
      writer->SetFileName( argv[2] );
      writer->Update();
    }
  catch(itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return EXIT_FAILURE;
    }

  // test for correct setting of non-macro methods
  if (filter->GetVariance()[0] != 1.0f || filter->GetMaximumError()[0] != .01f)
    {
      return EXIT_FAILURE;
    }
  filter->SetVariance(0.5f);
  filter->SetMaximumError(0.5f);
  if (filter->GetVariance()[0] != 0.5f || filter->GetMaximumError()[0] != 0.5f)
    {
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

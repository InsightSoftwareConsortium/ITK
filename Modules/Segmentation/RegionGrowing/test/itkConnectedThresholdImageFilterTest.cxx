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

#include <fstream>
#include "itkConnectedThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"

int itkConnectedThresholdImageFilterTest(int ac, char* av[] )
{
  if(ac < 7)
    {
    std::cerr << "Usage: " << av[0]
      << " InputImage OutputImage "
      << "seed_x seed_y "
      << "LowerConnectedThreshold UpperConnectedThreshold "
      << "Connectivity[1=Full,0=Face]" << std::endl;
    return -1;
    }

  typedef unsigned char            PixelType;
  typedef itk::Image<PixelType, 2> myImage;

  itk::ImageFileReader<myImage>::Pointer input
    = itk::ImageFileReader<myImage>::New();
  input->SetFileName(av[1]);

  // Create a filter
  typedef itk::ConnectedThresholdImageFilter<myImage,myImage> FilterType;

  FilterType::Pointer filter = FilterType::New();
  FilterWatcher watcher(filter);

  filter->SetInput(input->GetOutput());
  FilterType::IndexType seed; seed[0] = atoi(av[3]); seed[1] = atoi(av[4]);
  filter->AddSeed(seed);
  filter->SetLower(atoi(av[5]));
  filter->SetUpper(atoi(av[6]));
  filter->SetReplaceValue(255);

  // Test the use of full (8 connectivity in 2D) on this image.
  if (ac > 7)
    {
    filter->SetConnectivity( atoi(av[7]) ?
        FilterType::FullConnectivity : FilterType::FaceConnectivity );
    }

  try
    {
    input->Update();
    filter->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return -1;
    }

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( av[2] );
  writer->Update();

  return EXIT_SUCCESS;
}

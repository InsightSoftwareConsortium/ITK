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
#include "itkBoxSigmaImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkFilterWatcher.h"

int itkBoxSigmaImageFilterTest(int ac, char* av[] )
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if(ac < 4)
    {
    std::cerr << "Usage: " << av[0] << " InputImage BaselineImage radius" << std::endl;
    return -1;
    }

  typedef itk::Image<unsigned char, 2> ImageType;

  typedef itk::ImageFileReader<ImageType> ReaderType;
  ReaderType::Pointer input  = ReaderType::New();
  input->SetFileName(av[1]);

  // Create a filter
  typedef itk::BoxSigmaImageFilter<ImageType,ImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();
  FilterWatcher filterWatch(filter);

  typedef FilterType::RadiusType RadiusType;

  // test default values
  RadiusType r1;
  r1.Fill( 1 );
  if ( filter->GetRadius() != r1 )
    {
    std::cerr << "Wrong default Radius." << std::endl;
    return EXIT_FAILURE;
    }

  // set radius with a radius type
  RadiusType r5;
  r5.Fill( 5 );
  filter->SetRadius( r5 );
  if ( filter->GetRadius() != r5 )
    {
    std::cerr << "Radius value is not the expected one: r5." << std::endl;
    return EXIT_FAILURE;
    }

  // set radius with an integer
  filter->SetRadius( 1 );
  if ( filter->GetRadius() != r1 )
    {
    std::cerr << "Radius value is not the expected one: r1." << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    int r = atoi( av[3] );
    filter->SetInput(input->GetOutput());
    filter->SetRadius( r );
    filter->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return EXIT_FAILURE;
    }

  // Generate test image
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( av[2] );
  writer->Update();

  return EXIT_SUCCESS;
}

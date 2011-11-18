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
#include "itkMaskedRankImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkFilterWatcher.h"

int itkMaskedRankImageFilterTest(int ac, char* av[] )
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if(ac < 5)
    {
    std::cerr << "Usage: " << av[0] << " InputImage maskImage BaselineImage radius" << std::endl;
    return -1;
    }

  typedef itk::Image<unsigned char, 2> ImageType;

  typedef itk::ImageFileReader<ImageType> ReaderType;
  ReaderType::Pointer input  = ReaderType::New();
  input->SetFileName(av[1]);

  ReaderType::Pointer input2  = ReaderType::New();
  input2->SetFileName(av[2]);

  // Create a filter
  typedef itk::MaskedRankImageFilter<ImageType,ImageType,ImageType> FilterType;
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
  if ( filter->GetRank() != 0.5 )
    {
    std::cerr << "Wrong default Rank." << std::endl;
    return EXIT_FAILURE;
    }
  if ( filter->GetMaskValue() != 255 )
    {
    std::cerr << "Wrong default mask value." << std::endl;
    return EXIT_FAILURE;
    }
  if ( filter->GetFillValue() != 0 )
    {
    std::cerr << "Wrong default fill value." << std::endl;
    return EXIT_FAILURE;
    }
  if ( filter->GetBackgroundMaskValue() != 0 )
    {
    std::cerr << "Wrong default background mask value." << std::endl;
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

  filter->SetRank( 0.25 );
  if ( filter->GetRank() != 0.25 )
    {
    std::cerr << "Rank value is not the expected one: " << filter->GetRank() << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetMaskValue( 1 );
  if ( filter->GetMaskValue() != 1 )
    {
    std::cerr << "Mask value value is not the expected one: " << filter->GetMaskValue() << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetFillValue( 1 );
  if ( filter->GetFillValue() != 1 )
    {
    std::cerr << "Fill value value is not the expected one: " << filter->GetFillValue() << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetBackgroundMaskValue( 1 );
  if ( filter->GetBackgroundMaskValue() != 1 )
    {
    std::cerr << "Background mask value value is not the expected one: " << filter->GetBackgroundMaskValue() << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    int r = atoi( av[4] );
    filter->SetInput(input->GetOutput());
    filter->SetMaskImage(input2->GetOutput());
    filter->SetRadius( r );
    filter->SetRank( 0.5 );
    filter->SetMaskValue( 255 );
    filter->SetFillValue( 1 );
    filter->SetBackgroundMaskValue( 0 );
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
  writer->SetFileName( av[3] );
  writer->Update();

  return EXIT_SUCCESS;
}

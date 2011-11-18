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

#include "itkVectorGradientMagnitudeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPipelineMonitorImageFilter.h"

int itkVectorGradientMagnitudeImageFilterTest3(int ac, char* av[] )
{
  typedef itk::RGBPixel<unsigned char> RGBPixelType;
  typedef itk::Image<RGBPixelType, 3>  RGBImageType;

  typedef itk::PipelineMonitorImageFilter<RGBImageType>                Monitor1Filter;
  typedef itk::VectorGradientMagnitudeImageFilter<RGBImageType>        FilterType;
  typedef itk::ImageFileReader<RGBImageType>                           ReaderType;
  typedef itk::PipelineMonitorImageFilter<FilterType::OutputImageType> Monitor2Filter;
  typedef itk::ImageFileWriter<FilterType::OutputImageType>            WriterType;

  if(ac < 4)
    {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage Mode\n";
    return EXIT_FAILURE;
    }

  // Create a reader and filter
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(av[1]);

  Monitor1Filter::Pointer monitor1 = Monitor1Filter::New();
  monitor1->SetInput( reader->GetOutput() );
  monitor1->ClearPipelineOnGenerateOutputInformationOff();

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( monitor1->GetOutput() );

  const int mode = ::atoi( av[3] );

  if ( mode == 1)
    {
    filter->SetUsePrincipleComponentsOn();
    }
  else
    {
    filter->SetUsePrincipleComponentsOff();
    }

  Monitor2Filter::Pointer monitor2 = Monitor2Filter::New();
  monitor2->SetInput( filter->GetOutput() );

  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( av[2] );
  writer->SetInput( monitor2->GetOutput() );
  writer->SetNumberOfStreamDivisions( 4 );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return EXIT_FAILURE;
    }
  catch (...)
    {
    std::cerr << "Some other exception occurred" << std::endl;
    return EXIT_FAILURE;
    }

  if ( !monitor1->VerifyAllInputCanStream(4-1) )
    {
    std::cout << "Input of filter failed to execute as expected!" << std::endl;
    std::cout << monitor1;
    return EXIT_FAILURE;
    }

  // this verifies that the pipeline was executed as expected allong
  // with correct region propagation and output information
  if (!monitor2->VerifyAllInputCanStream(4) )
    {
    std::cout << "Filter failed to execute as expected!" << std::endl;
    std::cout << monitor2;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

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

#include "itkConvolutionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkStreamingImageFilter.h"

int itkConvolutionImageFilterTestInt(int argc, char * argv[])
{

  if ( argc < 4 )
    {
    std::cout << "Usage: " << argv[0]
      << " inputImage kernelImage outputImage [normalizeImage] [outputRegionMode]" << std::endl;
    return EXIT_FAILURE;
    }

  const int ImageDimension = 2;

  typedef unsigned char                          PixelType;
  typedef itk::Image<PixelType, ImageDimension>  ImageType;
  typedef itk::ImageFileReader<ImageType>        ReaderType;

  ReaderType::Pointer reader1 = ReaderType::New();
  reader1->SetFileName( argv[1] );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( argv[2] );

  typedef itk::ConvolutionImageFilter<ImageType> ConvolutionFilterType;
  ConvolutionFilterType::Pointer convolver = ConvolutionFilterType::New();
  convolver->SetInput( reader1->GetOutput() );
  convolver->SetKernelImage( reader2->GetOutput() );

  itk::SimpleFilterWatcher watcher(convolver, "filter");

  if ( argc >= 5 )
    {
    convolver->SetNormalize( static_cast<bool>( atoi( argv[4] ) ) );
    }

  if ( argc >= 6 )
    {
    std::string outputRegionMode( argv[5] );
    if ( outputRegionMode == "SAME" )
      {
      convolver->SetOutputRegionModeToSame();
      std::cout << "OutputRegionMode set to SAME." << std::endl;
      }
    else if ( outputRegionMode == "VALID" )
      {
      convolver->SetOutputRegionModeToValid();
      std::cout << "OutputRegionMode set to VALID." << std::endl;
      }
    else
      {
      std::cerr << "Invalid OutputRegionMode '" << outputRegionMode << "'." << std::endl;
      std::cerr << "Valid values are SAME or VALID." << std::endl;
      return EXIT_FAILURE;
      }
    }

  typedef itk::PipelineMonitorImageFilter< ImageType > MonitorFilter;

  MonitorFilter::Pointer monitor = MonitorFilter::New();
  monitor->SetInput( convolver->GetOutput() );

  const unsigned int numberOfStreamDivisions = 4;
  itk::StreamingImageFilter< ImageType, ImageType >::Pointer streamingFilter =
    itk::StreamingImageFilter< ImageType, ImageType >::New();
  streamingFilter->SetNumberOfStreamDivisions( numberOfStreamDivisions );
  streamingFilter->SetInput( monitor->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[3] );
  writer->SetInput( streamingFilter->GetOutput() );

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  if ( !monitor->VerifyAllInputCanStream( numberOfStreamDivisions ) )
    {
    std::cerr << "ConvolutionImageFilter failed to stream as expected!" << std::endl;
    std::cerr << monitor;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

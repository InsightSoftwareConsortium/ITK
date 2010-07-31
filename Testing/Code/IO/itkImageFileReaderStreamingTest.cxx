/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileReaderStreamingTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <fstream>
#include "itkImageFileReader.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkStreamingImageFilter.h"

int itkImageFileReaderStreamingTest(int argc, char* argv[])
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " input [expected-to-stream 1|0 [ force-no-streaming 1|0] ]" << std::endl;
    return EXIT_FAILURE;
    }

  unsigned int numberOfDataPieces = 4;

  bool expectedToStream = true;
  if( argc > 2 )
    {
    if( atoi(argv[2]) != 1 )
      {
      expectedToStream = false;
      }
    }

  bool forceNoStreamingInput = false;
  if( argc > 3 )
    {
    if( atoi(argv[3]) == 1 )
      {
      forceNoStreamingInput = true;
      }
    }


  typedef unsigned char             PixelType;
  typedef itk::Image<PixelType,3>   ImageType;

  typedef itk::ImageFileReader<ImageType>         ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->SetUseStreaming( true );

  typedef itk::PipelineMonitorImageFilter<ImageType> MonitorFilter;
  MonitorFilter::Pointer monitor = MonitorFilter::New();
  monitor->SetInput(reader->GetOutput());


  typedef itk::StreamingImageFilter<ImageType, ImageType> StreamingFilter;
  StreamingFilter::Pointer streamer = StreamingFilter::New();
  streamer->SetInput(monitor->GetOutput());
  streamer->SetNumberOfStreamDivisions(numberOfDataPieces);


  try
    {
    if (forceNoStreamingInput)
      {
      monitor->UpdateLargestPossibleRegion();
      }
    else
      {
      streamer->Update();
      }
    }
  catch( itk::ExceptionObject & err )
    {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }


  bool passed = true;
  if( expectedToStream )
    {
    if( !monitor->VerifyAllInputCanStream(numberOfDataPieces) )
      {
      passed = false;
      }
    }
  else
    {
    if( !monitor->VerifyAllIputCanNotStream() )
      {
      passed = false;
      }
    }


  if( !passed )
    {
    std::cout << monitor << std::endl;
    std::cout << "pipeline did not execute as expected!" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

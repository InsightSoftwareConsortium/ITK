/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileWriterStreamingTest1.cxx
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
#include "itkImageFileWriter.h"
#include "itkPipelineMonitorImageFilter.h"

int itkImageFileWriterStreamingTest1(int argc, char* argv[])
{
  if( argc < 3 )
    { 
    std::cerr << "Usage: " << argv[0] << " input output [existingFile [ no-streaming 1|0] ]" << std::endl;
    return EXIT_FAILURE;
    }
      
  // We remove the output file
  if (argc == 3)
    {
      itksys::SystemTools::RemoveFile( argv[2] ); 
    } 
  else 
    {
      // copy this file to over write
      itksys::SystemTools::CopyAFile(argv[3], argv[2]);
    } 

  
  unsigned int numberOfDataPieces = 4;
 

  bool forceNoStreamingInput = false;
  if ( argc > 4 ) 
    {
      if ( atoi( argv[4] ) == 1 ) 
          forceNoStreamingInput = true;
    }


  typedef unsigned char            PixelType;
  typedef itk::Image<PixelType,3>   ImageType;

  typedef itk::ImageFileReader<ImageType>         ReaderType;
  typedef itk::ImageFileWriter< ImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->SetUseStreaming( true );
    
  typedef itk::PipelineMonitorImageFilter<ImageType> MonitorFilter;
  MonitorFilter::Pointer monitor = MonitorFilter::New();
  monitor->SetInput(reader->GetOutput());

  if ( forceNoStreamingInput )
    {
    monitor->UpdateLargestPossibleRegion();
    monitor->VerifyAllIputCanNotStream();
    }

  // Setup the writer
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput(monitor->GetOutput());
  writer->SetNumberOfStreamDivisions(numberOfDataPieces);
  
    
  try
    {
      writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
      std::cerr << "ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
    }
   

  bool passed = true;
  if( !forceNoStreamingInput )
    {
    if( !monitor->VerifyAllInputCanStream(numberOfDataPieces) )
      {
      passed = false;
      }
    }
  else
    {
    if( !monitor->VerifyAllNoUpdate() )
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

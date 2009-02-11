/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGDCMSeriesStreamReadImageWrite.cxx
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

#include "itkImageSeriesReader.h"
#include "itkImageFileWriter.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkStreamingImageFilter.h"

int main( int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " DicomDirectory  outputFile ";
    std::cerr << " [expected-to-stream 1|0 [ force-no-streaming 1|0] ]" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::Image<unsigned short,3>            ImageType;
  typedef itk::ImageSeriesReader< ImageType >     ReaderType;
  typedef itk::GDCMImageIO                        ImageIOType;
  typedef itk::GDCMSeriesFileNames                SeriesFileNames;

  ImageIOType::Pointer gdcmIO = ImageIOType::New();
  SeriesFileNames::Pointer filenameGenerator = SeriesFileNames::New();

  filenameGenerator->SetInputDirectory( argv[1] );

  ReaderType::Pointer reader = ReaderType::New();

  const ReaderType::FileNamesContainer & filenames =
    filenameGenerator->GetInputFileNames();

  unsigned int numberOfFilenames =  filenames.size();
  std::cout << numberOfFilenames << std::endl;
  for(unsigned int fni = 0; fni<numberOfFilenames; fni++)
    {
    std::cout << "filename # " << fni << " = ";
    std::cout << filenames[fni] << std::endl;
    }

  reader->SetFileNames( filenames );
  reader->SetImageIO( gdcmIO );

  unsigned int numberOfDataPieces = 4;

  bool expectedToStream = true;
  if( argc > 3 )
    {
    if( atoi(argv[3]) != 1 )
      {
      expectedToStream = false;
      }
    }

  bool forceNoStreamingInput = false;
  if( argc > 4 )
    {
    if( atoi( argv[4] ) == 1 )
      {
      forceNoStreamingInput = true;
      }
    }

  typedef itk::PipelineMonitorImageFilter<ImageType> MonitorFilter;
  MonitorFilter::Pointer monitor = MonitorFilter::New();
  monitor->SetInput( reader->GetOutput() );

  typedef itk::StreamingImageFilter<ImageType, ImageType> StreamingFilter;
  StreamingFilter::Pointer streamer = StreamingFilter::New();
  streamer->SetInput( monitor->GetOutput() );
  streamer->SetNumberOfStreamDivisions( numberOfDataPieces );

  try
    {
    if( forceNoStreamingInput )
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
    std::cerr << monitor << std::endl;
    std::cerr << "pipeline did not execute as expected!" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
  writer->SetInput( reader->GetOutput() );

  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &excp)
    {
    std::cerr << "Exception thrown while writing the image" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

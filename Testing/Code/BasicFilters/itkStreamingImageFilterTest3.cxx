/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStreamingImageFilterTest3.cxx
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

#include <iostream>
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkImageRegionMultidimensionalSplitter.h"
#include "../IO/itkPipelineMonitorImageFilter.h"


int itkStreamingImageFilterTest3(int argc, char*argv [] )
{
   if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile outputImageFile numberOfStreamDivisions" << std::endl;
    return EXIT_FAILURE;
    }
   
   const std::string inputFilename = argv[1];
   const std::string outputFilename = argv[2];
   unsigned int numberOfStreamDivisions = atoi(argv[3]);

  typedef unsigned char PixelType;  
  typedef itk::Image< PixelType, 2 > ImageType;
  
  typedef itk::ImageFileReader<ImageType> ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( inputFilename );

  typedef itk::ShiftScaleImageFilter<ImageType, ImageType> SomeFilter;
  SomeFilter::Pointer filter = SomeFilter::New();
  filter->SetInput( reader->GetOutput() );
  
  // monitor what's going on
  itk::PipelineMonitorImageFilter<ImageType>::Pointer monitor;
  monitor = itk::PipelineMonitorImageFilter<ImageType>::New();
  monitor->SetInput( filter->GetOutput() );

  itk::ImageRegionMultidimensionalSplitter<2>::Pointer splitter;
  splitter = itk::ImageRegionMultidimensionalSplitter<2>::New();
  
  itk::StreamingImageFilter<ImageType, ImageType>::Pointer streamer;
  streamer = itk::StreamingImageFilter<ImageType, ImageType>::New();
  streamer->SetInput( monitor->GetOutput() );
  streamer->SetNumberOfStreamDivisions( numberOfStreamDivisions );
  streamer->SetRegionSplitter( splitter );

  
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( outputFilename );
  writer->SetInput( streamer->GetOutput() );
  writer->Update();
  

  unsigned int expectedNumberOfStreams =
    splitter->GetNumberOfSplits(streamer->GetOutput()->GetLargestPossibleRegion(), numberOfStreamDivisions);
 
  std::cout << "ExpectedNumberOfStreams: " << expectedNumberOfStreams << std::endl;
  
  if (!monitor->VerifyAllInputCanStream(expectedNumberOfStreams))
    {
    std::cout << "Filter failed to execute as expected!" << std::endl;
    std::cout << monitor;
    return EXIT_FAILURE;
    }
  
  return EXIT_SUCCESS;    


}

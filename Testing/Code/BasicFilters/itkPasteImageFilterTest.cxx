/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPasteImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <fstream>
#include "itkPasteImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkImageRegionMultidimensionalSplitter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"

int itkPasteImageFilterTest(int ac, char* av[] )
{
  if(ac < 4)
    {
    std::cerr << "Usage: " << av[0] << " DestinationImage SourceImage OutputImage\n";
    return -1;
    }

  typedef unsigned char PixelType;
  typedef itk::Image<PixelType, 2> myImage;
  itk::ImageFileReader<myImage>::Pointer dest 
    = itk::ImageFileReader<myImage>::New();
  dest->SetFileName(av[1]);

  itk::ImageFileReader<myImage>::Pointer src 
    = itk::ImageFileReader<myImage>::New();
  src->SetFileName(av[2]);
  
  // Create a filter
  typedef itk::PasteImageFilter<myImage> FilterType;

  FilterType::Pointer filter = FilterType::New();
  filter->SetDestinationImage(dest->GetOutput());
  filter->SetSourceImage( src->GetOutput() );

  FilterType::InputImageIndexType destIndex;
  destIndex[0] = 100;
  destIndex[1] = 70;
  filter->SetDestinationIndex( destIndex );

  FilterType::InputImageIndexType srcIndex;
  FilterType::InputImageSizeType srcSize;
  FilterType::InputImageRegionType srcRegion;

  srcIndex[0] = 20;
  srcIndex[1] = 40;

  srcSize[0] = 60;
  srcSize[1] = 40;

  srcRegion.SetIndex( srcIndex );
  srcRegion.SetSize( srcSize );

  filter->SetSourceRegion( srcRegion );


  // We'll tie this to a streamer to really exercise the paste code
  typedef itk::ImageRegionMultidimensionalSplitter<2> SplitterType;
  SplitterType::Pointer splitter = SplitterType::New();
  //splitter->DebugOn();

  typedef itk::StreamingImageFilter<myImage, myImage> StreamerType;
  StreamerType::Pointer streamer = StreamerType::New();
  streamer->SetInput( filter->GetOutput() );
  streamer->SetNumberOfStreamDivisions( 25 );
  streamer->SetRegionSplitter( splitter );
  

  // Test itkGetMacros
  myImage::IndexType  value  = filter->GetDestinationIndex();
  myImage::RegionType value2 = filter->GetSourceRegion();
  

  try
    {
    streamer->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return -1;
    }
  catch (...)
    {
    std::cerr << "Some other exception occurred" << std::endl;
    return -2;
    }

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
    writer = itk::ImageFileWriter<myImage>::New();
    writer->SetInput( streamer->GetOutput() );
    writer->SetFileName( av[3] );
    writer->Update();

  return 0;
}

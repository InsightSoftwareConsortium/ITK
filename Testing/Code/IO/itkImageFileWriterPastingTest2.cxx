/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageFileWriterPastingTest2.cxx
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

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPipelineMonitorImageFilter.h"

int itkImageFileWriterPastingTest2(int argc, char* argv[])
{
  if( argc < 3 )
    { 
    std::cerr << "Usage: " << argv[0] << " input output [existingFile]" << std::endl;
    return EXIT_FAILURE;
    }
      
  // We remove the output file
  if (argc == 3)
    {
      itksys::SystemTools::RemoveFile(argv[2]); 
    } 
  else 
    {
      // copy this file to over write
      itksys::SystemTools::CopyAFile(argv[3], argv[2]);
    } 

  typedef unsigned char            PixelType;
  typedef itk::Image<PixelType,3>   ImageType;

  typedef itk::ImageFileReader<ImageType>         ReaderType;
  typedef itk::ImageFileWriter< ImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->SetUseStreaming( true );
  
  // We decide how we want to read the image and we split accordingly
  // The image is read slice by slice
  reader->GenerateOutputInformation();

  ImageType::RegionType largestRegion;
  largestRegion = reader->GetOutput()->GetLargestPossibleRegion().GetSize();
  
  
  ImageType::IndexType pasteIndex;
  pasteIndex[0] = largestRegion.GetIndex()[0]+largestRegion.GetSize()[0]/3;
  pasteIndex[1] = largestRegion.GetIndex()[1]+largestRegion.GetSize()[1]/3;
  pasteIndex[2] = largestRegion.GetIndex()[2]+largestRegion.GetSize()[2]/3;
  ImageType::SizeType pasteSize;
  pasteSize[0] = largestRegion.GetSize()[0]/3;
  pasteSize[1] = largestRegion.GetSize()[1]/3;
  pasteSize[2] = largestRegion.GetSize()[2]/3;

  
  typedef itk::PipelineMonitorImageFilter<ImageType> MonitorFilter;
  MonitorFilter::Pointer monitor = MonitorFilter::New();
  monitor->SetInput(reader->GetOutput());

  // Setup the writer
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(monitor->GetOutput());

  // create a vaild region from the largest
  itk::ImageIORegion  ioregion(3);
  itk::ImageIORegion::IndexType index;
  
  index.push_back(pasteIndex[0]);
  index.push_back(pasteIndex[1]);
  index.push_back(pasteIndex[2]);
  ioregion.SetIndex(index);
  itk::ImageIORegion::SizeType size;
  size.push_back(pasteSize[0]);
  size.push_back(pasteSize[1]);
  size.push_back(pasteSize[2]);
  ioregion.SetSize(size);
  writer->SetIORegion(ioregion);
  
    
  try
    {
      writer->Update();
    }
  catch( itk::ExceptionObject & err )
    {
      if (argc >= 4 && strcmp(argv[4], "keep") )
        {
          // we expect this to fail
          return EXIT_SUCCESS;
        }
      std::cerr << "ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
    }
   
  //check that the pipeline executed as expected
  if (monitor->GetNumberOfUpdates() != 1) {
    std::cerr << "pipeline did not execute as expected" << std::endl;
    return EXIT_FAILURE;
  }
  
  std::cout << monitor;
  
  return EXIT_SUCCESS;
}

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOtsuThresholdImageFilterTest.cxx
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

#include "itkOtsuThresholdImageFilter.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"

int itkOtsuThresholdImageFilterTest(int argc, char* argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImageFile outputImageFile";  
    std::cerr << " numberOfHistogramBins";  
    std::cerr << std::endl;  
    return EXIT_FAILURE;
    }

  typedef  short  InputPixelType;
  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;

  typedef itk::OtsuThresholdImageFilter<
               InputImageType, OutputImageType >  FilterType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  FilterType::Pointer filter = FilterType::New();
  WriterType::Pointer writer = WriterType::New();

  FilterWatcher watcher(filter);

  reader->SetFileName( argv[1] );
  filter->SetInput( reader->GetOutput() );
  filter->SetNumberOfHistogramBins (atoi(argv[3]));
  writer->SetInput( filter->GetOutput() );

  filter->Update();
  std::cout << "Computed Threshold is: " << filter->GetThreshold() << std::endl;
  writer->SetFileName( argv[2] );
  writer->Update();

  return EXIT_SUCCESS;
}

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryThresholdImageFilterTest2.cxx
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



#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkStatisticsImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"


int itkBinaryThresholdImageFilterTest2(int ac, char* av[] ) 
{
  if(ac < 4)
    {
    std::cerr << "Usage: " << av[0] <<" InputImage1 InputImage2 OutputImage\n";
    return -1;
    }

  // Threshold one image based on the statistics of another image
  //
  //
  
  // Define the dimension of the images
  const unsigned int ImageDimension = 2;

  // Declare the types of the images
  typedef itk::Image<unsigned char, ImageDimension>  ImageType;
  typedef itk::Image<double, ImageDimension>  FloatImageType;

  // File reader and writer
  typedef itk::ImageFileReader<FloatImageType> ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( av[1] );

  ReaderType::Pointer reader2 = ReaderType::New();
  reader2->SetFileName( av[2] );
  
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( av[3] );
  
  // Declare the filter types
  typedef itk::StatisticsImageFilter<FloatImageType>  StatisticsType;
  typedef itk::BinaryThresholdImageFilter<FloatImageType, ImageType>  ThresholdType;
            
  // Create the filters                               
  StatisticsType::Pointer statistics = StatisticsType::New();
  ThresholdType::Pointer threshold = ThresholdType::New();

  // connect the standard pipeline connections
  statistics->SetInput( reader2->GetOutput() );
  threshold->SetInput( reader->GetOutput() );

  // print before assigning thresholds
  threshold->Print(std::cout);

  // now connect the inputs and outputs that are decorated scalars
  threshold->SetUpperThresholdInput( statistics->GetMeanOutput() );
  threshold->SetLowerThresholdInput( statistics->GetMinimumOutput() );
  
  // connect the writer
  writer->SetInput( threshold->GetOutput() );
  
  // Execute the filter
  try
    {
    writer->Update();
    }
  catch(...)
    {
    std::cerr << "Caught an unexpected exception. " << std::endl;
    std::cerr << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}





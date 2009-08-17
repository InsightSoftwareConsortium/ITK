/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCannyEdgeDetectionImageFilterTest2.cxx
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
#include "itkCannyEdgeDetectionImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkVector.h"
#include "itkSimpleFilterWatcher.h"
#include "itkRescaleIntensityImageFilter.h"

// This test is was written to test bug 9431
//
// It run a filter multiple times and expects the output to be the
// same when run with the same parameters. The two output images
// should be identical
int itkCannyEdgeDetectionImageFilterTest2(int argc, char * argv[] )
{
  if(argc < 4)
    {
    std::cerr << "Usage: " << argv[0] << " InputImage OutputImage1 OutputImage2\n";
    return -1;
    }

  const unsigned int dimension = 2;
  typedef float PixelType;
  typedef itk::Image<float, dimension> InputImage;
  typedef itk::Image<unsigned char, dimension> OutputImage;

  itk::ImageFileReader<InputImage>::Pointer input 
    = itk::ImageFileReader<InputImage>::New();
  input->SetFileName(argv[1]);
  
  // Set up filter
  itk::CannyEdgeDetectionImageFilter<InputImage, InputImage>::Pointer 
    filter =
    itk::CannyEdgeDetectionImageFilter<InputImage, InputImage>::New();
  filter->SetInput(input->GetOutput());  
  filter->SetVariance(1.0f);
  filter->SetMaximumError(.01f);
  
  filter->SetUpperThreshold(25);
  filter->SetLowerThreshold(10);
  

  itk::RescaleIntensityImageFilter<InputImage, OutputImage>::Pointer
    rescale =
    itk::RescaleIntensityImageFilter<InputImage, OutputImage>::New();
  rescale->SetInput(filter->GetOutput());
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);

  itk::ImageFileWriter<OutputImage>::Pointer writer;
  writer = itk::ImageFileWriter<OutputImage>::New();
  writer->SetInput( rescale->GetOutput() );

  
  try
    {
    // Generate test image
    writer->SetFileName( argv[2] );
    writer->Update();

    // set canny filter to another value
    filter->SetUpperThreshold(20);
    filter->SetLowerThreshold(5);
    
    rescale->Update();
    
    // set it back expecting the same results
    filter->SetUpperThreshold(25);
    filter->SetLowerThreshold(10);
    
    // Generate test image
    writer->SetFileName( argv[3] );
    writer->Update();

    
    }
  catch(itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return EXIT_FAILURE;
    } 




  return EXIT_SUCCESS;   
}

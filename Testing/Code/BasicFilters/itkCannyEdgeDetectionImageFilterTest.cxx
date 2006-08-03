/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCannyEdgeDetectionImageFilterTest.cxx
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

int itkCannyEdgeDetectionImageFilterTest(int argc, char * argv[] )
{
  if(argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " InputImage OutputImage\n";
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
  itk::SimpleFilterWatcher watcher(filter);
  filter->SetInput(input->GetOutput());
  filter->SetUpperThreshold(30);
  filter->SetLowerThreshold(10);
  filter->SetThreshold(30);
  filter->SetVariance(1.0f);
  filter->SetMaximumError(.01f);

  itk::RescaleIntensityImageFilter<InputImage, OutputImage>::Pointer
    rescale =
    itk::RescaleIntensityImageFilter<InputImage, OutputImage>::New();
  rescale->SetInput(filter->GetOutput());
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);
  
  try
    {
    // Generate test image
    itk::ImageFileWriter<OutputImage>::Pointer writer;
      writer = itk::ImageFileWriter<OutputImage>::New();
      writer->SetInput( rescale->GetOutput() );
      writer->SetFileName( argv[2] );
      writer->Update();
    }
  catch(itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return 1;
    } 
  return 0;   
}

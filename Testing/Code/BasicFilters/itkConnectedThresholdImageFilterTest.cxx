/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedThresholdImageFilterTest.cxx
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
#include "itkConnectedThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPNGImageIO.h"
#include "itkImageRegionIterator.h"
#include "itkFilterWatcher.h"

int itkConnectedThresholdImageFilterTest(int ac, char* av[] )
{
  if(ac < 7)
    {
    std::cerr << "Usage: " << av[0] 
      << " InputImage OutputImage " 
      << "seed_x seed_y "
      << "LowerConnectedThreshold UpperConnectedThreshold "
      << "Connectivity[1=Full,0=Face]\n"
      << " ITK_USE_REVIEW must be on to be able to use full connectivity.\n";
    return -1;
    }

  typedef unsigned char PixelType;
  typedef itk::Image<PixelType, 2> myImage;

  itk::ImageFileReader<myImage>::Pointer input 
    = itk::ImageFileReader<myImage>::New();
  input->SetFileName(av[1]);
  
  // Create a filter
  typedef itk::ConnectedThresholdImageFilter<myImage,myImage> FilterType;

  FilterType::Pointer filter = FilterType::New();
  FilterWatcher watcher(filter);

  filter->SetInput(input->GetOutput());
  FilterType::IndexType seed; seed[0] = atoi(av[3]); seed[1] = atoi(av[4]);
  filter->AddSeed(seed);
  filter->SetLower(atoi(av[5]));
  filter->SetUpper(atoi(av[6]));
  filter->SetReplaceValue(255);
  
#ifdef ITK_USE_REVIEW
  // Test the use of full (8 connectivity in 2D) on this image. 
  if (ac > 7)
    {
    filter->SetConnectivity( atoi(av[7]) ? 
        FilterType::FullConnectivity : FilterType::FaceConnectivity );
    }
#endif
  
  try
    {
    input->Update();
    filter->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return -1;
    }

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( av[2] );
  writer->Update();

  return EXIT_SUCCESS;
}

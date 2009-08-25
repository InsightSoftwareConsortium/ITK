/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoxMeanImageFilterTest.cxx
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
#include "itkBoxMeanImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkNumericTraits.h"
#include "itkFilterWatcher.h"

int itkBoxMeanImageFilterTest(int ac, char* av[] )
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if(ac < 4)
    {
    std::cerr << "Usage: " << av[0] << " InputImage BaselineImage radius" << std::endl;
    return -1;
    }

  typedef itk::Image<unsigned char, 2> ImageType;
  
  typedef itk::ImageFileReader<ImageType> ReaderType;
  ReaderType::Pointer input  = ReaderType::New();
  input->SetFileName(av[1]);
  
  // Create a filter
  typedef itk::BoxMeanImageFilter<ImageType,ImageType> FilterType;
  FilterType::Pointer filter = FilterType::New();
  FilterWatcher filterWatch(filter);

  typedef FilterType::RadiusType RadiusType;

  // test default values
  RadiusType r1;
  r1.Fill( 1 );
  if ( filter->GetRadius() != r1 )
    {
    std::cerr << "Wrong default Radius." << std::endl;
    return EXIT_FAILURE;
    }
    
  // set radius with a radius type
  RadiusType r5;
  r5.Fill( 5 );
  filter->SetRadius( r5 );
  if ( filter->GetRadius() != r5 )
    {
    std::cerr << "Radius value is not the expected one: r5." << std::endl;
    return EXIT_FAILURE;
    }

  // set radius with an integer
  filter->SetRadius( 1 );
  if ( filter->GetRadius() != r1 )
    {
    std::cerr << "Radius value is not the expected one: r1." << std::endl;
    return EXIT_FAILURE;
    }

  try
    {
    int r = atoi( av[3] );
    filter->SetInput(input->GetOutput());
    filter->SetRadius( r );
    filter->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return EXIT_FAILURE;
    }

  // Generate test image
  typedef itk::ImageFileWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( av[2] );
  writer->Update();

  return EXIT_SUCCESS;
}

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNoiseImageFilterTest.cxx
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
#include "itkNoiseImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkPNGImageIOFactory.h"
#include "itkTextOutput.h"
#include "itkImageRegionIterator.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkNumericTraits.h"
#include "itkFilterWatcher.h"

int itkNoiseImageFilterTest(int ac, char* av[] )
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage BaselineImage\n";
    return -1;
    }

  itk::Size<2> radius;
  typedef itk::Image<unsigned short, 2> myImageIn;
  typedef itk::Image<float, 2> myImageOut;
  typedef itk::Image<unsigned char, 2> myImageChar;
  itk::ImageFileReader<myImageIn>::Pointer input 
    = itk::ImageFileReader<myImageIn>::New();
  input->SetFileName(av[1]);
  
  // Create a filter
  typedef itk::NoiseImageFilter<myImageIn,myImageOut> FilterType;

  FilterType::Pointer filter = FilterType::New();
  FilterWatcher filterWatch(filter);

  typedef itk::RescaleIntensityImageFilter<myImageOut,myImageChar> RescaleFilterType; 

  RescaleFilterType::Pointer rescale = RescaleFilterType::New();
  rescale->SetOutputMinimum(0);
  rescale->SetOutputMaximum(255);
  rescale->SetInput( filter->GetOutput() );

  try
    {
    radius.Fill(5);
    filter->SetInput (input->GetOutput());
    filter->SetRadius (radius);
    filter->Update();
    }
  catch (itk::ExceptionObject& e)
    {
    std::cerr << "Exception detected: "  << e.GetDescription();
    return -1;
    }

  // Generate test image
  itk::ImageFileWriter<myImageChar>::Pointer writer;
    writer = itk::ImageFileWriter<myImageChar>::New();
    writer->SetInput( rescale->GetOutput() );
    writer->SetFileName( av[2] );
    writer->Update();

    return EXIT_SUCCESS;
}

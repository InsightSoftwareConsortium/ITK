/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConfidenceConnectedImageFilterTest.cxx
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
#include "itkConfidenceConnectedImageFilter.h"
#include "itkImageFileReader.h"
#include "itkPNGImageIO.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkSimpleFilterWatcher.h"
#include "itkOrientedImage.h"

int itkConfidenceConnectedImageFilterTest(int ac, char* av[] )
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if(ac < 5)
    {
    std::cerr << "Usage: " << av[0] << " InputImage BaselineImage seed_x seed_y\n";
    return -1;
    }

  typedef unsigned char PixelType;
  typedef itk::OrientedImage<PixelType, 2> myImage;
  itk::ImageFileReader<myImage>::Pointer input 
    = itk::ImageFileReader<myImage>::New();
  input->SetFileName(av[1]);
  
  // Create a filter
  typedef itk::ConfidenceConnectedImageFilter<myImage,myImage> FilterType;

  FilterType::Pointer filter = FilterType::New();
  itk::SimpleFilterWatcher filterWatch(filter);

  filter->SetInput(input->GetOutput());
  filter->SetInitialNeighborhoodRadius( 3 ); // measured in pixels

  FilterType::IndexType seed; seed[0] = atoi(av[3]); seed[1] = atoi(av[4]);
  //  FilterType::IndexType seed; seed[0] = 56; seed[1] = 90;
  //  FilterType::IndexType seed; seed[0] = 96; seed[1] = 214;
  filter->SetSeed(seed);
  filter->SetMultiplier(2.5);
  filter->SetReplaceValue(255);
  filter->SetNumberOfIterations(10);
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

  // Test the GetMacros
  double doubleMultiplier = filter->GetMultiplier();
  std::cout << "filter->GetMultiplier(): " 
            << doubleMultiplier
            << std::endl;

  unsigned int uintNumberOfIterations = filter->GetNumberOfIterations();
  std::cout << "filter->GetNumberOfIterations(): "
            << uintNumberOfIterations 
            << std::endl;

  PixelType pixelReplaceValue = filter->GetReplaceValue();
  std::cout << "filter->GetReplaceValue(): "
            << static_cast<itk::NumericTraits<PixelType>::PrintType>(pixelReplaceValue)
            << std::endl;

  const unsigned int cuintInitialNeighborhoodRadius = filter->GetInitialNeighborhoodRadius();
  std::cout << "filter->GetInitialNeighborhoodRadius(): "
            << cuintInitialNeighborhoodRadius
            << std::endl;

  const double mean = filter->GetMean();
  std::cout << "filter->GetMean(): "
            << mean
            << std::endl;

  const double variance = filter->GetVariance();
  std::cout << "filter->GetVariance(): "
            << variance
            << std::endl;

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
    writer = itk::ImageFileWriter<myImage>::New();
    writer->SetInput( filter->GetOutput() );
    writer->SetFileName( av[2] );
    writer->Update();



  // Exercise AddSeed() method
  filter->AddSeed( seed );


    return 0;
}

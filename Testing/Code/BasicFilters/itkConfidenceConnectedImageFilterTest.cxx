/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConfidenceConnectedImageFilterTest.cxx
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
#include "itkConfidenceConnectedImageFilter.h"
#include "itkImageFileReader.h"
#include "itkPNGImageIOFactory.h"
#include "itkTextOutput.h"
#include "itkImageRegionIterator.h"


int itkConfidenceConnectedImageFilterTest(int ac, char** av)
{
  // Comment the following if you want to use the itk text output window
  itk::OutputWindow::SetInstance(itk::TextOutput::New());

  if(ac < 3)
    {
    std::cerr << "Usage: " << av[0] << " InputImage BaselineImage\n";
    return -1;
    }

  // Register one Factory of PNG readers
  itk::PNGImageIOFactory::RegisterOneFactory();
  
  typedef unsigned char PixelType;
  typedef itk::Image<PixelType, 2> myImage;
  itk::ImageFileReader<myImage>::Pointer input 
    = itk::ImageFileReader<myImage>::New();
  input->SetFileName(av[1]);
  
  // Create a filter
  typedef itk::ConfidenceConnectedImageFilter<myImage,myImage> FilterType;

  FilterType::Pointer filter = FilterType::New();
  filter->SetInput(input->GetOutput());
  filter->SetInitialNeighborhoodRadius( 3 ); // measured in pixels

  FilterType::IndexType seed; seed[0] = 165; seed[1] = 90;
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

  // now read the regression image
  itk::ImageFileReader<myImage>::Pointer baseline 
    = itk::ImageFileReader<myImage>::New();
    baseline->SetFileName(av[2]);

  try
    {
    baseline->Update();
    }
  catch (itk::ImageFileReaderException& e)
    {
    std::cerr << "Exception in file reader: "  << e.GetDescription() << std::endl;
    return -1;
    }
  
  // compare the two images
  itk::ImageRegionIterator<myImage> it(filter->GetOutput(),filter->GetOutput()->GetBufferedRegion());
  itk::ImageRegionIterator<myImage> rit(baseline->GetOutput(),baseline->GetOutput()->GetBufferedRegion());
  int status = 0;
  while (!it.IsAtEnd())
    {
    if (it.Get() != rit.Get())
      {
      status++;
      } 
    ++it;
    ++rit;  
    }
  return status;
}

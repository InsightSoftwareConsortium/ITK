/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIsolatedConnectedImageFilterTest.cxx
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
#include "itkIsolatedConnectedImageFilter.h"
#include "itkImageFileReader.h"
#include "itkPNGImageIOFactory.h"
#include "itkImageRegionIterator.h"

int itkIsolatedConnectedImageFilterTest(int ac, char* av[] )
{
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
  typedef itk::IsolatedConnectedImageFilter<myImage,myImage> FilterType;

  FilterType::Pointer filter = FilterType::New();
    filter->SetInput(input->GetOutput());

  FilterType::IndexType seed1;

    seed1[0] = 146; seed1[1] = 88;
    seed1[0] = 115; seed1[1] = 170;
    filter->SetSeed1(seed1);

    seed1[0] = 156; seed1[1] = 97;
    seed1[0] = 107; seed1[1] = 145;
    filter->SetSeed2(seed1);

    filter->SetLower(0);
    filter->SetReplaceValue(255);
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

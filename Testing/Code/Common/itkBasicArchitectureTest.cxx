/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkBasicArchitectureTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>
#include "itkImage.h"
#include "itkScalar.h"
#include "itkVector.h"
#include "itkRandomImageSource.h"
#include "itkShrinkImage.h"
#include "itkWriteVTKImage.h"
#include "itkReadVTKImage.h"

void main()
{
  // Test the creation of an image with native type
  itk::Image<float,2>::Pointer if2 = itk::Image<float,2>::New();

  std::cout << std::endl
            << "Image dimension is " << itk::Image<float,5>::ImageDimension
            << std::endl;
  std::cout << "Image dimension is " << itk::Image<short,1>::ImageDimension
            << std::endl;

  // Begin by creating a simple pipeline
  //
  // Create another source
  itk::ReadVTKImage< itk::Image<itk::Scalar<float>,2> >::Pointer reader;
  reader = itk::ReadVTKImage< itk::Image<itk::Scalar<float>,2> >::New();
  reader->SetFileName("junkInput.vtk");

  // Create a source
  itk::RandomImageSource< itk::Image<itk::Scalar<float>,2> >::Pointer random;
  random = itk::RandomImageSource< itk::Image<itk::Scalar<float>,2> >::New();

  
  // Create a filter
  itk::ShrinkImage< itk::Image<itk::Scalar<float>,2>, itk::Image<itk::Scalar<float>,2> >::Pointer shrink;
  shrink = itk::ShrinkImage< itk::Image<itk::Scalar<float>,2>, itk::Image<itk::Scalar<float>,2> >::New();
  shrink->SetInput(random->GetOutput());
  shrink->SetShrinkFactor(2);
  shrink->DebugOn();

  // Create a mapper
  itk::WriteVTKImage< itk::Image<itk::Scalar<float>,2> >::Pointer writer;
  writer = itk::WriteVTKImage< itk::Image<itk::Scalar<float>,2> >::New();
  writer->SetInput(shrink->GetOutput());
  writer->SetFileName("junkImage.vtk");
  writer->SetFileTypeToASCII();
  writer->DebugOn();
  writer->Write();

  exit(EXIT_SUCCESS);
}




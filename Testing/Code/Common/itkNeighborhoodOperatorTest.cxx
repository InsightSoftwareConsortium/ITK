/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkNeighborhoodOperatorTest.cxx
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
#include "itkRandomImageSource.h"
#include "itkFilterImageSingleOperator.h"
#include "itkWriteVTKImage.h"

int main()
{
  // Create a source
  itk::RandomImageSource< itk::Image<itk::Scalar<float>,2> >::Pointer random;
  random = itk::RandomImageSource< itk::Image<itk::Scalar<float>,2> >::New();

  // Create a neighborhood
  itk::NeighborhoodOperator< itkScalar<float>, 3> *neighborhood
    = itk::NeighborhoodOperator< itkScalar<float, 3>::New();

  // Create a filter
  itk::FilterImageSingleOperator< itkScalar<float>, 3>::Pointer filter;
  filter = itk::FilterImageSingleOperator< itkScalar<float>, 3>::New();
  filter->SetInput( random->GetOutput() );
  filter->SetOperator( neighborhood );
  
  // Create a mapper
  itk::WriteVTKImage< itk::Image<itk::Scalar<float>,2> >::Pointer writer;
  writer = itk::WriteVTKImage< itk::Image<itk::Scalar<float>,2> >::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName("junkImage.vtk");
  writer->SetFileTypeToASCII();
  writer->DebugOn();
  writer->Write();

  return 1;
}




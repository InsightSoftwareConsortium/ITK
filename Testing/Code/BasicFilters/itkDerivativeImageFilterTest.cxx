/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>
#include "itkImage.h"
#include "itkDerivativeImageFilter.h"
#include "itkNullImageToImageFilterDriver.txx"

int main(int argc, char *argv[])
{
  try
    {
      typedef  itk::Image<float, 3> ImageType;

      // Set up filter
      itk::DerivativeImageFilter<ImageType, ImageType>::Pointer filter
        = itk::DerivativeImageFilter<ImageType, ImageType>::New();
      filter->SetOrder(1);
      filter->SetDirection(1);

      // Run Test
      itk::Size<3> sz;
      sz[0]=256;
      sz[1]=256;
      sz[2]=5;
      itk::NullImageToImageFilterDriver< ImageType, ImageType >
        test1;
      test1.SetImageSize(sz);
      test1.SetFilter(filter.GetPointer());
      test1.Execute();
    }
  catch(itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return 1;
    } 
  return 0;   
}

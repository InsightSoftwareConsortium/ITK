/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageGradientMagnitudeTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <iostream>
#include "itkImage.h"
#include "itkFilterImageGradientMagnitude.h"
#include "itkNullImageToImageFilterDriver.txx"

/**
 * This program tests the FilterImageGradientMagnitude object by driving it
 * with a null input and output.  Returns 0 on success and 1 on failure.
 */
int main(int argc, char *argv[])
{
  try
    {
      // Set up filter
      itk::FilterImageGradientMagnitude<float, 4>::Pointer
        filter = itk::FilterImageGradientMagnitude<float, 4>::New();

      // Run Test
      itk::Size<4> sz;
      sz[0] = 25;
      sz[1] = 25;
      sz[2] = 10;
      sz[3] = 5;
      itk::NullImageToImageFilterDriver< itk::Image<float, 4>,
        itk::Image<float, 4> > test1;
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

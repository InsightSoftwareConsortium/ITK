/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageAnisotropicDiffusionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <iostream>
#include "itkImage.h"
#include "itkFilterImageAnisotropicDiffusion.h"
#include "itkNullImageToImageFilterDriver.h"

/**
 * This program tests the FilterImageAnisotropicDiffusion object by driving it
 * with a null input and output.  Returns 0 on success and 1 on failure.
 */
main(int argc, char *argv[])
{
  try
    {
      // Set up filter
      itk::FilterImageAnisotropicDiffusion<float, 4>::Pointer
        filter = itk::FilterImageAnisotropicDiffusion<float, 4>::New();
      filter->SetIterations(1);
      filter->SetConductanceParameter(3.0f);
      filter->SetTimeStep(0.125f);

      // Run Test
      itk::Size<4> sz;
      sz[0] = 25;//atoi(argv[1]);
      sz[1] = 25;//atoi(argv[2]);
      sz[2] = 10;//atoi(argv[3]);
      sz[3] = 5;//atoi(argv[4]);
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

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDiscreteGaussianImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>
#include "itkImage.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkNullImageToImageFilterDriver.txx"
#include "itkVector.h"
#include "itkFilterWatcher.h"

int itkDiscreteGaussianImageFilterTest(int , char * [] )
{
  try
    {
      typedef itk::Image<float, 3> ImageType;
      
      // Set up filter
      itk::DiscreteGaussianImageFilter<ImageType, ImageType>::Pointer 
        filter =
        itk::DiscreteGaussianImageFilter<ImageType, ImageType>::New();
      FilterWatcher watcher(filter);

      filter->SetVariance(1.0f);
      filter->SetMaximumError(.01f);
      
      // Run Test
      itk::Size<3> sz;
      sz[0] = 100 ; //atoi(argv[1]);
      sz[1] = 100 ; // atoi(argv[2]);
      sz[2] = 40;
      //      sz[2] = 10;//atoi(argv[3]);
      //      sz[3] = 5;//atoi(argv[4]);
      itk::NullImageToImageFilterDriver< ImageType, ImageType > test1;
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

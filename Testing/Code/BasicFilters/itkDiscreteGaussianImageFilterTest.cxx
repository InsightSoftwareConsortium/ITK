/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDiscreteGaussianImageFilterTest.cxx
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

      // Test other set/get functions
      itk::DiscreteGaussianImageFilter<ImageType, ImageType>::ArrayType array;
      array[0] = 0.05;
      array[1] = 0.06;
      array[2] = 0.07;
      filter->SetMaximumError( array );

      array.Fill( 0.04 );
      filter->SetMaximumError( array.GetDataPointer() );

      // set some parameters
      filter->SetVariance(1.0);
      filter->SetMaximumError(.01);
      
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
      return EXIT_FAILURE;
    } 
  return EXIT_SUCCESS;   
}

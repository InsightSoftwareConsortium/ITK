/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeImageFilterTest.cxx
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
#include "itkDerivativeImageFilter.h"
#include "itkNullImageToImageFilterDriver.txx"
#include "itkFilterWatcher.h"

int itkDerivativeImageFilterTest(int , char * [] )
{
  //  try
    {
      typedef  itk::Image<float, 3> ImageType;

      // Set up filter
      itk::DerivativeImageFilter<ImageType, ImageType>::Pointer filter
        = itk::DerivativeImageFilter<ImageType, ImageType>::New();
      FilterWatcher watcher(filter);

      filter->SetOrder(1);
      filter->SetDirection(1);
      std::cout << "About to execute" << std::endl;
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
      std::cout << "Finished executing" << std::endl;
    }
//   catch(itk::ExceptionObject &err)
//     {
//       (&err)->Print(std::cerr);
//       return 1;
//     } 
  return 0;   
}

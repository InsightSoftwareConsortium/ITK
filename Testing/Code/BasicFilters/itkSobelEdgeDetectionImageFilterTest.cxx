/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSobelEdgeDetectionImageFilterTest.cxx
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
#include "itkSobelEdgeDetectionImageFilter.h"
#include "itkNullImageToImageFilterDriver.txx"
#include "itkVector.h"

inline std::ostream& operator<<(std::ostream &o, const itk::Vector<float, 3> &v)
{
  o << "["<< v[0] << " " << v[1] << " " << v[2] << "]";
  return o;
}

int itkSobelEdgeDetectionImageFilterTest(int argc, char *argv[] )
{
  try
    {
      typedef itk::Image<float, 2> ImageType;
      
      // Set up filter
      itk::SobelEdgeDetectionImageFilter<ImageType, ImageType>::Pointer 
        filter =
        itk::SobelEdgeDetectionImageFilter<ImageType, ImageType>::New();

      // Run Test
      itk::Size<2> sz;
      sz[0] = 100 ; //atoi(argv[1]);
      sz[1] = 100 ; // atoi(argv[2]);
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

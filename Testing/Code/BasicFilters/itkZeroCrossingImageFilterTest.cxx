/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkZeroCrossingImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
#include "itkZeroCrossingImageFilter.h"
#include "itkNullImageToImageFilterDriver.txx"
#include "itkVector.h"

inline std::ostream& operator<<(std::ostream &o, const itk::Vector<float, 3> &v)
{
  o << "["<< v[0] << " " << v[1] << " " << v[2] << "]";
  return o;
}

int itkZeroCrossingImageFilterTest(int, char * [] )
{
  try
    {
      typedef itk::Image<float, 2> ImageType;
      
      // Set up filter
      itk::ZeroCrossingImageFilter<ImageType, ImageType>::Pointer 
        filter =
        itk::ZeroCrossingImageFilter<ImageType, ImageType>::New();

      std::cout << "filter: " << filter;
      // Run Test
      itk::Size<2> sz;
      sz[0] = 100 ;
      sz[1] = 100 ;

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

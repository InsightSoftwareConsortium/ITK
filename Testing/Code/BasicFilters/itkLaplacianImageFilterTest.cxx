/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLaplacianImageFilterTest.cxx
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
#include "itkImage.h"
#include <iostream>
#include "itkLaplacianImageFilter.h"
#include "itkNullImageToImageFilterDriver.txx"
#include "itkVector.h"
#include "itkFilterWatcher.h"

inline std::ostream& operator<<(std::ostream &o, const itk::Vector<float, 3> &v)
{
  o << "["<< v[0] << " " << v[1] << " " << v[2] << "]";
  return o;
}

int itkLaplacianImageFilterTest(int , char * [] )
{
  try
    {
    typedef itk::Image<float, 2> ImageType;
      
      // Set up filter
      itk::LaplacianImageFilter<ImageType, ImageType>::Pointer 
        filter =
        itk::LaplacianImageFilter<ImageType, ImageType>::New();

      FilterWatcher watch(filter);

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

      // verify the fix for Bug: 788
      // The following code should throw an exception and not crash.
      filter->SetInput(NULL);
      bool exceptionSeen = false;
      try
        {
        filter->Update();
        }
      catch(itk::ExceptionObject &err)
        {
        exceptionSeen = true;
        std::cout << "Expected exception: " << std::endl;
        std::cout << err << std::endl;
        std::cout << " was received OK" << std::endl;
        }
      if( !exceptionSeen )
        {
        std::cerr << "Expected exception was not thrown" << std::endl;
        return EXIT_FAILURE;
        }
    }
  catch(itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return EXIT_FAILURE;
    } 
  return EXIT_SUCCESS;   
}

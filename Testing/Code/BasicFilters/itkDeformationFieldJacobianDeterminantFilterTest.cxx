/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDeformationFieldJacobianDeterminantFilterTest.cxx
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
#include "itkDeformationFieldJacobianDeterminantFilter.h"
#include "itkNullImageToImageFilterDriver.txx"
#include "itkVector.h"

int
itkDeformationFieldJacobianDeterminantFilterTest(int , char * [] )
{
  try
    {
    typedef itk::Vector<float, 3> VectorType;
    typedef itk::Image< VectorType, 3> VectorImageType;
    typedef itk::Image< float, 3> ScalarImageType;
      
    // Set up filter
    itk::DeformationFieldJacobianDeterminantFilter<VectorImageType>::Pointer 
    filter =
        itk::DeformationFieldJacobianDeterminantFilter<VectorImageType>::New();

    // Run Test
    itk::Size<3> sz;
    sz[0] = 100 ;
    sz[1] = 100 ;
    sz[2] = 100 ;
    itk::NullImageToImageFilterDriver< VectorImageType, ScalarImageType > test1;
    test1.SetImageSize(sz);
    test1.SetFilter(filter.GetPointer());
    test1.Execute();
    }
  catch(itk::ExceptionObject &err)
    {
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    } 
  return EXIT_SUCCESS;   
}


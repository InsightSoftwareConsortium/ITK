/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorNeighborhoodOperatorImageFilterTest.cxx
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
#include "itkVectorNeighborhoodOperatorImageFilter.h"
#include "itkDerivativeOperator.h"
#include "itkNullImageToImageFilterDriver.txx"

int 
itkVectorNeighborhoodOperatorImageFilterTest(
int itkNotUsed(argc), 
char * itkNotUsed(argv) [] )
{
  try
    {
      typedef  itk::Vector<double, 2> VectorType;
      typedef  VectorType::ValueType  ScalarValueType;
      typedef  itk::Image<VectorType, 3> ImageType;
      // Set up operator
      itk::DerivativeOperator<ScalarValueType, 3> oper;
      oper.SetOrder(2);
      oper.SetDirection(2);
      oper.CreateDirectional();

      // Set up filter
      itk::VectorNeighborhoodOperatorImageFilter<ImageType, ImageType>::Pointer filter
        = itk::VectorNeighborhoodOperatorImageFilter<ImageType, ImageType>::New();
      filter->SetOperator(oper);

      // Run Test
      itk::Size<3> sz;
      sz[0]=128;
      sz[1]=128;
      sz[2]=2;
      itk::NullImageToImageFilterDriver< ImageType, ImageType >  test1;
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

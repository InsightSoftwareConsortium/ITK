/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperatorImageFilterTest.cxx
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
#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkDerivativeOperator.h"
#include "itkNullImageToImageFilterDriver.txx"

int itkNeighborhoodOperatorImageFilterTest(int , char *[] )
{
  try
    {
      typedef  itk::Image<float, 3> ImageType;
      // Set up operator
      itk::DerivativeOperator<float, 3> oper;
      oper.SetOrder(2);
      oper.SetDirection(2);
      oper.CreateDirectional();

      // Set up filter
      itk::NeighborhoodOperatorImageFilter<ImageType, ImageType>::Pointer filter
        = itk::NeighborhoodOperatorImageFilter<ImageType, ImageType>::New();
      filter->SetOperator(oper);

      // Run Test
      itk::Size<3> sz;
      sz[0]=257;
      sz[1]=252;
      sz[2]=5;
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

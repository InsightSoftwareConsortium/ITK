/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageSingleOperatorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>
#include "itkImage.h"
#include "itkFilterImageSingleOperator.h"
#include "itkDerivativeOperator.h"
#include "itkUUImageToImageFilterDriver.h"
#include "itkScalar.h"

main(int argc, char *argv[])
{
  try
    {
      // Set up operator
      itk::DerivativeOperator<itk::Scalar<float>, 3> oper;
      oper.SetOrder(1);
      oper.SetDirection(2);
      oper.CreateDirectional();

      // Set up filter
      itk::FilterImageSingleOperator<itk::Scalar<float>, 3>::Pointer filter
        = itk::FilterImageSingleOperator<itk::Scalar<float>, 3>::New();
      filter->SetOperator(oper);

      // Run Test
      itk::Size<3> sz;
      sz[0]=256;
      sz[1]=256;
      sz[2]=5;
      itk::UUImageToImageFilterDriver< itk::Image<itk::Scalar<float>, 3>,
                                        itk::Image<itk::Scalar<float>, 3> >
        test1;
      test1.SetImageSize(sz);
      test1.SetFilter(filter.GetPointer());
      test1.Execute();
    }
  catch(itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return 2;
    } 
  return 1;   
}

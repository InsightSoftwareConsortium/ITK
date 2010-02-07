/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReviewTests6.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests

#include "vnl/vnl_sample.h"
#include "itkTestMain.h"
#include "itkConfigure.h"

void RegisterTests()
{
  vnl_sample_reseed(8775070);

  REGISTER_TEST(itkScalarChanAndVeseLevelSetFunctionTest1);
  REGISTER_TEST(itkScalarChanAndVeseLevelSetFunctionTest2);
  REGISTER_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest1);
  REGISTER_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest2);
  REGISTER_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest3);
  REGISTER_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest4);
  REGISTER_TEST(itkScalarChanAndVeseSparseLevelSetImageFilterTest1);
  REGISTER_TEST(itkScalarChanAndVeseSparseLevelSetImageFilterTest2);
}

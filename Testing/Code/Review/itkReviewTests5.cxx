/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReviewTests5.cxx
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
  REGISTER_TEST(itkLabelGeometryImageFilterTest);
  REGISTER_TEST(itkMRCImageIOTest);
  REGISTER_TEST(itkVTKImageIO2Test);
  REGISTER_TEST(itkJPEG2000ImageIOFactoryTest01);
  REGISTER_TEST(itkJPEG2000ImageIOTest00);
  REGISTER_TEST(itkJPEG2000ImageIOTest01);
  REGISTER_TEST(itkJPEG2000ImageIOTest02);
  REGISTER_TEST(itkJPEG2000ImageIOTest03);
  REGISTER_TEST(itkJPEG2000ImageIOTest04);
  REGISTER_TEST(itkJPEG2000ImageIOTest05);
  REGISTER_TEST(itkJPEG2000ImageIOTest06);
  REGISTER_TEST(itkJPEG2000ImageIORegionOfInterest);
}

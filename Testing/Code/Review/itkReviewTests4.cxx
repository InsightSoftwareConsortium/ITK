/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReviewTests4.cxx
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
  //
  // These tests currently cause the following linker failure on Borland
  // Fatal: illegal VIRDEF fixup index in module ...
  // Please limit the tests in this file to Borland failing in this
  // manner
  REGISTER_TEST(itkBinaryImageToStatisticsLabelMapFilterTest1);
  REGISTER_TEST(itkLabelImageToStatisticsLabelMapFilterTest1);
  REGISTER_TEST(itkStatisticsOpeningLabelMapFilterTest1);
  REGISTER_TEST(itkStatisticsKeepNObjectsLabelMapFilterTest1);
  REGISTER_TEST(itkStatisticsRelabelLabelMapFilterTest1);
  REGISTER_TEST(itkBinaryStatisticsOpeningImageFilterTest1);
  REGISTER_TEST(itkBinaryStatisticsKeepNObjectsImageFilterTest1);
  REGISTER_TEST(itkLabelStatisticsKeepNObjectsImageFilterTest1);
  REGISTER_TEST(itkLabelStatisticsOpeningImageFilterTest1);
  REGISTER_TEST(itkStatisticsRelabelImageFilterTest1);
  REGISTER_TEST(itkStatisticsUniqueLabelMapFilterTest1);

}

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBasicFiltersTests5.cxx
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
  REGISTER_TEST(itkBSplineDownsampleImageFilterTest);
  REGISTER_TEST(itkBSplineUpsampleImageFilterTest);
  REGISTER_TEST(itkSumProjectionImageFilterTest);
  REGISTER_TEST(itkMaximumProjectionImageFilterTest);
  REGISTER_TEST(itkMaximumProjectionImageFilterTest2);
  REGISTER_TEST(itkMaximumProjectionImageFilterTest3);
  REGISTER_TEST(itkMeanProjectionImageFilterTest);
  REGISTER_TEST(itkMedianProjectionImageFilterTest);
  REGISTER_TEST(itkMinimumProjectionImageFilterTest);
  REGISTER_TEST(itkStandardDeviationProjectionImageFilterTest);
  REGISTER_TEST(itkBinaryProjectionImageFilterTest);
  REGISTER_TEST(itkBinaryThresholdProjectionImageFilterTest);
  REGISTER_TEST(itkProjectionImageFilterTest);
  REGISTER_TEST(itkImageToVectorImageFilterTest);
  REGISTER_TEST(itkSimplexMeshWithFloatCoordRepTest);
  REGISTER_TEST(itkReleaseDataFilterTest);
  REGISTER_TEST(itkWarpImageFilterTest2);
}

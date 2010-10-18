/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReviewTests2.cxx
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

  // NOTE: Please do not add more tests to this test driver. The Borland
  // compiler sometimes has problems linking large executables. Add new
  // tests to itkReviewTests3.cxx
  //
  REGISTER_TEST(itkDirectFourierReconstructionImageToImageFilterTest);
  REGISTER_TEST(itkBSplineDeformableTransformInitializerTest1);
  REGISTER_TEST(itkBSplineDeformableTransformInitializerTest2);
  REGISTER_TEST(itkHeavisideStepFunctionTest1);
  REGISTER_TEST(itkLabelImageToLabelMapFilterTest);
  REGISTER_TEST(itkLabelMapFilterTest);
  REGISTER_TEST(itkLabelMapTest);
  REGISTER_TEST(itkLabelMapToLabelImageFilterTest);
  REGISTER_TEST(itkLabelObjectLineComparatorTest);
  REGISTER_TEST(itkLabelObjectLineTest);
  REGISTER_TEST(itkLabelObjectTest);
  REGISTER_TEST(itkMultiphaseFiniteDifferenceImageFilterTest);
  REGISTER_TEST(itkMultiphaseDenseFiniteDifferenceImageFilterTest);
  REGISTER_TEST(itkMultiphaseSparseFiniteDifferenceImageFilterTest);

  REGISTER_TEST(itkContourExtractor2DImageFilterTest);
  REGISTER_TEST(itkAtanRegularizedHeavisideStepFunctionTest1);
  REGISTER_TEST(itkRegionBasedLevelSetFunctionTest);
  REGISTER_TEST(itkScalarRegionBasedLevelSetFunctionTest);
  REGISTER_TEST(itkScalarToRGBColormapImageFilterTest);
  REGISTER_TEST(itkStochasticFractalDimensionImageFilterTest);
  REGISTER_TEST(itkSinRegularizedHeavisideStepFunctionTest1);

  // NOTE: Please do not add more tests to this test driver. The Borland
  // compiler sometimes has problems linking large executables. Add new
  // tests to itkReviewTests3.cxx
  //
}

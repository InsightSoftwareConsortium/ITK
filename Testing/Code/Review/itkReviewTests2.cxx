/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
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

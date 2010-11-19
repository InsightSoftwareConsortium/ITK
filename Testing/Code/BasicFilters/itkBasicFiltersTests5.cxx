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
  REGISTER_TEST(itkDiscreteGaussianImageFilterTest2);
  REGISTER_TEST(itkRecursiveGaussianScaleSpaceTest1);
}

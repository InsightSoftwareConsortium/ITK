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

  REGISTER_TEST(itkObjectToObjectMetricTest);
  REGISTER_TEST(itkDeformationFieldTransformTest);
  REGISTER_TEST(itkBSplineControlPointImageFilterTest);
  REGISTER_TEST(itkBSplineControlPointImageFilterTest1);
  REGISTER_TEST(itkN4MRIBiasFieldCorrectionImageFilterTest);
  REGISTER_TEST(itkCompositeTransformTest);
  REGISTER_TEST(itkScalarChanAndVeseLevelSetFunctionTest1);
  REGISTER_TEST(itkScalarChanAndVeseLevelSetFunctionTest2);
  REGISTER_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest1);
  REGISTER_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest2);
  REGISTER_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest3);
  REGISTER_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest4);
  REGISTER_TEST(itkScalarChanAndVeseSparseLevelSetImageFilterTest1);
  REGISTER_TEST(itkScalarChanAndVeseSparseLevelSetImageFilterTest2);
}

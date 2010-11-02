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
#include <iostream>
#include "itkTestMain.h"


void RegisterTests()
{
  REGISTER_TEST(itkAmoebaOptimizerTest);
  REGISTER_TEST(itkCompositeValleyFunctionTest);
  REGISTER_TEST(itkConjugateGradientOptimizerTest);
  REGISTER_TEST(itkCumulativeGaussianOptimizerTest);
  REGISTER_TEST(itkExhaustiveOptimizerTest);
  REGISTER_TEST(itkFRPROptimizerTest);
  REGISTER_TEST(itkGradientDescentOptimizerTest);
  REGISTER_TEST(itkLBFGSBOptimizerTest);
  REGISTER_TEST(itkLBFGSOptimizerTest);
  REGISTER_TEST(itkLevenbergMarquardtOptimizerTest);
  REGISTER_TEST(itkMultivariateLegendrePolynomialTest);
  REGISTER_TEST(itkNumericsPrintTest);
  REGISTER_TEST(itkNumericsTest);
  REGISTER_TEST(itkOptimizersHierarchyTest);
  REGISTER_TEST(itkPowellOptimizerTest);
  REGISTER_TEST(itkRegularStepGradientDescentOptimizerTest);
  REGISTER_TEST(itkSymmetricEigenSystemTest);
  REGISTER_TEST(itkSPSAOptimizerTest);
  REGISTER_TEST(itkVersorRigid3DTransformOptimizerTest);
  REGISTER_TEST(itkVersorTransformOptimizerTest);
}

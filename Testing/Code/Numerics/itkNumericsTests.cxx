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

// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(itkAmoebaOptimizerTest);
  REGISTER_TEST(itkConjugateGradientOptimizerTest);
  REGISTER_TEST(itkGradientDescentOptimizerTest);
  REGISTER_TEST(itkLevenbergMarquardtOptimizerTest);
  REGISTER_TEST(itkLBFGSOptimizerTest);
  REGISTER_TEST(itkMultivariateLegendrePolynomialTest);
  REGISTER_TEST(itkNumericsTest);
  REGISTER_TEST(itkOptimizersHierarchyTest);
  REGISTER_TEST(itkRegularStepGradientDescentOptimizerTest);
  REGISTER_TEST(itkVersorTransformOptimizerTest);
}

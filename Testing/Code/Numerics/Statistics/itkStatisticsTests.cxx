// this file defines the itkStatisticsTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(itkDenseFrequencyContainerTest);
  REGISTER_TEST(itkFastRandomUnitNormalVariateGeneratorTest);
  REGISTER_TEST(itkHistogramTest);
  REGISTER_TEST(itkImageToListAdaptorTest);
  REGISTER_TEST(itkMembershipSampleTest);
  REGISTER_TEST(itkSubsampleTest);
  REGISTER_TEST(itkMeanCalculatorTest);
  REGISTER_TEST(itkCovarianceCalculatorTest);
  REGISTER_TEST(itkStatisticsAlgorithmTest);
}

// this file defines the itkStatisticsTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(itkStatisticsPrintTest);
  REGISTER_TEST(itkCovarianceCalculatorTest);
  REGISTER_TEST(itkDenseFrequencyContainerTest);
  REGISTER_TEST(itkExpectationMaximizationMixtureModelEstimatorTest);
  REGISTER_TEST(itkGoodnessOfFitMixtureModelCostFunctionTest);
  REGISTER_TEST(itkHistogramTest);
  REGISTER_TEST(itkImageToListAdaptorTest);
  REGISTER_TEST(itkKdTreeBasedKmeansEstimatorTest);
  REGISTER_TEST(itkListSampleTest);
  REGISTER_TEST(itkListSampleToHistogramFilterTest);
  REGISTER_TEST(itkListSampleToHistogramGeneratorTest);
  REGISTER_TEST(itkMembershipSampleTest);
  REGISTER_TEST(itkMembershipSampleGeneratorTest);
  REGISTER_TEST(itkMeanCalculatorTest);
  REGISTER_TEST(itkNeighborhoodSamplerTest) ;
  REGISTER_TEST(itkNormalVariateGeneratorTest);
  REGISTER_TEST(itkSampleClassifierTest) ;
  REGISTER_TEST(itkSampleClassifierWithMaskTest) ;
  REGISTER_TEST(itkSelectiveSubsampleGeneratorTest) ;
  REGISTER_TEST(itkStatisticsAlgorithmTest);
  REGISTER_TEST(itkSubsampleTest);
  REGISTER_TEST(itkWeightedMeanCalculatorTest);
}

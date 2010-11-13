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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

// this file defines the itkStatisticsTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "vnl/vnl_sample.h"
#include "itkTestMain.h"


void RegisterTests()
{
  vnl_sample_reseed(8775070);
  REGISTER_TEST(itkStatisticsPrintTest);
  REGISTER_TEST(itkStatisticsTypesTest);
  REGISTER_TEST(itkHistogramTest);
  REGISTER_TEST(itkHistogramToTextureFeaturesFilterTest);
  REGISTER_TEST(itkHistogramToEntropyImageFilterTest1);
  REGISTER_TEST(itkHistogramToEntropyImageFilterTest2);
  REGISTER_TEST(itkHistogramToIntensityImageFilterTest1);
  REGISTER_TEST(itkHistogramToIntensityImageFilterTest2);
  REGISTER_TEST(itkHistogramToLogProbabilityImageFilterTest1);
  REGISTER_TEST(itkHistogramToLogProbabilityImageFilterTest2);
  REGISTER_TEST(itkHistogramToProbabilityImageFilterTest1);
  REGISTER_TEST(itkHistogramToProbabilityImageFilterTest2);
  REGISTER_TEST(itkImageToHistogramFilterTest);
  REGISTER_TEST(itkImageToHistogramFilterTest2);
  REGISTER_TEST(itkImageToHistogramFilterTest3);
  REGISTER_TEST(itkImageToListSampleFilterTest);
  REGISTER_TEST(itkImageToListSampleFilterTest2);
  REGISTER_TEST(itkImageToListSampleFilterTest3);
  REGISTER_TEST(itkListSampleTest);
  REGISTER_TEST(itkSampleToHistogramFilterTest);
  REGISTER_TEST(itkSampleToHistogramFilterTest2);
  REGISTER_TEST(itkSampleToHistogramFilterTest3);
  REGISTER_TEST(itkSampleToHistogramFilterTest4);
  REGISTER_TEST(itkSampleToHistogramFilterTest5);
  REGISTER_TEST(itkSampleToHistogramFilterTest6);
  REGISTER_TEST(itkSampleToHistogramFilterTest7);
  REGISTER_TEST(itkMeanSampleFilterTest);
  REGISTER_TEST(itkMeasurementVectorTraitsTest);
  REGISTER_TEST(itkMembershipSampleTest1);
  REGISTER_TEST(itkMembershipSampleTest2);
  REGISTER_TEST(itkMembershipSampleTest3);
  REGISTER_TEST(itkMembershipSampleTest4);
  REGISTER_TEST(itkNeighborhoodSamplerTest1);
  REGISTER_TEST(itkNormalVariateGeneratorTest1);
  REGISTER_TEST(itkCovarianceSampleFilterTest);
  REGISTER_TEST(itkCovarianceSampleFilterTest2);
  REGISTER_TEST(itkCovarianceSampleFilterTest3);
  REGISTER_TEST(itkRandomVariateGeneratorBaseTest);
  REGISTER_TEST(itkScalarImageToCooccurrenceMatrixFilterTest);
  REGISTER_TEST(itkScalarImageToCooccurrenceMatrixFilterTest2);
  REGISTER_TEST(itkScalarImageToCooccurrenceListSampleFilterTest);
  REGISTER_TEST(itkSampleTest);
  REGISTER_TEST(itkSampleTest2);
  REGISTER_TEST(itkSampleTest3);
  REGISTER_TEST(itkSampleTest4);

  REGISTER_TEST(itkSampleClassifierFilterTest1);
  REGISTER_TEST(itkSampleClassifierFilterTest2);
  REGISTER_TEST(itkSampleClassifierFilterTest3);
  REGISTER_TEST(itkSampleClassifierFilterTest4);
  REGISTER_TEST(itkSampleClassifierFilterTest5);
  REGISTER_TEST(itkSampleClassifierFilterTest6);
  REGISTER_TEST(itkSampleClassifierFilterTest7);

  REGISTER_TEST(itkSampleToSubsampleFilterTest1);
  REGISTER_TEST(itkSubsampleTest);
  REGISTER_TEST(itkSubsampleTest2);
  REGISTER_TEST(itkSubsampleTest3);
  REGISTER_TEST(itkStatisticsAlgorithmTest);
  REGISTER_TEST(itkStatisticsAlgorithmTest2);
  REGISTER_TEST(itkStandardDeviationPerComponentSampleFilterTest);
  REGISTER_TEST(itkWeightedMeanSampleFilterTest);
  REGISTER_TEST(itkWeightedCovarianceSampleFilterTest);
  REGISTER_TEST(itkWeightedCovarianceSampleFilterTest2);
  REGISTER_TEST(itkImageToListSampleAdaptorTest);
  REGISTER_TEST(itkImageToListSampleAdaptorTest2);
  REGISTER_TEST(itkPointSetToListSampleAdaptorTest);
  REGISTER_TEST(itkProbabilityDistributionTest);

  REGISTER_TEST(itkJointDomainImageToListSampleAdaptorTest);
  REGISTER_TEST(itkDenseFrequencyContainer2Test);
  REGISTER_TEST(itkSparseFrequencyContainer2Test);
  REGISTER_TEST(itkScalarImageToTextureFeaturesFilterTest);

  REGISTER_TEST(itkMembershipFunctionBaseTest);
  REGISTER_TEST(itkMembershipFunctionBaseTest2);

  REGISTER_TEST(itkDistanceToCentroidMembershipFunctionTest);
  REGISTER_TEST(itkGaussianMembershipFunctionTest);

  REGISTER_TEST(itkDistanceMetricTest);
  REGISTER_TEST(itkDistanceMetricTest2);
  REGISTER_TEST(itkEuclideanDistanceMetricTest);
  REGISTER_TEST(itkEuclideanSquareDistanceMetricTest);
  REGISTER_TEST(itkMahalanobisDistanceMetricTest);
  REGISTER_TEST(itkManhattanDistanceMetricTest);

  REGISTER_TEST(itkDecisionRuleTest);
  REGISTER_TEST(itkMaximumDecisionRuleTest);
  REGISTER_TEST(itkMaximumRatioDecisionRuleTest);
  REGISTER_TEST(itkMinimumDecisionRuleTest);
  REGISTER_TEST(itkMixtureModelComponentBaseTest);

  REGISTER_TEST(itkKdTreeBasedKmeansEstimatorTest);
  REGISTER_TEST(itkKdTreeGeneratorTest);
  REGISTER_TEST(itkKdTreeTest1);
  REGISTER_TEST(itkKdTreeTestSamplePoints);
  REGISTER_TEST(itkKdTreeTest2);
  REGISTER_TEST(itkWeightedCentroidKdTreeGeneratorTest1);
  REGISTER_TEST(itkWeightedCentroidKdTreeGeneratorTest8);
  REGISTER_TEST(itkWeightedCentroidKdTreeGeneratorTest9);

  REGISTER_TEST(itkGaussianMixtureModelComponentTest);
  REGISTER_TEST(itkExpectationMaximizationMixtureModelEstimatorTest);

  REGISTER_TEST(itkImageClassifierFilterTest);

  REGISTER_TEST(itkChiSquareDistributionTest);
  REGISTER_TEST(itkGaussianDistributionTest);
  REGISTER_TEST(itkTDistributionTest);
}

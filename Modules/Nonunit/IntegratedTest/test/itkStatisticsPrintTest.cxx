/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkSampleClassifierFilter.h"
#include "itkSampleToHistogramFilter.h"
#include "itkNeighborhoodSampler.h"
#include "itkScalarImageToCooccurrenceListSampleFilter.h"
#include "itkScalarImageToTextureFeaturesFilter.h"
#include "itkWeightedCovarianceSampleFilter.h"
#include "itkImageToListSampleAdaptor.h"
#include "itkPointSetToListSampleAdaptor.h"
#include "itkJointDomainImageToListSampleAdaptor.h"
#include "itkMaximumDecisionRule.h"
#include "itkMinimumDecisionRule.h"
#include "itkEuclideanSquareDistanceMetric.h"
#include "itkMahalanobisDistanceMetric.h"
#include "itkManhattanDistanceMetric.h"
#include "itkImageClassifierFilter.h"
#include "itkKdTreeBasedKmeansEstimator.h"
#include "itkExpectationMaximizationMixtureModelEstimator.h"
#include "itkWeightedCentroidKdTreeGenerator.h"

int
itkStatisticsPrintTest(int, char *[])
{
  using TMeasurementType = float;

  using TMeasurementVectorType = itk::FixedArray<TMeasurementType, 2>;
  using ImageType = itk::Image<TMeasurementVectorType, 3>;
  using ScalarImageType = itk::Image<unsigned char, 3>;
  using PointSetType = itk::PointSet<TMeasurementType, 2>;
  using OutputImageType = itk::Image<unsigned long, 3>;

  using SampleType = itk::Statistics::ListSample<TMeasurementVectorType>;

  using SubSampleType = itk::Statistics::Subsample<SampleType>;

  using HistogramType = itk::Statistics::Histogram<TMeasurementType>;

  using SampleToHistogramFilterType = itk::Statistics::SampleToHistogramFilter<SampleType, HistogramType>;

  using SampleClassifierFilterType = itk::Statistics::SampleClassifierFilter<SampleType>;

  using ImageClassifierFilterType = itk::Statistics::ImageClassifierFilter<SampleType, ImageType, OutputImageType>;

  using ImageToListSampleFilterType = itk::Statistics::ImageToListSampleFilter<ImageType, ScalarImageType>;

  using ImageToListSampleAdaptorType = itk::Statistics::ImageToListSampleAdaptor<ImageType>;

  using JointDomainImageToListSampleAdaptorType = itk::Statistics::JointDomainImageToListSampleAdaptor<ImageType>;

  using ScalarImageToCooccurrenceMatrixFilterType =
    itk::Statistics::ScalarImageToCooccurrenceMatrixFilter<ScalarImageType>;

  using ScalarImageToCooccurrenceListSampleFilterType =
    itk::Statistics::ScalarImageToCooccurrenceListSampleFilter<ScalarImageType>;

  using ScalarImageToTextureFeaturesFilterType = itk::Statistics::ScalarImageToTextureFeaturesFilter<ScalarImageType>;

  using MembershipSampleType = itk::Statistics::MembershipSample<SampleType>;

  using DistanceToCentroidMembershipFunctionType =
    itk::Statistics::DistanceToCentroidMembershipFunction<TMeasurementVectorType>;

  using EuclideanDistanceMetricType = itk::Statistics::EuclideanDistanceMetric<TMeasurementVectorType>;

  using EuclideanSquareDistanceMetricType = itk::Statistics::EuclideanSquareDistanceMetric<TMeasurementVectorType>;

  using MahalanobisDistanceMetricType = itk::Statistics::MahalanobisDistanceMetric<TMeasurementVectorType>;

  using ManhattanDistanceMetricType = itk::Statistics::ManhattanDistanceMetric<TMeasurementVectorType>;

  using MaximumDecisionRuleType = itk::Statistics::MaximumDecisionRule;
  using MinimumDecisionRuleType = itk::Statistics::MinimumDecisionRule;

  using HistogramToTextureFeaturesFilterType = itk::Statistics::HistogramToTextureFeaturesFilter<HistogramType>;

  using MeanSampleFilterType = itk::Statistics::MeanSampleFilter<SampleType>;

  using WeightedMeanSampleFilterType = itk::Statistics::WeightedMeanSampleFilter<SampleType>;

  using CovarianceSampleFilterType = itk::Statistics::CovarianceSampleFilter<SampleType>;

  using WeightedCovarianceSampleFilterType = itk::Statistics::WeightedCovarianceSampleFilter<SampleType>;

  using NeighborhoodSamplerType = itk::Statistics::NeighborhoodSampler<SampleType>;

  using PointSetToListSampleAdaptorType = itk::Statistics::PointSetToListSampleAdaptor<PointSetType>;

  using DenseFrequencyContainer2Type = itk::Statistics::DenseFrequencyContainer2;

  using SparseFrequencyContainer2Type = itk::Statistics::SparseFrequencyContainer2;

  using EMEstimatorType = itk::Statistics::ExpectationMaximizationMixtureModelEstimator<SampleType>;

  using TreeGeneratorType = itk::Statistics::WeightedCentroidKdTreeGenerator<SampleType>;

  using KdTreeBasedKMeansEstimatorType = itk::Statistics::KdTreeBasedKmeansEstimator<TreeGeneratorType::KdTreeType>;

  auto sampleObj = SampleType::New();
  std::cout << "----------ListSample " << sampleObj;

  auto subsampleObj = SubSampleType::New();
  std::cout << "----------Subsample " << subsampleObj;

  auto HistogramObj = HistogramType::New();
  std::cout << "----------Histogram " << HistogramObj;

  auto SampleToHistogramFilterObj = SampleToHistogramFilterType::New();
  std::cout << "----------SampleToHistogramFilter ";
  std::cout << SampleToHistogramFilterObj;

  auto xSampleClassifierFilterObj = SampleClassifierFilterType::New();
  std::cout << "----------SampleClassifierFilter ";
  std::cout << xSampleClassifierFilterObj;

  auto ImageToListSampleFilterObj = ImageToListSampleFilterType::New();
  std::cout << "----------ImageToListSampleFilter ";
  std::cout << ImageToListSampleFilterObj;

  auto ImageToListSampleAdaptorObj = ImageToListSampleAdaptorType::New();
  std::cout << "----------ImageToListSampleAdaptor ";
  std::cout << ImageToListSampleAdaptorObj;

  JointDomainImageToListSampleAdaptorType::Pointer JointDomainImageToListSampleAdaptorObj =
    JointDomainImageToListSampleAdaptorType::New();
  std::cout << "----------JointDomainImageToListSampleAdaptor ";
  std::cout << JointDomainImageToListSampleAdaptorObj;

  auto PointSetToListSampleAdaptorObj = PointSetToListSampleAdaptorType::New();
  std::cout << "----------PointSetToListSampleAdaptor ";
  std::cout << PointSetToListSampleAdaptorObj;

  ScalarImageToCooccurrenceMatrixFilterType::Pointer ScalarImageToCooccurrenceMatrixFilterObj =
    ScalarImageToCooccurrenceMatrixFilterType::New();
  std::cout << "----------ScalarImageToCooccurrenceMatrixFilter ";
  std::cout << ScalarImageToCooccurrenceMatrixFilterObj;

  ScalarImageToCooccurrenceListSampleFilterType::Pointer ScalarImageToCooccurrenceListSampleFilterObj =
    ScalarImageToCooccurrenceListSampleFilterType::New();
  std::cout << "----------ScalarImageToCooccurrenceListSampleFilter ";
  std::cout << ScalarImageToCooccurrenceListSampleFilterObj;

  ScalarImageToTextureFeaturesFilterType::Pointer ScalarImageToTextureFeaturesFilterObj =
    ScalarImageToTextureFeaturesFilterType::New();
  std::cout << "----------ScalarImageToTextureFeaturesFilter ";
  std::cout << ScalarImageToTextureFeaturesFilterObj;

  HistogramToTextureFeaturesFilterType::Pointer HistogramToTextureFeaturesFilterObj =
    HistogramToTextureFeaturesFilterType::New();
  std::cout << "----------HistogramToTextureFeaturesFilter " << HistogramToTextureFeaturesFilterObj;

  auto MembershipSampleObj = MembershipSampleType::New();
  std::cout << "----------MembershipSample " << MembershipSampleObj;

  DistanceToCentroidMembershipFunctionType::Pointer DistanceToCentroidMembershipFunctionObj =
    DistanceToCentroidMembershipFunctionType::New();
  std::cout << "----------DistanceToCentroidMembershipFunction " << DistanceToCentroidMembershipFunctionObj;

  auto meanFilterObj = MeanSampleFilterType::New();
  std::cout << "----------Mean filter " << meanFilterObj;

  auto weighedMeanSampleFilterObj = WeightedMeanSampleFilterType::New();
  std::cout << "----------WeightedMean filter " << weighedMeanSampleFilterObj;

  auto covarianceFilterObj = CovarianceSampleFilterType::New();
  std::cout << "----------Covariance filter " << covarianceFilterObj;

  WeightedCovarianceSampleFilterType::Pointer weighedCovarianceSampleFilterObj =
    WeightedCovarianceSampleFilterType::New();
  std::cout << "----------WeightedCovariance filter " << weighedCovarianceSampleFilterObj;

  auto neighborhoodSamplerObj = NeighborhoodSamplerType::New();
  std::cout << "----------NeighborhoodSamplerType filter " << neighborhoodSamplerObj;

  auto DenseFrequencyContainer2Obj = DenseFrequencyContainer2Type::New();
  std::cout << "----------DenseFrequencyContainer " << DenseFrequencyContainer2Obj;

  auto SparseFrequencyContainer2Obj = SparseFrequencyContainer2Type::New();
  std::cout << "----------SparseFrequencyContainer2 " << SparseFrequencyContainer2Obj;

  auto euclideanDistance = EuclideanDistanceMetricType::New();
  std::cout << "----------EuclideanDistanceMetricType " << euclideanDistance;

  auto euclideanSquareDistance = EuclideanSquareDistanceMetricType::New();
  std::cout << "----------EuclideanSquareDistanceMetricType " << euclideanSquareDistance;

  auto mahalanobisDistance = MahalanobisDistanceMetricType::New();
  std::cout << "----------MahalanobisDistanceMetricType " << mahalanobisDistance;

  auto manhattanDistance = ManhattanDistanceMetricType::New();
  std::cout << "----------ManhattanDistanceMetricType " << manhattanDistance;

  auto maximumDecsion = MaximumDecisionRuleType::New();
  std::cout << "----------MaximumDecisionRuleType " << maximumDecsion;

  auto minimumDecsion = MinimumDecisionRuleType::New();
  std::cout << "----------MinimumDecisionRuleType " << minimumDecsion;

  auto classifierFilter = ImageClassifierFilterType::New();
  std::cout << "----------ImageClassifierFilterType " << classifierFilter;

  auto emEstimator = EMEstimatorType::New();
  std::cout << "----------EMEstimatorType " << emEstimator;

  auto kdTreeBasedEstimator = KdTreeBasedKMeansEstimatorType::New();
  std::cout << "----------KdTreeBasedKMeansEstimatorType " << kdTreeBasedEstimator;

  return EXIT_SUCCESS;
}

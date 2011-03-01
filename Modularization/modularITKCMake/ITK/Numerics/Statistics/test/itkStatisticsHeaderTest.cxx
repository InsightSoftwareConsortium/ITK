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

#include <iostream>

#include "itkChiSquareDistribution.h"
#include "itkCovarianceSampleFilter.h"
#include "itkCovarianceSampleFilter.txx"
#include "itkDecisionRule.h"
#include "itkDecisionRuleBase.h"
#include "itkDenseFrequencyContainer2.h"
#include "itkDistanceMetric.h"
#include "itkDistanceMetric.txx"
#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkDistanceToCentroidMembershipFunction.txx"
#include "itkEuclideanDistanceMetric.h"
#include "itkEuclideanDistanceMetric.txx"
#include "itkEuclideanSquareDistanceMetric.h"
#include "itkEuclideanSquareDistanceMetric.txx"
#include "itkExpectationMaximizationMixtureModelEstimator.h"
#include "itkExpectationMaximizationMixtureModelEstimator.txx"
#include "itkGaussianDistribution.h"
#include "itkGaussianMembershipFunction.h"
#include "itkGaussianMembershipFunction.txx"
#include "itkGaussianMixtureModelComponent.h"
#include "itkGaussianMixtureModelComponent.txx"
#include "itkHistogram.h"
#include "itkHistogram.txx"
#include "itkHistogramToEntropyImageFilter.h"
#include "itkHistogramToImageFilter.h"
#include "itkHistogramToImageFilter.txx"
#include "itkHistogramToIntensityImageFilter.h"
#include "itkHistogramToLogProbabilityImageFilter.h"
#include "itkHistogramToProbabilityImageFilter.h"
#include "itkHistogramToTextureFeaturesFilter.h"
#include "itkHistogramToTextureFeaturesFilter.txx"
#include "itkImageClassifierFilter.h"
#include "itkImageClassifierFilter.txx"
#include "itkImageToHistogramFilter.h"
#include "itkImageToHistogramFilter.txx"
#include "itkImageToListSampleAdaptor.h"
#include "itkImageToListSampleAdaptor.txx"
#include "itkImageToListSampleFilter.h"
#include "itkImageToListSampleFilter.txx"
#include "itkJointDomainImageToListSampleAdaptor.h"
#include "itkJointDomainImageToListSampleAdaptor.txx"
#include "itkKalmanLinearEstimator.h"
#include "itkKdTree.h"
#include "itkKdTree.txx"
#include "itkKdTreeBasedKmeansEstimator.h"
#include "itkKdTreeBasedKmeansEstimator.txx"
#include "itkKdTreeGenerator.h"
#include "itkKdTreeGenerator.txx"
#include "itkListSample.h"
#include "itkListSample.txx"
#include "itkMahalanobisDistanceMembershipFunction.h"
#include "itkMahalanobisDistanceMembershipFunction.txx"
#include "itkMahalanobisDistanceMetric.h"
#include "itkMahalanobisDistanceMetric.txx"
#include "itkManhattanDistanceMetric.h"
#include "itkManhattanDistanceMetric.txx"
#include "itkMaximumDecisionRule.h"
#include "itkMaximumDecisionRule2.h"
#include "itkMaximumRatioDecisionRule.h"
#include "itkMaximumRatioDecisionRule2.h"
#include "itkMeanSampleFilter.h"
#include "itkMeanSampleFilter.txx"
#include "itkMeasurementVectorTraits.h"
#include "itkMembershipFunctionBase.h"
#include "itkMembershipSample.h"
#include "itkMembershipSample.txx"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkMinimumDecisionRule.h"
#include "itkMinimumDecisionRule2.h"
#include "itkMixtureModelComponentBase.h"
#include "itkMixtureModelComponentBase.txx"
#include "itkNeighborhoodSampler.h"
#include "itkNeighborhoodSampler.txx"
#include "itkNormalVariateGenerator.h"
#include "itkPointSetToListSampleAdaptor.h"
#include "itkPointSetToListSampleAdaptor.txx"
#include "itkProbabilityDistribution.h"
#include "itkRandomVariateGeneratorBase.h"
#include "itkSample.h"
#include "itkSampleClassifierFilter.h"
#include "itkSampleClassifierFilter.txx"
#include "itkSampleToHistogramFilter.h"
#include "itkSampleToHistogramFilter.txx"
#include "itkSampleToSubsampleFilter.h"
#include "itkSampleToSubsampleFilter.txx"
#include "itkScalarImageToCooccurrenceListSampleFilter.h"
#include "itkScalarImageToCooccurrenceListSampleFilter.txx"
#include "itkScalarImageToCooccurrenceMatrixFilter.h"
#include "itkScalarImageToCooccurrenceMatrixFilter.txx"
#include "itkScalarImageToHistogramGenerator.h"
#include "itkScalarImageToHistogramGenerator.txx"
#include "itkScalarImageToTextureFeaturesFilter.h"
#include "itkScalarImageToTextureFeaturesFilter.txx"
#include "itkSparseFrequencyContainer2.h"
#include "itkStandardDeviationPerComponentSampleFilter.h"
#include "itkStandardDeviationPerComponentSampleFilter.txx"
#include "itkStatisticsAlgorithm.h"
#include "itkStatisticsAlgorithm.txx"
#include "itkSubsample.h"
#include "itkSubsample.txx"
#include "itkTDistribution.h"
#include "itkWeightedCentroidKdTreeGenerator.h"
#include "itkWeightedCentroidKdTreeGenerator.txx"
#include "itkWeightedCovarianceSampleFilter.h"
#include "itkWeightedCovarianceSampleFilter.txx"
#include "itkWeightedMeanSampleFilter.h"
#include "itkWeightedMeanSampleFilter.txx"



int itkStatisticsHeaderTest ( int , char ** )
{

  return EXIT_SUCCESS;
}

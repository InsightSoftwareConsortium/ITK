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
#include "itkCovarianceSampleFilter.txx"
#include "itkDecisionRule.h"
#include "itkDecisionRuleBase.h"
#include "itkDenseFrequencyContainer2.h"
#include "itkDistanceMetric.txx"
#include "itkDistanceToCentroidMembershipFunction.txx"
#include "itkEuclideanDistanceMetric.txx"
#include "itkEuclideanSquareDistanceMetric.txx"
#include "itkExpectationMaximizationMixtureModelEstimator.txx"
#include "itkGaussianDistribution.h"
#include "itkGaussianMembershipFunction.txx"
#include "itkGaussianMixtureModelComponent.txx"
#include "itkHistogram.txx"
#include "itkHistogramToEntropyImageFilter.h"
#include "itkHistogramToImageFilter.txx"
#include "itkHistogramToIntensityImageFilter.h"
#include "itkHistogramToLogProbabilityImageFilter.h"
#include "itkHistogramToProbabilityImageFilter.h"
#include "itkHistogramToTextureFeaturesFilter.txx"
#include "itkImageClassifierFilter.txx"
#include "itkImageToHistogramFilter.txx"
#include "itkImageToListSampleAdaptor.txx"
#include "itkImageToListSampleFilter.txx"
#include "itkJointDomainImageToListSampleAdaptor.txx"
#include "itkKalmanLinearEstimator.h"
#include "itkKdTree.txx"
#include "itkKdTreeBasedKmeansEstimator.txx"
#include "itkKdTreeGenerator.txx"
#include "itkListSample.txx"
#include "itkMahalanobisDistanceMembershipFunction.txx"
#include "itkMahalanobisDistanceMetric.txx"
#include "itkManhattanDistanceMetric.txx"
#include "itkMaximumDecisionRule.h"
#include "itkMaximumDecisionRule2.h"
#include "itkMaximumRatioDecisionRule2.h"
#include "itkMeanSampleFilter.txx"
#include "itkMeasurementVectorTraits.h"
#include "itkMembershipFunctionBase.h"
#include "itkMembershipSample.txx"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkMinimumDecisionRule.h"
#include "itkMinimumDecisionRule2.h"
#include "itkMixtureModelComponentBase.txx"
#include "itkNeighborhoodSampler.txx"
#include "itkNormalVariateGenerator.h"
#include "itkPointSetToListSampleAdaptor.txx"
#include "itkProbabilityDistribution.h"
#include "itkRandomVariateGeneratorBase.h"
#include "itkSample.h"
#include "itkSampleClassifierFilter.txx"
#include "itkSampleToHistogramFilter.txx"
#include "itkSampleToSubsampleFilter.txx"
#include "itkScalarImageToCooccurrenceListSampleFilter.txx"
#include "itkScalarImageToCooccurrenceMatrixFilter.txx"
#include "itkScalarImageToHistogramGenerator.txx"
#include "itkScalarImageToTextureFeaturesFilter.txx"
#include "itkSparseFrequencyContainer2.h"
#include "itkStandardDeviationPerComponentSampleFilter.txx"
#include "itkStatisticsAlgorithm.txx"
#include "itkSubsample.txx"
#include "itkTDistribution.h"
#include "itkWeightedCentroidKdTreeGenerator.txx"
#include "itkWeightedCovarianceSampleFilter.txx"
#include "itkWeightedMeanSampleFilter.txx"

int itkStatisticsHeaderTest ( int , char * [] )
{
  return EXIT_SUCCESS;
}

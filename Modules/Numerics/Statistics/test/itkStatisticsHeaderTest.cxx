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
#include "itkCovarianceSampleFilter.hxx"
#include "itkDecisionRule.h"
#include "itkDecisionRuleBase.h"
#include "itkDenseFrequencyContainer2.h"
#include "itkDistanceMetric.hxx"
#include "itkDistanceToCentroidMembershipFunction.hxx"
#include "itkEuclideanDistanceMetric.hxx"
#include "itkEuclideanSquareDistanceMetric.hxx"
#include "itkExpectationMaximizationMixtureModelEstimator.hxx"
#include "itkGaussianDistribution.h"
#include "itkGaussianMembershipFunction.hxx"
#include "itkGaussianMixtureModelComponent.hxx"
#include "itkHistogram.hxx"
#include "itkHistogramToEntropyImageFilter.h"
#include "itkHistogramToImageFilter.hxx"
#include "itkHistogramToIntensityImageFilter.h"
#include "itkHistogramToLogProbabilityImageFilter.h"
#include "itkHistogramToProbabilityImageFilter.h"
#include "itkHistogramToTextureFeaturesFilter.hxx"
#include "itkImageClassifierFilter.hxx"
#include "itkImageToHistogramFilter.hxx"
#include "itkImageToListSampleAdaptor.hxx"
#include "itkImageToListSampleFilter.hxx"
#include "itkJointDomainImageToListSampleAdaptor.hxx"
#include "itkKalmanLinearEstimator.h"
#include "itkKdTree.hxx"
#include "itkKdTreeBasedKmeansEstimator.hxx"
#include "itkKdTreeGenerator.hxx"
#include "itkListSample.hxx"
#include "itkMahalanobisDistanceMembershipFunction.hxx"
#include "itkMahalanobisDistanceMetric.hxx"
#include "itkManhattanDistanceMetric.hxx"
#include "itkMaximumDecisionRule.h"
#include "itkMaximumDecisionRule2.h"
#include "itkMaximumRatioDecisionRule2.h"
#include "itkMeanSampleFilter.hxx"
#include "itkMeasurementVectorTraits.h"
#include "itkMembershipFunctionBase.h"
#include "itkMembershipSample.hxx"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkMinimumDecisionRule.h"
#include "itkMinimumDecisionRule2.h"
#include "itkMixtureModelComponentBase.hxx"
#include "itkNeighborhoodSampler.hxx"
#include "itkNormalVariateGenerator.h"
#include "itkPointSetToListSampleAdaptor.hxx"
#include "itkProbabilityDistribution.h"
#include "itkRandomVariateGeneratorBase.h"
#include "itkSample.h"
#include "itkSampleClassifierFilter.hxx"
#include "itkSampleToHistogramFilter.hxx"
#include "itkSampleToSubsampleFilter.hxx"
#include "itkScalarImageToCooccurrenceListSampleFilter.hxx"
#include "itkScalarImageToCooccurrenceMatrixFilter.hxx"
#include "itkScalarImageToHistogramGenerator.hxx"
#include "itkScalarImageToTextureFeaturesFilter.hxx"
#include "itkSparseFrequencyContainer2.h"
#include "itkStandardDeviationPerComponentSampleFilter.hxx"
#include "itkStatisticsAlgorithm.hxx"
#include "itkSubsample.hxx"
#include "itkTDistribution.h"
#include "itkWeightedCentroidKdTreeGenerator.hxx"
#include "itkWeightedCovarianceSampleFilter.hxx"
#include "itkWeightedMeanSampleFilter.hxx"

int itkStatisticsHeaderTest ( int , char * [] )
{
  return EXIT_SUCCESS;
}

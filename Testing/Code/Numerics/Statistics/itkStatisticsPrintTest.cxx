/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsPrintTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkFixedArray.h"
#include "itkCovarianceCalculator.h"
// #include "itkDecisionRuleBase.h" // abstract class
#include "itkDenseFrequencyContainer.h"
// #include "itkDensityFunction.h" // abstract class
#include "itkDistanceMetric.h"
#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkEuclideanDistance.h"
#include "itkExpectationMaximizationMixtureModelEstimator.h"
#include "itkGaussianDensityFunction.h"
#include "itkGaussianGoodnessOfFitComponent.h"
#include "itkGaussianMixtureModelComponent.h"
#include "itkGoodnessOfFitFunctionBase.h"
#include "itkGoodnessOfFitMixtureModelCostFunction.h"
#include "itkHistogram.h"
#include "itkHypersphereKernelMeanShiftModeSeeker.h"
#include "itkImageToListAdaptor.h"
#include "itkJointDomainImageToListAdaptor.h"
#include "itkKdTree.h"
#include "itkKdTreeBasedKmeansEstimator.h"
#include "itkKdTreeGenerator.h"
#include "itkListSample.h"
#include "itkListSampleToHistogramFilter.h"
#include "itkListSampleToHistogramGenerator.h"
#include "itkLogLikelihoodGoodnessOfFitFunction.h"
#include "itkMahalanobisDistanceMembershipFunction.h"
#include "itkMeanCalculator.h"
#include "itkMeanShiftModeCacheMethod.h"
#include "itkMembershipSample.h"
#include "itkMembershipSampleGenerator.h"
#include "itkMixtureModelComponentBase.h"
#include "itkNeighborhoodSampler.h"
#include "itkNormalVariateGenerator.h"
#include "itkPointSetToListAdaptor.h"
// #include "itkRandomVariateGeneratorBase.h" // abstract class
#include "itkSampleAlgorithmBase.h"
#include "itkSampleClassifier.h"
#include "itkSampleClassifierWithMask.h"
#include "itkSampleMeanShiftBlurringFilter.h"
#include "itkSampleMeanShiftClusteringFilter.h"
#include "itkSampleSelectiveMeanShiftBlurringFilter.h"
#include "itkSampleToHistogramProjectionFilter.h"
#include "itkSelectiveSubsampleGenerator.h"
#include "itkSparseFrequencyContainer.h"
#include "itkSubsample.h"
#include "itkWeightedCentroidKdTreeGenerator.h"
#include "itkWeightedCovarianceCalculator.h"
#include "itkWeightedMeanCalculator.h"

int itkStatisticsPrintTest(int , char* [])
{
  typedef float MeasurementType ;
  typedef float FrequencyType ;
  typedef itk::FixedArray< MeasurementType, 2 > MeasurementVectorType ;
  typedef itk::Image< MeasurementVectorType, 3 > ImageType ;
  typedef itk::PointSet< MeasurementType > PointSetType ;
  typedef itk::Statistics::ListSample< MeasurementVectorType >
    SampleType ;
  typedef itk::Statistics::Histogram< MeasurementType, 2 > HistogramType ;

  itk::Statistics::CovarianceCalculator< SampleType >::Pointer CovarianceCalculatorObj=
    itk::Statistics::CovarianceCalculator< SampleType >::New();
  std::cout << "----------CovarianceCalculator " << CovarianceCalculatorObj;

  itk::Statistics::DenseFrequencyContainer< FrequencyType >::Pointer DenseFrequencyContainerObj=
    itk::Statistics::DenseFrequencyContainer< FrequencyType >::New();
  std::cout << "----------DenseFrequencyContainer " << DenseFrequencyContainerObj;

  itk::Statistics::DistanceToCentroidMembershipFunction< MeasurementVectorType >::Pointer
    DistanceToCentroidMembershipFunctionObj= 
    itk::Statistics::DistanceToCentroidMembershipFunction< MeasurementVectorType >::New();
  std::cout << "----------DistanceToCentroidMembershipFunction " << DistanceToCentroidMembershipFunctionObj;

  itk::Statistics::EuclideanDistance< MeasurementVectorType >::Pointer EuclideanDistanceObj=
    itk::Statistics::EuclideanDistance< MeasurementVectorType >::New();
  std::cout << "----------EuclideanDistance " << EuclideanDistanceObj;

  itk::Statistics::ExpectationMaximizationMixtureModelEstimator< SampleType >::Pointer ExpectationMaximizationMixtureModelEstimatorObj=
    itk::Statistics::ExpectationMaximizationMixtureModelEstimator< SampleType >::New();
  std::cout << "----------ExpectationMaximizationMixtureModelEstimator " << ExpectationMaximizationMixtureModelEstimatorObj;

  itk::Statistics::GaussianDensityFunction< MeasurementVectorType >::Pointer GaussianDensityFunctionObj=
    itk::Statistics::GaussianDensityFunction< MeasurementVectorType >::New();
  std::cout << "----------GaussianDensityFunction " << GaussianDensityFunctionObj;

  itk::Statistics::GaussianGoodnessOfFitComponent< SampleType >::Pointer GaussianGoodnessOfFitComponentObj=
    itk::Statistics::GaussianGoodnessOfFitComponent< SampleType >::New();
  std::cout << "----------GaussianGoodnessOfFitComponent " << GaussianGoodnessOfFitComponentObj;

  itk::Statistics::GaussianMixtureModelComponent< SampleType >::Pointer GaussianMixtureModelComponentObj=
    itk::Statistics::GaussianMixtureModelComponent< SampleType >::New();
  std::cout << "----------GaussianMixtureModelComponent " << GaussianMixtureModelComponentObj;

  itk::Statistics::GoodnessOfFitFunctionBase< HistogramType >::Pointer GoodnessOfFitFunctionBaseObj=
    itk::Statistics::GoodnessOfFitFunctionBase< HistogramType >::New();
  std::cout << "----------GoodnessOfFitFunctionBase " << GoodnessOfFitFunctionBaseObj;

  itk::Statistics::GoodnessOfFitMixtureModelCostFunction< SampleType >::Pointer GoodnessOfFitMixtureModelCostFunctionObj=
    itk::Statistics::GoodnessOfFitMixtureModelCostFunction< SampleType >::New();
  std::cout << "----------GoodnessOfFitMixtureModelCostFunction " << GoodnessOfFitMixtureModelCostFunctionObj;

  HistogramType::Pointer HistogramObj=
    HistogramType::New();
  std::cout << "----------Histogram " << HistogramObj;

  itk::Statistics::HypersphereKernelMeanShiftModeSeeker< SampleType >::Pointer HypersphereKernelMeanShiftModeSeekerObj=
    itk::Statistics::HypersphereKernelMeanShiftModeSeeker< SampleType >::New();
  std::cout << "----------HypersphereKernelMeanShiftModeSeeker " << HypersphereKernelMeanShiftModeSeekerObj;

   itk::Statistics::ImageToListAdaptor< ImageType >::Pointer ImageToListAdaptorObj=
    itk::Statistics::ImageToListAdaptor< ImageType >::New();
  std::cout << "----------ImageToListAdaptor " << ImageToListAdaptorObj;

   itk::Statistics::JointDomainImageToListAdaptor< ImageType >::Pointer JointDomainImageToListAdaptorObj=
    itk::Statistics::JointDomainImageToListAdaptor< ImageType >::New();
  std::cout << "----------JointDomainImageToListAdaptor " << JointDomainImageToListAdaptorObj;

  itk::Statistics::KdTree< SampleType >::Pointer KdTreeObj=
    itk::Statistics::KdTree< SampleType >::New();
  std::cout << "----------KdTree " << KdTreeObj;

  typedef itk::Statistics::KdTree< SampleType > KdTreeType ;

  itk::Statistics::KdTreeBasedKmeansEstimator< KdTreeType >::Pointer KdTreeBasedKmeansEstimatorObj=
    itk::Statistics::KdTreeBasedKmeansEstimator< KdTreeType >::New();
  std::cout << "----------KdTreeBasedKmeansEstimator " << KdTreeBasedKmeansEstimatorObj;

  itk::Statistics::KdTreeGenerator< SampleType >::Pointer KdTreeGeneratorObj=
    itk::Statistics::KdTreeGenerator< SampleType >::New();
  std::cout << "----------KdTreeGenerator " << KdTreeGeneratorObj;

  itk::Statistics::ListSample< MeasurementVectorType >::Pointer ListSampleObj=
    itk::Statistics::ListSample< MeasurementVectorType >::New();
  std::cout << "----------ListSample " << ListSampleObj;

  itk::Statistics::ListSampleToHistogramFilter< SampleType, HistogramType >::Pointer ListSampleToHistogramFilterObj=
    itk::Statistics::ListSampleToHistogramFilter< SampleType, HistogramType >::New();
  std::cout << "----------ListSampleToHistogramFilter " << ListSampleToHistogramFilterObj;

  itk::Statistics::ListSampleToHistogramGenerator< SampleType, float >::Pointer ListSampleToHistogramGeneratorObj=
    itk::Statistics::ListSampleToHistogramGenerator< SampleType, float >::New();
  std::cout << "----------ListSampleToHistogramGenerator " << ListSampleToHistogramGeneratorObj;

  itk::Statistics::LogLikelihoodGoodnessOfFitFunction< HistogramType >::Pointer LogLikelihoodGoodnessOfFitFunctionObj=
    itk::Statistics::LogLikelihoodGoodnessOfFitFunction< HistogramType >::New();
  std::cout << "----------LogLikelihoodGoodnessOfFitFunction " << LogLikelihoodGoodnessOfFitFunctionObj;

  itk::Statistics::MahalanobisDistanceMembershipFunction< MeasurementVectorType >::Pointer MahalanobisDistanceMembershipFunctionObj=
    itk::Statistics::MahalanobisDistanceMembershipFunction< MeasurementVectorType >::New();
  std::cout << "----------MahalanobisDistanceMembershipFunction " << MahalanobisDistanceMembershipFunctionObj;

  itk::Statistics::MeanCalculator< SampleType >::Pointer MeanCalculatorObj=
    itk::Statistics::MeanCalculator< SampleType >::New();
  std::cout << "----------MeanCalculator " << MeanCalculatorObj;

  itk::Statistics::MeanShiftModeCacheMethod< MeasurementVectorType >::Pointer MeanShiftModeCacheMethodObj=
    itk::Statistics::MeanShiftModeCacheMethod< MeasurementVectorType >::New();
  std::cout << "----------MeanShiftModeCacheMethod " << MeanShiftModeCacheMethodObj;

  itk::Statistics::MembershipSample< SampleType >::Pointer MembershipSampleObj=
    itk::Statistics::MembershipSample< SampleType >::New();
  std::cout << "----------MembershipSample " << MembershipSampleObj;

  itk::Statistics::MembershipSampleGenerator< SampleType, SampleType >::Pointer MembershipSampleGeneratorObj=
    itk::Statistics::MembershipSampleGenerator< SampleType, SampleType >::New();
  std::cout << "----------MembershipSampleGenerator " << MembershipSampleGeneratorObj;

  itk::Statistics::MixtureModelComponentBase< SampleType >::Pointer MixtureModelComponentBaseObj=
    itk::Statistics::MixtureModelComponentBase< SampleType >::New();
  std::cout << "----------MixtureModelComponentBase " << MixtureModelComponentBaseObj;

  itk::Statistics::NeighborhoodSampler< SampleType >::Pointer NeighborhoodSamplerObj=
    itk::Statistics::NeighborhoodSampler< SampleType >::New();
  std::cout << "----------NeighborhoodSampler " << NeighborhoodSamplerObj;

  itk::Statistics::NormalVariateGenerator::Pointer NormalVariateGeneratorObj=
    itk::Statistics::NormalVariateGenerator::New();
  std::cout << "----------NormalVariateGenerator " << NormalVariateGeneratorObj;

  itk::Statistics::PointSetToListAdaptor< PointSetType >::Pointer PointSetToListAdaptorObj=
    itk::Statistics::PointSetToListAdaptor< PointSetType >::New();
  std::cout << "----------PointSetToListAdaptor " << PointSetToListAdaptorObj;

  itk::Statistics::SampleAlgorithmBase< SampleType >::Pointer SampleAlgorithmBaseObj=
    itk::Statistics::SampleAlgorithmBase< SampleType >::New();
  std::cout << "----------SampleAlgorithmBase " << SampleAlgorithmBaseObj;

  itk::Statistics::SampleClassifier< SampleType >::Pointer SampleClassifierObj=
    itk::Statistics::SampleClassifier< SampleType >::New();
  std::cout << "----------SampleClassifier " << SampleClassifierObj;

  itk::Statistics::SampleClassifierWithMask< SampleType, SampleType >::Pointer SampleClassifierWithMaskObj=
    itk::Statistics::SampleClassifierWithMask< SampleType, SampleType >::New();
  std::cout << "----------SampleClassifierWithMask " << SampleClassifierWithMaskObj;

  itk::Statistics::SampleToHistogramProjectionFilter< SampleType, MeasurementType >::Pointer SampleToHistogramProjectionFilterObj=
    itk::Statistics::SampleToHistogramProjectionFilter< SampleType, MeasurementType >::New();
  std::cout << "----------SampleToHistogramProjectionFilter " << SampleToHistogramProjectionFilterObj;

  itk::Statistics::SampleMeanShiftBlurringFilter< SampleType >::Pointer SampleMeanShiftBlurringFilterObj=
    itk::Statistics::SampleMeanShiftBlurringFilter< SampleType >::New();
  std::cout << "----------SampleMeanShiftBlurringFilter " << SampleMeanShiftBlurringFilterObj;

  itk::Statistics::SampleMeanShiftClusteringFilter< SampleType >::Pointer SampleMeanShiftClusteringFilterObj=
    itk::Statistics::SampleMeanShiftClusteringFilter< SampleType >::New();
  std::cout << "----------SampleMeanShiftClusteringFilter " << SampleMeanShiftClusteringFilterObj;

  itk::Statistics::SampleSelectiveMeanShiftBlurringFilter< SampleType >::Pointer SampleSelectiveMeanShiftBlurringFilterObj=
    itk::Statistics::SampleSelectiveMeanShiftBlurringFilter< SampleType >::New();
  std::cout << "----------SampleSelectiveMeanShiftBlurringFilter " << SampleSelectiveMeanShiftBlurringFilterObj;

  itk::Statistics::SelectiveSubsampleGenerator< SampleType, SampleType >::Pointer SelectiveSubsampleGeneratorObj=
    itk::Statistics::SelectiveSubsampleGenerator< SampleType, SampleType >::New();
  std::cout << "----------SelectiveSubsampleGenerator " << SelectiveSubsampleGeneratorObj;

  itk::Statistics::SparseFrequencyContainer< FrequencyType >::Pointer SparseFrequencyContainerObj=
    itk::Statistics::SparseFrequencyContainer< FrequencyType >::New();
  std::cout << "----------SparseFrequencyContainer " << SparseFrequencyContainerObj;

  itk::Statistics::Subsample< SampleType >::Pointer SubsampleObj=
    itk::Statistics::Subsample< SampleType >::New();
  std::cout << "----------Subsample " << SubsampleObj;

  itk::Statistics::WeightedCentroidKdTreeGenerator< SampleType >::Pointer WeightedCentroidKdTreeGeneratorObj=
    itk::Statistics::WeightedCentroidKdTreeGenerator< SampleType >::New();
  std::cout << "----------WeightedCentroidKdTreeGenerator " << WeightedCentroidKdTreeGeneratorObj;

  itk::Statistics::WeightedCovarianceCalculator< SampleType >::Pointer WeightedCovarianceCalculatorObj=
    itk::Statistics::WeightedCovarianceCalculator< SampleType >::New();
  std::cout << "----------WeightedCovarianceCalculator " << WeightedCovarianceCalculatorObj;

  itk::Statistics::WeightedMeanCalculator< SampleType >::Pointer WeightedMeanCalculatorObj=
    itk::Statistics::WeightedMeanCalculator< SampleType >::New();
  std::cout << "----------WeightedMeanCalculator " << WeightedMeanCalculatorObj;

  return 0;
}

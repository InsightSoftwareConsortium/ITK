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

#include "itkCovarianceCalculator.h"
#include "itkDenseFrequencyContainer.h"
#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkEuclideanDistance.h"
#include "itkExpectationMaximizationMixtureModelEstimator.h"
#include "itkGaussianDensityFunction.h"
#include "itkGaussianGoodnessOfFitComponent.h"
#include "itkGaussianMixtureModelComponent.h"
#include "itkGoodnessOfFitFunctionBase.h"
#include "itkGoodnessOfFitMixtureModelCostFunction.h"
#include "itkHistogram.h"
#include "itkImageToListAdaptor.h"
#include "itkKdTree.h"
#include "itkKdTreeBasedKmeansEstimator.h"
#include "itkKdTreeGenerator.h"
#include "itkListSample.h"
#include "itkListSampleToHistogramFilter.h"
#include "itkListSampleToHistogramGenerator.h"
#include "itkLogLikelihoodGoodnessOfFitFunction.h"
#include "itkMahalanobisDistanceMembershipFunction.h"
#include "itkMeanCalculator.h"
#include "itkMembershipSample.h"
#include "itkMembershipSampleGenerator.h"
#include "itkMixtureModelComponentBase.h"
#include "itkNeighborhoodSampler.h"
#include "itkNormalVariateGenerator.h"
#include "itkPointSetToListAdaptor.h"
#include "itkSampleAlgorithmBase.h"
#include "itkSampleClassifier.h"
#include "itkSampleClassifierWithMask.h"
#include "itkSampleToHistogramProjectionFilter.h"
#include "itkSelectiveSubsampleGenerator.h"
#include "itkSparseFrequencyContainer.h"
#include "itkSubsample.h"
#include "itkTableLookupSampleClassifier.h"
#include "itkWeightedCenteroidKdTreeGenerator.h"
#include "itkWeightedCovarianceCalculator.h"
#include "itkWeightedMeanCalculator.h"

int itkStatisticsPrintTest(int , char* [])
{
  typedef float MeasurementType ;
  typedef itk::FixedArray< MeasurementType, 1 > 
    MeasurementVectorType ;
  typedef itk::Image< MeasurementVectorType, 3 > ImageType ;
  typedef  itk::Statistics::ImageToListAdaptor< ImageType >
    ImageToListAdaptorType ;

  itk::Statistics::CovarianceCalculator<ImageToListAdaptorType>::Pointer CovarianceCalculatorObj=
    itk::Statistics::CovarianceCalculator<ImageToListAdaptorType>::New();
  std::cout << "----------CovarianceCalculator " << CovarianceCalculatorObj;

#if 0
  itk::Statistics::DenseFrequencyContainer<foo>::Pointer DenseFrequencyContainerObj=
    itk::Statistics::DenseFrequencyContainer<foo>::New();
  std::cout << "----------DenseFrequencyContainer " << DenseFrequencyContainerObj;

  itk::Statistics::DistanceToCentroidMembershipFunction<foo>::Pointer DistanceToCentroidMembershipFunctionObj=
    itk::Statistics::DistanceToCentroidMembershipFunction<foo>::New();
  std::cout << "----------DistanceToCentroidMembershipFunction " << DistanceToCentroidMembershipFunctionObj;

  itk::Statistics::EuclideanDistance<foo>::Pointer EuclideanDistanceObj=
    itk::Statistics::EuclideanDistance<foo>::New();
  std::cout << "----------EuclideanDistance " << EuclideanDistanceObj;

  itk::Statistics::ExpectationMaximizationMixtureModelEstimator<foo>::Pointer ExpectationMaximizationMixtureModelEstimatorObj=
    itk::Statistics::ExpectationMaximizationMixtureModelEstimator<foo>::New();
  std::cout << "----------ExpectationMaximizationMixtureModelEstimator " << ExpectationMaximizationMixtureModelEstimatorObj;

  itk::Statistics::GaussianDensityFunction<foo>::Pointer GaussianDensityFunctionObj=
    itk::Statistics::GaussianDensityFunction<foo>::New();
  std::cout << "----------GaussianDensityFunction " << GaussianDensityFunctionObj;

  itk::Statistics::GaussianGoodnessOfFitComponent<foo>::Pointer GaussianGoodnessOfFitComponentObj=
    itk::Statistics::GaussianGoodnessOfFitComponent<foo>::New();
  std::cout << "----------GaussianGoodnessOfFitComponent " << GaussianGoodnessOfFitComponentObj;

  itk::Statistics::GaussianMixtureModelComponent<foo>::Pointer GaussianMixtureModelComponentObj=
    itk::Statistics::GaussianMixtureModelComponent<foo>::New();
  std::cout << "----------GaussianMixtureModelComponent " << GaussianMixtureModelComponentObj;

  itk::Statistics::GoodnessOfFitFunctionBase<foo>::Pointer GoodnessOfFitFunctionBaseObj=
    itk::Statistics::GoodnessOfFitFunctionBase<foo>::New();
  std::cout << "----------GoodnessOfFitFunctionBase " << GoodnessOfFitFunctionBaseObj;

  itk::Statistics::GoodnessOfFitMixtureModelCostFunction<foo>::Pointer GoodnessOfFitMixtureModelCostFunctionObj=
    itk::Statistics::GoodnessOfFitMixtureModelCostFunction<foo>::New();
  std::cout << "----------GoodnessOfFitMixtureModelCostFunction " << GoodnessOfFitMixtureModelCostFunctionObj;

  itk::Statistics::Histogram<foo>::Pointer HistogramObj=
    itk::Statistics::Histogram<foo>::New();
  std::cout << "----------Histogram " << HistogramObj;

  itk::Statistics::ImageToListAdaptor<foo>::Pointer ImageToListAdaptorObj=
    itk::Statistics::ImageToListAdaptor<foo>::New();
  std::cout << "----------ImageToListAdaptor " << ImageToListAdaptorObj;

  itk::Statistics::KdTree<foo>::Pointer KdTreeObj=
    itk::Statistics::KdTree<foo>::New();
  std::cout << "----------KdTree " << KdTreeObj;

  itk::Statistics::KdTreeBasedKmeansEstimator<foo>::Pointer KdTreeBasedKmeansEstimatorObj=
    itk::Statistics::KdTreeBasedKmeansEstimator<foo>::New();
  std::cout << "----------KdTreeBasedKmeansEstimator " << KdTreeBasedKmeansEstimatorObj;

  itk::Statistics::KdTreeGenerator<foo>::Pointer KdTreeGeneratorObj=
    itk::Statistics::KdTreeGenerator<foo>::New();
  std::cout << "----------KdTreeGenerator " << KdTreeGeneratorObj;

  itk::Statistics::ListSample<foo>::Pointer ListSampleObj=
    itk::Statistics::ListSample<foo>::New();
  std::cout << "----------ListSample " << ListSampleObj;

  itk::Statistics::ListSampleToHistogramFilter<foo>::Pointer ListSampleToHistogramFilterObj=
    itk::Statistics::ListSampleToHistogramFilter<foo>::New();
  std::cout << "----------ListSampleToHistogramFilter " << ListSampleToHistogramFilterObj;

  itk::Statistics::ListSampleToHistogramGenerator<foo>::Pointer ListSampleToHistogramGeneratorObj=
    itk::Statistics::ListSampleToHistogramGenerator<foo>::New();
  std::cout << "----------ListSampleToHistogramGenerator " << ListSampleToHistogramGeneratorObj;

  itk::Statistics::LogLikelihoodGoodnessOfFitFunction<foo>::Pointer LogLikelihoodGoodnessOfFitFunctionObj=
    itk::Statistics::LogLikelihoodGoodnessOfFitFunction<foo>::New();
  std::cout << "----------LogLikelihoodGoodnessOfFitFunction " << LogLikelihoodGoodnessOfFitFunctionObj;

  itk::Statistics::MahalanobisDistanceMembershipFunction<foo>::Pointer MahalanobisDistanceMembershipFunctionObj=
    itk::Statistics::MahalanobisDistanceMembershipFunction<foo>::New();
  std::cout << "----------MahalanobisDistanceMembershipFunction " << MahalanobisDistanceMembershipFunctionObj;

  itk::Statistics::MeanCalculator<foo>::Pointer MeanCalculatorObj=
    itk::Statistics::MeanCalculator<foo>::New();
  std::cout << "----------MeanCalculator " << MeanCalculatorObj;

  itk::Statistics::MembershipSample<foo>::Pointer MembershipSampleObj=
    itk::Statistics::MembershipSample<foo>::New();
  std::cout << "----------MembershipSample " << MembershipSampleObj;

  itk::Statistics::MembershipSampleGenerator<foo>::Pointer MembershipSampleGeneratorObj=
    itk::Statistics::MembershipSampleGenerator<foo>::New();
  std::cout << "----------MembershipSampleGenerator " << MembershipSampleGeneratorObj;

  itk::Statistics::MixtureModelComponentBase<foo>::Pointer MixtureModelComponentBaseObj=
    itk::Statistics::MixtureModelComponentBase<foo>::New();
  std::cout << "----------MixtureModelComponentBase " << MixtureModelComponentBaseObj;

  itk::Statistics::NeighborhoodSampler<foo>::Pointer NeighborhoodSamplerObj=
    itk::Statistics::NeighborhoodSampler<foo>::New();
  std::cout << "----------NeighborhoodSampler " << NeighborhoodSamplerObj;

  itk::Statistics::NormalVariateGenerator<foo>::Pointer NormalVariateGeneratorObj=
    itk::Statistics::NormalVariateGenerator<foo>::New();
  std::cout << "----------NormalVariateGenerator " << NormalVariateGeneratorObj;

  itk::Statistics::PointSetToListAdaptor<foo>::Pointer PointSetToListAdaptorObj=
    itk::Statistics::PointSetToListAdaptor<foo>::New();
  std::cout << "----------PointSetToListAdaptor " << PointSetToListAdaptorObj;

  itk::Statistics::SampleAlgorithmBase<foo>::Pointer SampleAlgorithmBaseObj=
    itk::Statistics::SampleAlgorithmBase<foo>::New();
  std::cout << "----------SampleAlgorithmBase " << SampleAlgorithmBaseObj;

  itk::Statistics::SampleClassifier<foo>::Pointer SampleClassifierObj=
    itk::Statistics::SampleClassifier<foo>::New();
  std::cout << "----------SampleClassifier " << SampleClassifierObj;

  itk::Statistics::SampleClassifierWithMask<foo>::Pointer SampleClassifierWithMaskObj=
    itk::Statistics::SampleClassifierWithMask<foo>::New();
  std::cout << "----------SampleClassifierWithMask " << SampleClassifierWithMaskObj;

  itk::Statistics::SampleToHistogramProjectionFilter<foo>::Pointer SampleToHistogramProjectionFilterObj=
    itk::Statistics::SampleToHistogramProjectionFilter<foo>::New();
  std::cout << "----------SampleToHistogramProjectionFilter " << SampleToHistogramProjectionFilterObj;

  itk::Statistics::SelectiveSubsampleGenerator<foo>::Pointer SelectiveSubsampleGeneratorObj=
    itk::Statistics::SelectiveSubsampleGenerator<foo>::New();
  std::cout << "----------SelectiveSubsampleGenerator " << SelectiveSubsampleGeneratorObj;

  itk::Statistics::SparseFrequencyContainer<foo>::Pointer SparseFrequencyContainerObj=
    itk::Statistics::SparseFrequencyContainer<foo>::New();
  std::cout << "----------SparseFrequencyContainer " << SparseFrequencyContainerObj;

  itk::Statistics::Subsample<foo>::Pointer SubsampleObj=
    itk::Statistics::Subsample<foo>::New();
  std::cout << "----------Subsample " << SubsampleObj;

  itk::Statistics::TableLookupSampleClassifier<foo>::Pointer TableLookupSampleClassifierObj=
    itk::Statistics::TableLookupSampleClassifier<foo>::New();
  std::cout << "----------TableLookupSampleClassifier " << TableLookupSampleClassifierObj;

  itk::Statistics::WeightedCenteroidKdTreeGenerator<foo>::Pointer WeightedCenteroidKdTreeGeneratorObj=
    itk::Statistics::WeightedCenteroidKdTreeGenerator<foo>::New();
  std::cout << "----------WeightedCenteroidKdTreeGenerator " << WeightedCenteroidKdTreeGeneratorObj;

  itk::Statistics::WeightedCovarianceCalculator<foo>::Pointer WeightedCovarianceCalculatorObj=
    itk::Statistics::WeightedCovarianceCalculator<foo>::New();
  std::cout << "----------WeightedCovarianceCalculator " << WeightedCovarianceCalculatorObj;

  itk::Statistics::WeightedMeanCalculator<foo>::Pointer WeightedMeanCalculatorObj=
    itk::Statistics::WeightedMeanCalculator<foo>::New();
  std::cout << "----------WeightedMeanCalculator " << WeightedMeanCalculatorObj;
#endif
  return 0;
}

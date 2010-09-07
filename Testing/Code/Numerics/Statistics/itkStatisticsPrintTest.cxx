/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatisticsPrintTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkFixedArray.h"
#include "itkImage.h"
#include "itkPointSet.h"

// #include "itkSample.h"   // abstract class
// #include "itkSampleToSubsampleFilter.h"   // abstract class
#include "itkHistogram.h"
#include "itkListSample.h"
#include "itkSubsample.h"
#include "itkSampleClassifierFilter.h"
#include "itkSampleToHistogramFilter.h"
#include "itkMembershipSample.h"
#include "itkNeighborhoodSampler.h"
#include "itkImageToListSampleFilter.h"
#include "itkScalarImageToCooccurrenceMatrixFilter.h"
#include "itkScalarImageToCooccurrenceListSampleFilter.h"
#include "itkHistogramToTextureFeaturesFilter.h"
#include "itkScalarImageToTextureFeaturesFilter.h"
#include "itkMeanSampleFilter.h"
#include "itkWeightedMeanSampleFilter.h"
#include "itkCovarianceSampleFilter.h"
#include "itkWeightedCovarianceSampleFilter.h"
#include "itkImageToListSampleAdaptor.h"
#include "itkPointSetToListSampleAdaptor.h"
#include "itkJointDomainImageToListSampleAdaptor.h"
#include "itkDenseFrequencyContainer2.h"
#include "itkSparseFrequencyContainer2.h"
#include "itkMembershipFunctionBase.h"
#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkGaussianMembershipFunction.h"
#include "itkDistanceMetric.h"
#include "itkMaximumDecisionRule2.h"
#include "itkMinimumDecisionRule2.h"
#include "itkEuclideanDistanceMetric.h"
#include "itkEuclideanSquareDistanceMetric.h"
#include "itkMahalanobisDistanceMetric.h"
#include "itkManhattanDistanceMetric.h"
#include "itkImageClassifierFilter.h"
#include "itkImageClassifierFilter.h"
#include "itkKdTreeBasedKmeansEstimator.h"
#include "itkExpectationMaximizationMixtureModelEstimator.h"
#include "itkWeightedCentroidKdTreeGenerator.h"
#include "itkKdTree.h"

int itkStatisticsPrintTest(int , char* [])
{
  typedef float TMeasurementType;
  typedef float FrequencyType;

  typedef itk::FixedArray< TMeasurementType, 2 >  TMeasurementVectorType;
  typedef itk::Image< TMeasurementVectorType, 3 > ImageType;
  typedef itk::Image< unsigned char, 3>           ScalarImageType;
  typedef itk::PointSet< TMeasurementType, 2 >    PointSetType;
  typedef itk::Image< unsigned long , 3 >         OutputImageType;

  typedef itk::Statistics::ListSample< TMeasurementVectorType > SampleType;

  typedef itk::Statistics::Subsample< SampleType > SubSampleType;

  typedef itk::Statistics::Histogram< TMeasurementType > HistogramType;

  typedef itk::Statistics::SampleToHistogramFilter<
    SampleType, HistogramType > SampleToHistogramFilterType;

  typedef itk::Statistics::SampleClassifierFilter<
    SampleType > SampleClassifierFilterType;

  typedef itk::Statistics::ImageClassifierFilter<
    SampleType, ImageType, OutputImageType > ImageClassifierFilterType;

  typedef itk::Statistics::ImageToListSampleFilter<
    ImageType, ScalarImageType > ImageToListSampleFilterType;

  typedef itk::Statistics::ImageToListSampleAdaptor<
    ImageType> ImageToListSampleAdaptorType;

  typedef itk::Statistics::JointDomainImageToListSampleAdaptor<
    ImageType> JointDomainImageToListSampleAdaptorType;

  typedef itk::Statistics::ScalarImageToCooccurrenceMatrixFilter<
    ScalarImageType > ScalarImageToCooccurrenceMatrixFilterType;

  typedef itk::Statistics::ScalarImageToCooccurrenceListSampleFilter<
    ScalarImageType > ScalarImageToCooccurrenceListSampleFilterType;

  typedef itk::Statistics::ScalarImageToTextureFeaturesFilter<
    ScalarImageType > ScalarImageToTextureFeaturesFilterType;

  typedef itk::Statistics::MembershipSample< SampleType > MembershipSampleType;

  typedef itk::Statistics::MembershipFunctionBase< TMeasurementVectorType > MembershipFunctionBaseType;

  typedef itk::Statistics::DistanceToCentroidMembershipFunction<
            TMeasurementVectorType > DistanceToCentroidMembershipFunctionType;

  typedef itk::Statistics::DistanceMetric< TMeasurementVectorType >
    DistanceType;

  typedef itk::Statistics::EuclideanDistanceMetric< TMeasurementVectorType >
    EuclideanDistanceMetricType;

  typedef itk::Statistics::EuclideanSquareDistanceMetric< TMeasurementVectorType >
    EuclideanSquareDistanceMetricType;

  typedef itk::Statistics::MahalanobisDistanceMetric< TMeasurementVectorType >
    MahalanobisDistanceMetricType;

  typedef itk::Statistics::ManhattanDistanceMetric< TMeasurementVectorType >
    ManhattanDistanceMetricType;

  typedef itk::Statistics::MaximumDecisionRule2 MaximumDecisionRuleType;
  typedef itk::Statistics::MinimumDecisionRule2 MinimumDecisionRuleType;


  typedef itk::Statistics::HistogramToTextureFeaturesFilter<
    HistogramType > HistogramToTextureFeaturesFilterType;

  typedef itk::Statistics::MeanSampleFilter< SampleType > MeanSampleFilterType;

  typedef itk::Statistics::WeightedMeanSampleFilter< SampleType > WeightedMeanSampleFilterType;

  typedef itk::Statistics::CovarianceSampleFilter< SampleType > CovarianceSampleFilterType;

  typedef itk::Statistics::WeightedCovarianceSampleFilter< SampleType > WeightedCovarianceSampleFilterType;

  typedef itk::Statistics::NeighborhoodSampler< SampleType > NeighborhoodSamplerType;

  typedef itk::Statistics::PointSetToListSampleAdaptor< PointSetType > PointSetToListSampleAdaptorType;

  typedef itk::Statistics::DenseFrequencyContainer2 DenseFrequencyContainer2Type;

  typedef itk::Statistics::SparseFrequencyContainer2 SparseFrequencyContainer2Type;

  typedef itk::Statistics::ExpectationMaximizationMixtureModelEstimator< SampleType > EMEstimatorType;

  typedef itk::Statistics::WeightedCentroidKdTreeGenerator< SampleType >  TreeGeneratorType;

  typedef itk::Statistics::KdTreeBasedKmeansEstimator< TreeGeneratorType::KdTreeType >  KdTreeBasedKMeansEstimatorType;

  SampleType::Pointer sampleObj = SampleType::New();
  std::cout << "----------ListSample " << sampleObj;

  SubSampleType::Pointer subsampleObj = SubSampleType::New();
  std::cout << "----------Subsample " << subsampleObj;

  HistogramType::Pointer HistogramObj=
    HistogramType::New();
  std::cout << "----------Histogram " << HistogramObj;

  SampleToHistogramFilterType::Pointer SampleToHistogramFilterObj =
    SampleToHistogramFilterType::New();
  std::cout << "----------SampleToHistogramFilter ";
  std::cout << SampleToHistogramFilterObj;

  SampleClassifierFilterType::Pointer SampleClassifierFilterObj =
    SampleClassifierFilterType::New();
  std::cout << "----------SampleClassifierFilter ";
  std::cout << SampleClassifierFilterObj;

  ImageToListSampleFilterType::Pointer ImageToListSampleFilterObj =
    ImageToListSampleFilterType::New();
  std::cout << "----------ImageToListSampleFilter ";
  std::cout << ImageToListSampleFilterObj;

  ImageToListSampleAdaptorType::Pointer ImageToListSampleAdaptorObj =
    ImageToListSampleAdaptorType::New();
  std::cout << "----------ImageToListSampleAdaptor ";
  std::cout << ImageToListSampleAdaptorObj;

  JointDomainImageToListSampleAdaptorType::Pointer JointDomainImageToListSampleAdaptorObj =
    JointDomainImageToListSampleAdaptorType::New();
  std::cout << "----------JointDomainImageToListSampleAdaptor ";
  std::cout << JointDomainImageToListSampleAdaptorObj;

  PointSetToListSampleAdaptorType::Pointer PointSetToListSampleAdaptorObj =
    PointSetToListSampleAdaptorType::New();
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
  std::cout << "----------ScalarImageToCooccurrenceMatrixFilter ";
  std::cout << ScalarImageToCooccurrenceMatrixFilterObj;


  HistogramToTextureFeaturesFilterType::Pointer HistogramToTextureFeaturesFilterObj=
    HistogramToTextureFeaturesFilterType::New();
  std::cout << "----------HistogramToTextureFeaturesFilter " << HistogramToTextureFeaturesFilterObj;

  MembershipSampleType::Pointer MembershipSampleObj =
    MembershipSampleType::New();
  std::cout << "----------MembershipSample " << MembershipSampleObj;

  DistanceToCentroidMembershipFunctionType::Pointer DistanceToCentroidMembershipFunctionObj =
    DistanceToCentroidMembershipFunctionType::New();
  std::cout << "----------DistanceToCentroidMembershipFunction " << DistanceToCentroidMembershipFunctionObj;

  MeanSampleFilterType::Pointer meanFilterObj =
    MeanSampleFilterType::New();
  std::cout << "----------Mean filter " << meanFilterObj;

  WeightedMeanSampleFilterType::Pointer weighedMeanSampleFilterObj =
    WeightedMeanSampleFilterType::New();
  std::cout << "----------WeightedMean filter " << weighedMeanSampleFilterObj;

  CovarianceSampleFilterType::Pointer covarianceFilterObj =
    CovarianceSampleFilterType::New();
  std::cout << "----------Covariance filter " << covarianceFilterObj;

  WeightedCovarianceSampleFilterType::Pointer weighedCovarianceSampleFilterObj =
    WeightedCovarianceSampleFilterType::New();
  std::cout << "----------WeightedCovariance filter " << weighedCovarianceSampleFilterObj;

  NeighborhoodSamplerType::Pointer neighborhoodSamplerObj =
    NeighborhoodSamplerType::New();
  std::cout << "----------NeighborhoodSamplerType filter " << neighborhoodSamplerObj;

  DenseFrequencyContainer2Type::Pointer DenseFrequencyContainer2Obj=
    DenseFrequencyContainer2Type::New();
  std::cout << "----------DenseFrequencyContainer " << DenseFrequencyContainer2Obj;

  SparseFrequencyContainer2Type::Pointer SparseFrequencyContainer2Obj=
    SparseFrequencyContainer2Type::New();
  std::cout << "----------SparseFrequencyContainer2 " << SparseFrequencyContainer2Obj;

  EuclideanDistanceMetricType::Pointer euclideanDistance=
    EuclideanDistanceMetricType::New();
  std::cout << "----------EuclideanDistanceMetricType " << euclideanDistance;

  EuclideanSquareDistanceMetricType::Pointer euclideanSquareDistance=
    EuclideanSquareDistanceMetricType::New();
  std::cout << "----------EuclideanSquareDistanceMetricType " << euclideanSquareDistance;

  MahalanobisDistanceMetricType::Pointer mahalanobisDistance=
    MahalanobisDistanceMetricType::New();
  std::cout << "----------MahalanobisDistanceMetricType " << mahalanobisDistance;

  ManhattanDistanceMetricType::Pointer manhattanDistance=
    ManhattanDistanceMetricType::New();
  std::cout << "----------ManhattanDistanceMetricType " << manhattanDistance;

  MaximumDecisionRuleType::Pointer maximumDecsion=
    MaximumDecisionRuleType::New();
  std::cout << "----------MaximumDecisionRuleType " << maximumDecsion;

  MinimumDecisionRuleType::Pointer minimumDecsion=
    MinimumDecisionRuleType::New();
  std::cout << "----------MinimumDecisionRuleType " << minimumDecsion;

  ImageClassifierFilterType::Pointer classifierFilter=
   ImageClassifierFilterType::New();
  std::cout << "----------ImageClassifierFilterType " << classifierFilter;

  EMEstimatorType::Pointer emEstimator=
   EMEstimatorType::New();
  std::cout << "----------EMEstimatorType " << emEstimator;

  KdTreeBasedKMeansEstimatorType::Pointer kdTreeBasedEstimator=
   KdTreeBasedKMeansEstimatorType::New();
  std::cout << "----------KdTreeBasedKMeansEstimatorType " << kdTreeBasedEstimator;

  return EXIT_SUCCESS;
}

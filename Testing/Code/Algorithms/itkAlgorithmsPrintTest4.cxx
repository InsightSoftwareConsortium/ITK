/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAlgorithmsPrintTest4.cxx
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

#include "itkImage.h"
#include "itkVector.h"
#include "itkPoint.h"
#include "itkMesh.h"
#include "itkEllipseSpatialObject.h"

#include "itkGeodesicActiveContourLevelSetFunction.h"
#include "itkGeodesicActiveContourLevelSetImageFilter.h"
#include "itkGradientVectorFlowImageFilter.h"
#include "itkHistogramMatchingImageFilter.h"
#include "itkImageClassifierBase.h"
#include "itkImageGaussianModelEstimator.h"
#include "itkImageKmeansModelEstimator.h"
#include "itkImagePCAShapeModelEstimator.h"
#include "itkImageRegistrationMethod.h"
#include "itkImageToSpatialObjectMetric.h"
#include "itkImageToSpatialObjectRegistrationMethod.h"
#include "itkKLMRegionGrowImageFilter.h"
#include "itkLaplacianSegmentationLevelSetFunction.h"
#include "itkLaplacianSegmentationLevelSetImageFilter.h"
#include "itkLevelSetNeighborhoodExtractor.h"
#include "itkLevelSetVelocityNeighborhoodExtractor.h"

#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkMahalanobisDistanceMembershipFunction.h"
#include "itkGroupSpatialObject.h"
#include "itkMeanReciprocalSquareDifferencePointSetToImageMetric.h"

int main(int , char* [])
{
  typedef itk::Image<float,2> InputType; 
  typedef itk::Image<float,2> OutputType;
  typedef itk::Image<bool,2> BinaryImageType;
  typedef itk::Image<unsigned short,2> UShortImageType;
  typedef itk::Image<unsigned char,2> CharType;
  
  typedef itk::Mesh<double>  MeshType;
  
  typedef itk::Vector<float,2> VectorType;
  typedef itk::Image<VectorType, 2> VectorImageType;
  typedef itk::PointSet<float,2> PointSetType;
  
  // Used for GradientVectorFlowImageFilter
  typedef itk::CovariantVector<double,2> GradientType;
  typedef itk::Image<GradientType,2>   GradientImageType;

  //Used for ImageKMeansModelEstimator
  typedef itk::Statistics::DistanceToCentroidMembershipFunction<VectorType> KMeansMemFuncType;
  
  // Used for ImageGaussianModelEstimator
  typedef itk::Statistics::MahalanobisDistanceMembershipFunction<VectorType> GaussianMemFuncType;
  
  // Used for ImageToSpatialObjectRegistrationMethod
  typedef itk::GroupSpatialObject<2>   GroupType;

  // Used for ImageToSpatialObjectMetric
  typedef itk::EllipseSpatialObject<2> SpatialObjectType;


  itk::GeodesicActiveContourLevelSetFunction<InputType>::Pointer GeodesicActiveContourLevelSetFunctionObj =
    itk::GeodesicActiveContourLevelSetFunction<InputType>::New();
  std:: cout << "-------------GeodesicActiveContourLevelSetFunction " << GeodesicActiveContourLevelSetFunctionObj;

  itk::GeodesicActiveContourLevelSetImageFilter<InputType,OutputType>::Pointer GeodesicActiveContourLevelSetImageFilterObj =
    itk::GeodesicActiveContourLevelSetImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------GeodesicActiveContourLevelSetImageFilter " << GeodesicActiveContourLevelSetImageFilterObj;

  itk::GradientVectorFlowImageFilter<GradientImageType,GradientImageType>::Pointer GradientVectorFlowImageFilterObj =
    itk::GradientVectorFlowImageFilter<GradientImageType,GradientImageType>::New();
  std:: cout << "-------------GradientVectorFlowImageFilter " << GradientVectorFlowImageFilterObj;

  itk::HistogramMatchingImageFilter<InputType,OutputType>::Pointer HistogramMatchingImageFilterObj =
    itk::HistogramMatchingImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------HistogramMatchingImageFilter " << HistogramMatchingImageFilterObj;

  itk::ImageClassifierBase<InputType,OutputType>::Pointer ImageClassifierBaseObj =
    itk::ImageClassifierBase<InputType,OutputType>::New();
  std:: cout << "-------------ImageClassifierBase " << ImageClassifierBaseObj;

  itk::ImageGaussianModelEstimator<VectorImageType,GaussianMemFuncType,UShortImageType>::Pointer ImageGaussianModelEstimatorObj =
    itk::ImageGaussianModelEstimator<VectorImageType,GaussianMemFuncType,UShortImageType>::New();
  std:: cout << "-------------ImageGaussianModelEstimator " << ImageGaussianModelEstimatorObj;
  
  itk::ImageKmeansModelEstimator<VectorImageType,KMeansMemFuncType>::Pointer ImageKmeansModelEstimatorObj =
    itk::ImageKmeansModelEstimator<VectorImageType,KMeansMemFuncType>::New();
  std:: cout << "-------------ImageKmeansModelEstimator " << ImageKmeansModelEstimatorObj;

  itk::ImageRegistrationMethod<InputType,InputType>::Pointer ImageRegistrationMethodObj =
    itk::ImageRegistrationMethod<InputType,InputType>::New();
  std:: cout << "-------------ImageRegistrationMethod " << ImageRegistrationMethodObj;
#if 0
  itk::ImageToSpatialObjectMetric<InputType,SpatialObjectType>::Pointer ImageToSpatialObjectMetricObj =
    itk::ImageToSpatialObjectMetric<InputType,SpatialObjectType>::New();
  std:: cout << "-------------ImageToSpatialObjectMetric " << ImageToSpatialObjectMetricObj;
#endif
  itk::ImageToSpatialObjectRegistrationMethod<InputType,GroupType>::Pointer ImageToSpatialObjectRegistrationMethodObj =
    itk::ImageToSpatialObjectRegistrationMethod<InputType,GroupType>::New();
  std:: cout << "-------------ImageToSpatialObjectRegistrationMethod " << ImageToSpatialObjectRegistrationMethodObj;

  itk::KLMRegionGrowImageFilter<VectorImageType,VectorImageType>::Pointer KLMRegionGrowImageFilterObj =
    itk::KLMRegionGrowImageFilter<VectorImageType,VectorImageType>::New();
  std:: cout << "-------------KLMRegionGrowImageFilter " << KLMRegionGrowImageFilterObj;

  itk::LaplacianSegmentationLevelSetFunction<InputType,InputType>::Pointer LaplacianSegmentationLevelSetFunctionObj =
    itk::LaplacianSegmentationLevelSetFunction<InputType,InputType>::New();
  std:: cout << "-------------LaplacianSegmentationLevelSetFunction " << LaplacianSegmentationLevelSetFunctionObj;

  itk::LaplacianSegmentationLevelSetImageFilter<InputType,InputType,float>::Pointer LaplacianSegmentationLevelSetImageFilterObj =
    itk::LaplacianSegmentationLevelSetImageFilter<InputType,InputType,float>::New();
  std:: cout << "-------------LaplacianSegmentationLevelSetImageFilter " << LaplacianSegmentationLevelSetImageFilterObj;

  itk::LevelSetNeighborhoodExtractor<InputType>::Pointer LevelSetNeighborhoodExtractorObj =
    itk::LevelSetNeighborhoodExtractor<InputType>::New();
  std:: cout << "-------------LevelSetNeighborhoodExtractor " << LevelSetNeighborhoodExtractorObj;

  itk::LevelSetVelocityNeighborhoodExtractor<InputType,double>::Pointer LevelSetVelocityNeighborhoodExtractorObj =
    itk::LevelSetVelocityNeighborhoodExtractor<InputType,double>::New();

  std:: cout << "-------------LevelSetVelocityNeighborhoodExtractor " << LevelSetVelocityNeighborhoodExtractorObj;

  itk::MeanReciprocalSquareDifferencePointSetToImageMetric<PointSetType,InputType>::Pointer MeanReciprocalSquareDifferencePointSetToImageMetricObj =
    itk::MeanReciprocalSquareDifferencePointSetToImageMetric<PointSetType,InputType>::New();
  std:: cout << "-------------MeanReciprocalSquareDifferencePointSetToImageMetric " << MeanReciprocalSquareDifferencePointSetToImageMetricObj;
  
  return 0;

}

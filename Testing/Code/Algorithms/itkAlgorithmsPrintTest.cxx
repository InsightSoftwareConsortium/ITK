/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAlgorithmsPrintTest.cxx
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

#include "itkImage.h"
#include "itkVector.h"
#include "itkPoint.h"
#include "itkMesh.h"
#include "itkEllipseSpatialObject.h"

#include "itkAntiAliasBinaryImageFilter.h"
#include "itkBalloonForceFilter.h"
#include "itkBinaryMask3DMeshSource.h"
#include "itkBinaryMinMaxCurvatureFlowFunction.h"
#include "itkBinaryMinMaxCurvatureFlowImageFilter.h"
#include "itkCannySegmentationLevelSetFunction.h"
#include "itkCannySegmentationLevelSetImageFilter.h"
#include "itkConnectedRegionsMeshFilter.h"
#include "itkCurvatureFlowFunction.h"
#include "itkCurvatureFlowImageFilter.h"
#include "itkDeformableMesh3DFilter.h"
#include "itkDemonsRegistrationFilter.h"
#include "itkDemonsRegistrationFunction.h"
#include "itkExtensionVelocitiesImageFilter.h"
#include "itkFEMRegistrationFilter.h"
#include "itkFastMarchingExtensionImageFilter.h"
#include "itkFastMarchingImageFilter.h"
#include "itkGeodesicActiveContourLevelSetFunction.h"
#include "itkGeodesicActiveContourLevelSetImageFilter.h"
#include "itkGradientVectorFlowImageFilter.h"
#include "itkHistogramMatchingImageFilter.h"
#include "itkImageClassifierBase.h"
#include "itkImageGaussianModelEstimator.h"
#include "itkImageKmeansModelEstimator.h"
#include "itkImageRegistrationMethod.h"
#include "itkImageToSpatialObjectMetric.h"
#include "itkImageToSpatialObjectRegistrationMethod.h"
#include "itkKLMRegionGrowImageFilter.h"
#include "itkLaplacianSegmentationLevelSetFunction.h"
#include "itkLaplacianSegmentationLevelSetImageFilter.h"
#include "itkLevelSetNeighborhoodExtractor.h"
#include "itkLevelSetVelocityNeighborhoodExtractor.h"
#include "itkMRASlabIdentifier.h"
#include "itkMRFImageFilter.h"
#include "itkMRIBiasFieldCorrectionFilter.h"
#include "itkMattesMutualInformationImageToImageMetric.h"
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkMeanSquaresPointSetToImageMetric.h"
#include "itkMinMaxCurvatureFlowFunction.h"
#include "itkMinMaxCurvatureFlowImageFilter.h"

#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkMahalanobisDistanceMembershipFunction.h"
#include "itkGroupSpatialObject.h"
#include "itkRGBPixel.h"

int itkAlgorithmsPrintTest(int , char* [])
{
  typedef itk::Image<float,2> InputType; 
  typedef itk::Image<float,2> OutputType;
  typedef itk::Image<bool,2> BinaryImageType;
  typedef itk::Image<unsigned short,2> UShortImageType;
  typedef itk::Image<unsigned char,2> CharType;
  
  typedef itk::Mesh<double>  MeshType;
  
  typedef itk::Vector<float,2> VectorType;
  typedef itk::Image<VectorType, 2> VectorImageType;
  
  // Used for NormalizedCorrelationPointSetToImageMetric
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


  itk::AntiAliasBinaryImageFilter<InputType,OutputType>::Pointer AntiAliasBinaryImageFilterObj =
    itk::AntiAliasBinaryImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------AntiAliasBinaryImageFilter " << AntiAliasBinaryImageFilterObj;

  itk::BalloonForceFilter<MeshType,MeshType>::Pointer BalloonForceFilterObj =
    itk::BalloonForceFilter<MeshType,MeshType>::New();
  std:: cout << "-------------BalloonForceFilter " << BalloonForceFilterObj;

  itk::BinaryMask3DMeshSource<MeshType>::Pointer BinaryMask3DMeshSourceObj =
    itk::BinaryMask3DMeshSource<MeshType>::New();
  std:: cout << "-------------BinaryMask3DMeshSource " << BinaryMask3DMeshSourceObj;

  itk::BinaryMinMaxCurvatureFlowFunction<InputType>::Pointer BinaryMinMaxCurvatureFlowFunctionObj =
    itk::BinaryMinMaxCurvatureFlowFunction<InputType>::New();
  std:: cout << "-------------BinaryMinMaxCurvatureFlowFunction " << BinaryMinMaxCurvatureFlowFunctionObj;

  itk::BinaryMinMaxCurvatureFlowImageFilter<InputType,OutputType>::Pointer BinaryMinMaxCurvatureFlowImageFilterObj =
    itk::BinaryMinMaxCurvatureFlowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------BinaryMinMaxCurvatureFlowImageFilter " << BinaryMinMaxCurvatureFlowImageFilterObj;

  itk::CannySegmentationLevelSetFunction<InputType,InputType>::Pointer CannySegmentationLevelSetFunctionObj =
    itk::CannySegmentationLevelSetFunction<InputType,InputType>::New();
  std:: cout << "-------------CannySegmentationLevelSetFunction " << CannySegmentationLevelSetFunctionObj;

  itk::CannySegmentationLevelSetImageFilter<InputType,OutputType>::Pointer CannySegmentationLevelSetImageFilterObj =
    itk::CannySegmentationLevelSetImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------CannySegmentationLevelSetImageFilter " << CannySegmentationLevelSetImageFilterObj;

  itk::ConnectedRegionsMeshFilter<MeshType,MeshType>::Pointer ConnectedRegionsMeshFilterObj =
    itk::ConnectedRegionsMeshFilter<MeshType,MeshType>::New();
  std:: cout << "-------------ConnectedRegionsMeshFilter " << ConnectedRegionsMeshFilterObj;

  itk::CurvatureFlowFunction<InputType>::Pointer CurvatureFlowFunctionObj =
    itk::CurvatureFlowFunction<InputType>::New();
  std:: cout << "-------------CurvatureFlowFunction " << CurvatureFlowFunctionObj;

  itk::CurvatureFlowImageFilter<InputType,OutputType>::Pointer CurvatureFlowImageFilterObj =
    itk::CurvatureFlowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------CurvatureFlowImageFilter " << CurvatureFlowImageFilterObj;

  itk::DeformableMesh3DFilter<MeshType,MeshType>::Pointer DeformableMesh3DFilterObj =
    itk::DeformableMesh3DFilter<MeshType,MeshType>::New();
  std:: cout << "-------------DeformableMesh3DFilter " << DeformableMesh3DFilterObj;

  itk::DemonsRegistrationFilter<InputType,OutputType,VectorImageType>::Pointer DemonsRegistrationFilterObj =
    itk::DemonsRegistrationFilter<InputType,OutputType,VectorImageType>::New();
  std:: cout << "-------------DemonsRegistrationFilter " << DemonsRegistrationFilterObj;

  itk::DemonsRegistrationFunction<InputType,OutputType,VectorImageType>::Pointer DemonsRegistrationFunctionObj =
    itk::DemonsRegistrationFunction<InputType,OutputType,VectorImageType>::New();
  std:: cout << "-------------DemonsRegistrationFunction " << DemonsRegistrationFunctionObj;

  itk::ExtensionVelocitiesImageFilter<InputType,float,1>::Pointer ExtensionVelocitiesImageFilterObj =
    itk::ExtensionVelocitiesImageFilter<InputType,float,1>::New();
  std:: cout << "-------------ExtensionVelocitiesImageFilter " << ExtensionVelocitiesImageFilterObj;

  itk::fem::FEMRegistrationFilter<InputType,InputType>::Pointer FEMRegistrationFilterObj =
    itk::fem::FEMRegistrationFilter<InputType,InputType>::New();
  std:: cout << "-------------FEMRegistrationFilter " << FEMRegistrationFilterObj;

  itk::FastMarchingExtensionImageFilter<InputType,float>::Pointer FastMarchingExtensionImageFilterObj =
    itk::FastMarchingExtensionImageFilter<InputType,float>::New();
  std:: cout << "-------------FastMarchingExtensionImageFilter " << FastMarchingExtensionImageFilterObj;

  itk::FastMarchingImageFilter<InputType>::Pointer FastMarchingImageFilterObj =
    itk::FastMarchingImageFilter<InputType>::New();
  std:: cout << "-------------FastMarchingImageFilter " << FastMarchingImageFilterObj;

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

  itk::MRASlabIdentifier<InputType>::Pointer MRASlabIdentifierObj =
    itk::MRASlabIdentifier<InputType>::New();
  std:: cout << "-------------MRASlabIdentifier " << MRASlabIdentifierObj;

  itk::MRFImageFilter<VectorImageType,UShortImageType>::Pointer MRFImageFilterObj =
    itk::MRFImageFilter<VectorImageType,UShortImageType>::New();
  std:: cout << "-------------MRFImageFilter " << MRFImageFilterObj;
#if 0
  itk::MRIBiasFieldCorrectionFilter<InputType,InputType,VectorImageType>::Pointer MRIBiasFieldCorrectionFilterObj =
    itk::MRIBiasFieldCorrectionFilter<InputType,InputType,VectorImageType>::New();
  std:: cout << "-------------MRIBiasFieldCorrectionFilter " << MRIBiasFieldCorrectionFilterObj;
#endif
  itk::MattesMutualInformationImageToImageMetric<InputType,InputType>::Pointer MattesMutualInformationImageToImageMetricObj =
    itk::MattesMutualInformationImageToImageMetric<InputType,InputType>::New();
  std:: cout << "-------------MattesMutualInformationImageToImageMetric " << MattesMutualInformationImageToImageMetricObj;

  itk::MeanSquaresImageToImageMetric<InputType,InputType>::Pointer MeanSquaresImageToImageMetricObj =
    itk::MeanSquaresImageToImageMetric<InputType,InputType>::New();
  std:: cout << "-------------MeanSquaresImageToImageMetric " << MeanSquaresImageToImageMetricObj;
#if 0
  itk::MeanSquaresPointSetToImageMetric<InputType,OutputType>::Pointer MeanSquaresPointSetToImageMetricObj =
    itk::MeanSquaresPointSetToImageMetric<InputType,OutputType>::New();
  std:: cout << "-------------MeanSquaresPointSetToImageMetric " << MeanSquaresPointSetToImageMetricObj;
#endif
  itk::MinMaxCurvatureFlowFunction<InputType>::Pointer MinMaxCurvatureFlowFunctionObj =
    itk::MinMaxCurvatureFlowFunction<InputType>::New();
  std:: cout << "-------------MinMaxCurvatureFlowFunction " << MinMaxCurvatureFlowFunctionObj;

  itk::MinMaxCurvatureFlowImageFilter<InputType,OutputType>::Pointer MinMaxCurvatureFlowImageFilterObj =
    itk::MinMaxCurvatureFlowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------MinMaxCurvatureFlowImageFilter " << MinMaxCurvatureFlowImageFilterObj;

  return 0;

}

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
#include "itkBalloonForce3DFilter.h"
#include "itkBinaryMask3DMeshSource.h"
#include "itkBinaryMinMaxCurvatureFlowFunction.h"
#include "itkBinaryMinMaxCurvatureFlowImageFilter.h"
#include "itkCannySegmentationLevelSetFunction.h"
#include "itkCannySegmentationLevelSetImageFilter.h"
#include "itkConnectedRegionsMeshFilter.h"
#include "itkCurvatureFlowFunction.h"
#include "itkCurvatureFlowImageFilter.h"
#include "itkDeformableMesh3DFilter.h"
#include "itkDeformableMeshFilter.h"
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
#include "itkMultiResolutionImageRegistrationMethod.h"
#include "itkMultiResolutionPDEDeformableRegistration.h"
#include "itkMultiResolutionPyramidImageFilter.h"
#include "itkMutualInformationImageToImageMetric.h"
#include "itkNormalizedCorrelationImageToImageMetric.h"
#include "itkNormalizedCorrelationPointSetToImageMetric.h"
#include "itkOtsuThresholdImageCalculator.h"
#include "itkPDEDeformableRegistrationFilter.h"
#include "itkPatternIntensityImageToImageMetric.h"
#if 0
#include "itkPatternIntensityPointSetToImageMetric.h"
#endif
#include "itkRGBGibbsPriorFilter.h"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkRegionGrowImageFilter.h"
#include "itkRegistrationMethod.h"
#include "itkReinitializeLevelSetImageFilter.h"
#include "itkShapeDetectionLevelSetFunction.h"
#include "itkShapeDetectionLevelSetImageFilter.h"
#include "itkSimpleFuzzyConnectednessImageFilterBase.h"
#include "itkSimpleFuzzyConnectednessRGBImageFilter.h"
#include "itkSimpleFuzzyConnectednessScalarImageFilter.h"
#include "itkSphereMeshSource.h"
#include "itkThresholdSegmentationLevelSetFunction.h"
#include "itkThresholdSegmentationLevelSetImageFilter.h"
#include "itkVectorFuzzyConnectednessImageFilter.h"
#include "itkVoronoiDiagram2D.h"
#include "itkVoronoiDiagram2DGenerator.h"
#include "itkVoronoiPartitioningImageFilter.h"
#include "itkVoronoiSegmentationImageFilter.h"
#include "itkVoronoiSegmentationImageFilterBase.h"
#include "itkVoronoiSegmentationRGBImageFilter.h"
#include "itkWatershedBoundary.h"
#include "itkWatershedBoundaryResolver.h"
#include "itkWatershedEquivalenceRelabeler.h"
#include "itkWatershedEquivalencyTable.h"
#include "itkWatershedImageFilter.h"
#include "itkWatershedMiniPipelineProgressCommand.h"
#include "itkWatershedOneWayEquivalencyTable.h"
#include "itkWatershedRelabeler.h"
#include "itkWatershedSegmentTable.h"
#include "itkWatershedSegmentTree.h"
#include "itkWatershedSegmentTreeGenerator.h"
#include "itkWatershedSegmenter.h"

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

  itk::BalloonForce3DFilter<MeshType,MeshType>::Pointer BalloonForce3DFilterObj =
    itk::BalloonForce3DFilter<MeshType,MeshType>::New();
  std:: cout << "-------------BalloonForce3DFilter " << BalloonForce3DFilterObj;

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
#if 0
  itk::DeformableMeshFilter<MeshType,MeshType>::Pointer DeformableMeshFilterObj =
    itk::DeformableMeshFilter<MeshType,MeshType>::New();
  std:: cout << "-------------DeformableMeshFilter " << DeformableMeshFilterObj;
#endif
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

  itk::MultiResolutionImageRegistrationMethod<InputType,InputType>::Pointer MultiResolutionImageRegistrationMethodObj =
    itk::MultiResolutionImageRegistrationMethod<InputType,InputType>::New();
  std:: cout << "-------------MultiResolutionImageRegistrationMethod " << MultiResolutionImageRegistrationMethodObj;

  itk::MultiResolutionPDEDeformableRegistration<InputType,OutputType,VectorImageType>::Pointer MultiResolutionPDEDeformableRegistrationObj =
    itk::MultiResolutionPDEDeformableRegistration<InputType,OutputType,VectorImageType>::New();
  std:: cout << "-------------MultiResolutionPDEDeformableRegistration " << MultiResolutionPDEDeformableRegistrationObj;

  itk::MultiResolutionPyramidImageFilter<InputType,OutputType>::Pointer MultiResolutionPyramidImageFilterObj =
    itk::MultiResolutionPyramidImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------MultiResolutionPyramidImageFilter " << MultiResolutionPyramidImageFilterObj;

  itk::MutualInformationImageToImageMetric<InputType,InputType>::Pointer MutualInformationImageToImageMetricObj =
    itk::MutualInformationImageToImageMetric<InputType,InputType>::New();
  std:: cout << "-------------MutualInformationImageToImageMetric " << MutualInformationImageToImageMetricObj;

  itk::NormalizedCorrelationImageToImageMetric<InputType,InputType>::Pointer NormalizedCorrelationImageToImageMetricObj =
    itk::NormalizedCorrelationImageToImageMetric<InputType,InputType>::New();
  std:: cout << "-------------NormalizedCorrelationImageToImageMetric " << NormalizedCorrelationImageToImageMetricObj;

  itk::NormalizedCorrelationPointSetToImageMetric<PointSetType,InputType>::Pointer NormalizedCorrelationPointSetToImageMetricObj =
    itk::NormalizedCorrelationPointSetToImageMetric<PointSetType,InputType>::New();
  std:: cout << "-------------NormalizedCorrelationPointSetToImageMetric " << NormalizedCorrelationPointSetToImageMetricObj;

  itk::OtsuThresholdImageCalculator<InputType>::Pointer OtsuThresholdImageCalculatorObj =
    itk::OtsuThresholdImageCalculator<InputType>::New();
  std:: cout << "-------------OtsuThresholdImageCalculator " << OtsuThresholdImageCalculatorObj;

  itk::PDEDeformableRegistrationFilter<InputType,InputType,VectorImageType>::Pointer PDEDeformableRegistrationFilterObj =
    itk::PDEDeformableRegistrationFilter<InputType,InputType,VectorImageType>::New();
  std:: cout << "-------------PDEDeformableRegistrationFilter " << PDEDeformableRegistrationFilterObj;

  itk::PatternIntensityImageToImageMetric<InputType,InputType>::Pointer PatternIntensityImageToImageMetricObj =
    itk::PatternIntensityImageToImageMetric<InputType,InputType>::New();
  std:: cout << "-------------PatternIntensityImageToImageMetric " << PatternIntensityImageToImageMetricObj;
#if 0
  itk::PatternIntensityPointSetToImageMetric<InputType,OutputType>::Pointer PatternIntensityPointSetToImageMetricObj =
    itk::PatternIntensityPointSetToImageMetric<InputType,OutputType>::New();
  std:: cout << "-------------PatternIntensityPointSetToImageMetric " << PatternIntensityPointSetToImageMetricObj;
#endif
  itk::RGBGibbsPriorFilter<VectorImageType,UShortImageType>::Pointer RGBGibbsPriorFilterObj =
    itk::RGBGibbsPriorFilter<VectorImageType,UShortImageType>::New();
  std:: cout << "-------------RGBGibbsPriorFilter " << RGBGibbsPriorFilterObj;
  
  itk::RecursiveMultiResolutionPyramidImageFilter<InputType,OutputType>::Pointer RecursiveMultiResolutionPyramidImageFilterObj =
    itk::RecursiveMultiResolutionPyramidImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------RecursiveMultiResolutionPyramidImageFilter " << RecursiveMultiResolutionPyramidImageFilterObj;

  itk::RegionGrowImageFilter<InputType,OutputType>::Pointer RegionGrowImageFilterObj =
    itk::RegionGrowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------RegionGrowImageFilter " << RegionGrowImageFilterObj;
#if 0
  itk::RegistrationMethod<InputType,OutputType>::Pointer RegistrationMethodObj =
    itk::RegistrationMethod<InputType,OutputType>::New();
  std:: cout << "-------------RegistrationMethod " << RegistrationMethodObj;
#endif
  itk::ReinitializeLevelSetImageFilter<InputType>::Pointer ReinitializeLevelSetImageFilterObj =
    itk::ReinitializeLevelSetImageFilter<InputType>::New();
  std:: cout << "-------------ReinitializeLevelSetImageFilter " << ReinitializeLevelSetImageFilterObj;

  itk::ShapeDetectionLevelSetFunction<InputType,InputType>::Pointer ShapeDetectionLevelSetFunctionObj =
    itk::ShapeDetectionLevelSetFunction<InputType,InputType>::New();
  std:: cout << "-------------ShapeDetectionLevelSetFunction " << ShapeDetectionLevelSetFunctionObj;

  itk::ShapeDetectionLevelSetImageFilter<InputType,InputType,float>::Pointer ShapeDetectionLevelSetImageFilterObj =
    itk::ShapeDetectionLevelSetImageFilter<InputType,InputType,float>::New();
  std:: cout << "-------------ShapeDetectionLevelSetImageFilter " << ShapeDetectionLevelSetImageFilterObj;

  itk::SimpleFuzzyConnectednessImageFilterBase<InputType,OutputType>::Pointer SimpleFuzzyConnectednessImageFilterBaseObj =
    itk::SimpleFuzzyConnectednessImageFilterBase<InputType,OutputType>::New();
  std:: cout << "-------------SimpleFuzzyConnectednessImageFilterBase " << SimpleFuzzyConnectednessImageFilterBaseObj;

  itk::SimpleFuzzyConnectednessRGBImageFilter<VectorImageType,BinaryImageType>::Pointer SimpleFuzzyConnectednessRGBImageFilterObj =
    itk::SimpleFuzzyConnectednessRGBImageFilter<VectorImageType,BinaryImageType>::New();
  std:: cout << "-------------SimpleFuzzyConnectednessRGBImageFilter " << SimpleFuzzyConnectednessRGBImageFilterObj;

  itk::SimpleFuzzyConnectednessScalarImageFilter<UShortImageType,BinaryImageType>::Pointer SimpleFuzzyConnectednessScalarImageFilterObj =
    itk::SimpleFuzzyConnectednessScalarImageFilter<UShortImageType,BinaryImageType>::New();
  std:: cout << "-------------SimpleFuzzyConnectednessScalarImageFilter " << SimpleFuzzyConnectednessScalarImageFilterObj;

  itk::SphereMeshSource<MeshType>::Pointer SphereMeshSourceObj =
    itk::SphereMeshSource<MeshType>::New();
  std:: cout << "-------------SphereMeshSource " << SphereMeshSourceObj;

  itk::ThresholdSegmentationLevelSetFunction<InputType,InputType>::Pointer ThresholdSegmentationLevelSetFunctionObj =
    itk::ThresholdSegmentationLevelSetFunction<InputType,InputType>::New();
  std:: cout << "-------------ThresholdSegmentationLevelSetFunction " << ThresholdSegmentationLevelSetFunctionObj;

  itk::ThresholdSegmentationLevelSetImageFilter<InputType,InputType,float>::Pointer ThresholdSegmentationLevelSetImageFilterObj =
    itk::ThresholdSegmentationLevelSetImageFilter<InputType,InputType,float>::New();
  std:: cout << "-------------ThresholdSegmentationLevelSetImageFilter " << ThresholdSegmentationLevelSetImageFilterObj;

  itk::VectorFuzzyConnectednessImageFilter<VectorImageType,CharType>::Pointer VectorFuzzyConnectednessImageFilterObj =
    itk::VectorFuzzyConnectednessImageFilter<VectorImageType,CharType>::New();
  std:: cout << "-------------VectorFuzzyConnectednessImageFilter " << VectorFuzzyConnectednessImageFilterObj;

  itk::VoronoiDiagram2D<double>::Pointer VoronoiDiagram2DObj =
    itk::VoronoiDiagram2D<double>::New();
  std:: cout << "-------------VoronoiDiagram2D " << VoronoiDiagram2DObj;

  itk::VoronoiDiagram2DGenerator<double>::Pointer VoronoiDiagram2DGeneratorObj =
    itk::VoronoiDiagram2DGenerator<double>::New();
  std:: cout << "-------------VoronoiDiagram2DGenerator " << VoronoiDiagram2DGeneratorObj;

  itk::VoronoiPartitioningImageFilter<InputType,OutputType>::Pointer VoronoiPartitioningImageFilterObj =
    itk::VoronoiPartitioningImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------VoronoiPartitioningImageFilter " << VoronoiPartitioningImageFilterObj;

  itk::VoronoiSegmentationImageFilter<InputType,OutputType>::Pointer VoronoiSegmentationImageFilterObj =
    itk::VoronoiSegmentationImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------VoronoiSegmentationImageFilter " << VoronoiSegmentationImageFilterObj;

  itk::VoronoiSegmentationImageFilterBase<InputType,OutputType>::Pointer VoronoiSegmentationImageFilterBaseObj =
    itk::VoronoiSegmentationImageFilterBase<InputType,OutputType>::New();
  std:: cout << "-------------VoronoiSegmentationImageFilterBase " << VoronoiSegmentationImageFilterBaseObj;
#if 0
  itk::VoronoiSegmentationRGBImageFilter<VectorImageType,BinaryImageType>::Pointer VoronoiSegmentationRGBImageFilterObj =
    itk::VoronoiSegmentationRGBImageFilter<VectorImageType,BinaryImageType>::New();
  std:: cout << "-------------VoronoiSegmentationRGBImageFilter " << VoronoiSegmentationRGBImageFilterObj;
#endif
  itk::watershed::Boundary<double,3>::Pointer WatershedBoundaryObj =
    itk::watershed::Boundary<double,3>::New();
  std:: cout << "-------------WatershedBoundary " << WatershedBoundaryObj;

  itk::watershed::BoundaryResolver<double,3>::Pointer WatershedBoundaryResolverObj =
    itk::watershed::BoundaryResolver<double,3>::New();
  std:: cout << "-------------WatershedBoundaryResolver " << WatershedBoundaryResolverObj;

  itk::watershed::EquivalenceRelabeler<double,3>::Pointer WatershedEquivalenceRelabelerObj =
    itk::watershed::EquivalenceRelabeler<double,3>::New();
  std:: cout << "-------------WatershedEquivalenceRelabeler " << WatershedEquivalenceRelabelerObj;

  itk::watershed::EquivalencyTable::Pointer WatershedEquivalencyTableObj =
    itk::watershed::EquivalencyTable::New();
  std:: cout << "-------------WatershedEquivalencyTable " << WatershedEquivalencyTableObj;

  itk::WatershedImageFilter<InputType>::Pointer WatershedImageFilterObj =
    itk::WatershedImageFilter<InputType>::New();
  std:: cout << "-------------WatershedImageFilter " << WatershedImageFilterObj;

  itk::WatershedMiniPipelineProgressCommand::Pointer WatershedMiniPipelineProgressCommandObj =
    itk::WatershedMiniPipelineProgressCommand::New();
  std:: cout << "-------------WatershedMiniPipelineProgressCommand " << WatershedMiniPipelineProgressCommandObj;

  itk::watershed::OneWayEquivalencyTable::Pointer WatershedOneWayEquivalencyTableObj =
    itk::watershed::OneWayEquivalencyTable::New();
  std:: cout << "-------------WatershedOneWayEquivalencyTable " << WatershedOneWayEquivalencyTableObj;

  itk::watershed::Relabeler<double,3>::Pointer WatershedRelabelerObj =
    itk::watershed::Relabeler<double,3>::New();
  std:: cout << "-------------WatershedRelabeler " << WatershedRelabelerObj;

  itk::watershed::SegmentTable<double>::Pointer WatershedSegmentTableObj =
    itk::watershed::SegmentTable<double>::New();
  std:: cout << "-------------WatershedSegmentTable " << WatershedSegmentTableObj;

  itk::watershed::SegmentTree<double>::Pointer WatershedSegmentTreeObj =
    itk::watershed::SegmentTree<double>::New();
  std:: cout << "-------------WatershedSegmentTree " << WatershedSegmentTreeObj;

  itk::watershed::SegmentTreeGenerator<double>::Pointer WatershedSegmentTreeGeneratorObj =
    itk::watershed::SegmentTreeGenerator<double>::New();
  std:: cout << "-------------WatershedSegmentTreeGenerator " << WatershedSegmentTreeGeneratorObj;

  itk::watershed::Segmenter<InputType>::Pointer WatershedSegmenterObj =
    itk::watershed::Segmenter<InputType>::New();
  std:: cout << "-------------WatershedSegmenter " << WatershedSegmenterObj;

  return 0;

}

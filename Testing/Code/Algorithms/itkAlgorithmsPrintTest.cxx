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
#include "itkGibbsPriorFilter.h"
#include "itkGradientVectorFlowImageFilter.h"
#include "itkHistogramMatchingImageFilter.h"
#include "itkHybridFilter.h"
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

int itkAlgorithmsPrintTest(int , char* [])
{
  typedef itk::Image<float,2> InputType;
  typedef itk::Image<unsigned char,2> CharType;
  typedef itk::Image<float,2> OutputType;
  typedef itk::Mesh<double>  MeshType;
  typedef itk::Point<double,2> Mesh2DPixelType;
  typedef itk::Mesh<double>  Mesh2DType;
  typedef itk::Vector<float,2> VectorType;
  typedef itk::Image<VectorType, 2> VectorImageType;

  itk::AntiAliasBinaryImageFilter<InputType,OutputType>::Pointer AntiAliasBinaryImageFilterObj =
    itk::AntiAliasBinaryImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------AntiAliasBinaryImageFilter " << AntiAliasBinaryImageFilterObj;

  itk::BalloonForceFilter<Mesh2DType,Mesh2DType>::Pointer BalloonForceFilterObj =
    itk::BalloonForceFilter<Mesh2DType,Mesh2DType>::New();
  std:: cout << "-------------BalloonForceFilter " << BalloonForceFilterObj;

  itk::BalloonForce3DFilter<Mesh2DType,Mesh2DType>::Pointer BalloonForce3DFilterObj =
    itk::BalloonForce3DFilter<Mesh2DType,Mesh2DType>::New();
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

#if 0
  itk::CurvatureFlowImageFilter<InputType,OutputType>::Pointer CurvatureFlowImageFilterObj =
    itk::CurvatureFlowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------CurvatureFlowImageFilter " << CurvatureFlowImageFilterObj;

  itk::DeformableMesh3DFilter<InputType,OutputType>::Pointer DeformableMesh3DFilterObj =
    itk::DeformableMesh3DFilter<InputType,OutputType>::New();
  std:: cout << "-------------DeformableMesh3DFilter " << DeformableMesh3DFilterObj;

  itk::DeformableMeshFilter<InputType,OutputType>::Pointer DeformableMeshFilterObj =
    itk::DeformableMeshFilter<InputType,OutputType>::New();
  std:: cout << "-------------DeformableMeshFilter " << DeformableMeshFilterObj;

  itk::DemonsRegistrationFilter<InputType,OutputType>::Pointer DemonsRegistrationFilterObj =
    itk::DemonsRegistrationFilter<InputType,OutputType>::New();
  std:: cout << "-------------DemonsRegistrationFilter " << DemonsRegistrationFilterObj;

  itk::DemonsRegistrationFunction<InputType,OutputType>::Pointer DemonsRegistrationFunctionObj =
    itk::DemonsRegistrationFunction<InputType,OutputType>::New();
  std:: cout << "-------------DemonsRegistrationFunction " << DemonsRegistrationFunctionObj;

  itk::ExtensionVelocitiesImageFilter<InputType,OutputType>::Pointer ExtensionVelocitiesImageFilterObj =
    itk::ExtensionVelocitiesImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------ExtensionVelocitiesImageFilter " << ExtensionVelocitiesImageFilterObj;

  itk::FEMRegistrationFilter<InputType,OutputType>::Pointer FEMRegistrationFilterObj =
    itk::FEMRegistrationFilter<InputType,OutputType>::New();
  std:: cout << "-------------FEMRegistrationFilter " << FEMRegistrationFilterObj;

  itk::FastMarchingExtensionImageFilter<InputType,OutputType>::Pointer FastMarchingExtensionImageFilterObj =
    itk::FastMarchingExtensionImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------FastMarchingExtensionImageFilter " << FastMarchingExtensionImageFilterObj;

  itk::FastMarchingImageFilter<InputType,OutputType>::Pointer FastMarchingImageFilterObj =
    itk::FastMarchingImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------FastMarchingImageFilter " << FastMarchingImageFilterObj;

  itk::GeodesicActiveContourLevelSetFunction<InputType,OutputType>::Pointer GeodesicActiveContourLevelSetFunctionObj =
    itk::GeodesicActiveContourLevelSetFunction<InputType,OutputType>::New();
  std:: cout << "-------------GeodesicActiveContourLevelSetFunction " << GeodesicActiveContourLevelSetFunctionObj;

  itk::GeodesicActiveContourLevelSetImageFilter<InputType,OutputType>::Pointer GeodesicActiveContourLevelSetImageFilterObj =
    itk::GeodesicActiveContourLevelSetImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------GeodesicActiveContourLevelSetImageFilter " << GeodesicActiveContourLevelSetImageFilterObj;

  itk::GibbsPriorFilter<InputType,OutputType>::Pointer GibbsPriorFilterObj =
    itk::GibbsPriorFilter<InputType,OutputType>::New();
  std:: cout << "-------------GibbsPriorFilter " << GibbsPriorFilterObj;

  itk::GradientVectorFlowImageFilter<InputType,OutputType>::Pointer GradientVectorFlowImageFilterObj =
    itk::GradientVectorFlowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------GradientVectorFlowImageFilter " << GradientVectorFlowImageFilterObj;

  itk::HistogramMatchingImageFilter<InputType,OutputType>::Pointer HistogramMatchingImageFilterObj =
    itk::HistogramMatchingImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------HistogramMatchingImageFilter " << HistogramMatchingImageFilterObj;

  itk::HybridFilter<InputType,OutputType>::Pointer HybridFilterObj =
    itk::HybridFilter<InputType,OutputType>::New();
  std:: cout << "-------------HybridFilter " << HybridFilterObj;

  itk::ImageClassifierBase<InputType,OutputType>::Pointer ImageClassifierBaseObj =
    itk::ImageClassifierBase<InputType,OutputType>::New();
  std:: cout << "-------------ImageClassifierBase " << ImageClassifierBaseObj;

  itk::ImageGaussianModelEstimator<InputType,OutputType>::Pointer ImageGaussianModelEstimatorObj =
    itk::ImageGaussianModelEstimator<InputType,OutputType>::New();
  std:: cout << "-------------ImageGaussianModelEstimator " << ImageGaussianModelEstimatorObj;

  itk::ImageKmeansModelEstimator<InputType,OutputType>::Pointer ImageKmeansModelEstimatorObj =
    itk::ImageKmeansModelEstimator<InputType,OutputType>::New();
  std:: cout << "-------------ImageKmeansModelEstimator " << ImageKmeansModelEstimatorObj;

  itk::ImageRegistrationMethod<InputType,OutputType>::Pointer ImageRegistrationMethodObj =
    itk::ImageRegistrationMethod<InputType,OutputType>::New();
  std:: cout << "-------------ImageRegistrationMethod " << ImageRegistrationMethodObj;

  itk::ImageToSpatialObjectMetric<InputType,OutputType>::Pointer ImageToSpatialObjectMetricObj =
    itk::ImageToSpatialObjectMetric<InputType,OutputType>::New();
  std:: cout << "-------------ImageToSpatialObjectMetric " << ImageToSpatialObjectMetricObj;

  itk::ImageToSpatialObjectRegistrationMethod<InputType,OutputType>::Pointer ImageToSpatialObjectRegistrationMethodObj =
    itk::ImageToSpatialObjectRegistrationMethod<InputType,OutputType>::New();
  std:: cout << "-------------ImageToSpatialObjectRegistrationMethod " << ImageToSpatialObjectRegistrationMethodObj;

  itk::KLMRegionGrowImageFilter<InputType,OutputType>::Pointer KLMRegionGrowImageFilterObj =
    itk::KLMRegionGrowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------KLMRegionGrowImageFilter " << KLMRegionGrowImageFilterObj;

  itk::LaplacianSegmentationLevelSetFunction<InputType,OutputType>::Pointer LaplacianSegmentationLevelSetFunctionObj =
    itk::LaplacianSegmentationLevelSetFunction<InputType,OutputType>::New();
  std:: cout << "-------------LaplacianSegmentationLevelSetFunction " << LaplacianSegmentationLevelSetFunctionObj;

  itk::LaplacianSegmentationLevelSetImageFilter<InputType,OutputType>::Pointer LaplacianSegmentationLevelSetImageFilterObj =
    itk::LaplacianSegmentationLevelSetImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------LaplacianSegmentationLevelSetImageFilter " << LaplacianSegmentationLevelSetImageFilterObj;

  itk::LevelSetNeighborhoodExtractor<InputType,OutputType>::Pointer LevelSetNeighborhoodExtractorObj =
    itk::LevelSetNeighborhoodExtractor<InputType,OutputType>::New();
  std:: cout << "-------------LevelSetNeighborhoodExtractor " << LevelSetNeighborhoodExtractorObj;

  itk::LevelSetVelocityNeighborhoodExtractor<InputType,OutputType>::Pointer LevelSetVelocityNeighborhoodExtractorObj =
    itk::LevelSetVelocityNeighborhoodExtractor<InputType,OutputType>::New();
  std:: cout << "-------------LevelSetVelocityNeighborhoodExtractor " << LevelSetVelocityNeighborhoodExtractorObj;

  itk::MRASlabIdentifier<InputType,OutputType>::Pointer MRASlabIdentifierObj =
    itk::MRASlabIdentifier<InputType,OutputType>::New();
  std:: cout << "-------------MRASlabIdentifier " << MRASlabIdentifierObj;

  itk::MRFImageFilter<InputType,OutputType>::Pointer MRFImageFilterObj =
    itk::MRFImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------MRFImageFilter " << MRFImageFilterObj;

  itk::MRIBiasFieldCorrectionFilter<InputType,OutputType>::Pointer MRIBiasFieldCorrectionFilterObj =
    itk::MRIBiasFieldCorrectionFilter<InputType,OutputType>::New();
  std:: cout << "-------------MRIBiasFieldCorrectionFilter " << MRIBiasFieldCorrectionFilterObj;

  itk::MattesMutualInformationImageToImageMetric<InputType,OutputType>::Pointer MattesMutualInformationImageToImageMetricObj =
    itk::MattesMutualInformationImageToImageMetric<InputType,OutputType>::New();
  std:: cout << "-------------MattesMutualInformationImageToImageMetric " << MattesMutualInformationImageToImageMetricObj;

  itk::MeanSquaresImageToImageMetric<InputType,OutputType>::Pointer MeanSquaresImageToImageMetricObj =
    itk::MeanSquaresImageToImageMetric<InputType,OutputType>::New();
  std:: cout << "-------------MeanSquaresImageToImageMetric " << MeanSquaresImageToImageMetricObj;

  itk::MeanSquaresPointSetToImageMetric<InputType,OutputType>::Pointer MeanSquaresPointSetToImageMetricObj =
    itk::MeanSquaresPointSetToImageMetric<InputType,OutputType>::New();
  std:: cout << "-------------MeanSquaresPointSetToImageMetric " << MeanSquaresPointSetToImageMetricObj;

  itk::MinMaxCurvatureFlowFunction<InputType,OutputType>::Pointer MinMaxCurvatureFlowFunctionObj =
    itk::MinMaxCurvatureFlowFunction<InputType,OutputType>::New();
  std:: cout << "-------------MinMaxCurvatureFlowFunction " << MinMaxCurvatureFlowFunctionObj;

  itk::MinMaxCurvatureFlowImageFilter<InputType,OutputType>::Pointer MinMaxCurvatureFlowImageFilterObj =
    itk::MinMaxCurvatureFlowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------MinMaxCurvatureFlowImageFilter " << MinMaxCurvatureFlowImageFilterObj;

  itk::MultiResolutionImageRegistrationMethod<InputType,OutputType>::Pointer MultiResolutionImageRegistrationMethodObj =
    itk::MultiResolutionImageRegistrationMethod<InputType,OutputType>::New();
  std:: cout << "-------------MultiResolutionImageRegistrationMethod " << MultiResolutionImageRegistrationMethodObj;

  itk::MultiResolutionPDEDeformableRegistration<InputType,OutputType>::Pointer MultiResolutionPDEDeformableRegistrationObj =
    itk::MultiResolutionPDEDeformableRegistration<InputType,OutputType>::New();
  std:: cout << "-------------MultiResolutionPDEDeformableRegistration " << MultiResolutionPDEDeformableRegistrationObj;

  itk::MultiResolutionPyramidImageFilter<InputType,OutputType>::Pointer MultiResolutionPyramidImageFilterObj =
    itk::MultiResolutionPyramidImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------MultiResolutionPyramidImageFilter " << MultiResolutionPyramidImageFilterObj;

  itk::MutualInformationImageToImageMetric<InputType,OutputType>::Pointer MutualInformationImageToImageMetricObj =
    itk::MutualInformationImageToImageMetric<InputType,OutputType>::New();
  std:: cout << "-------------MutualInformationImageToImageMetric " << MutualInformationImageToImageMetricObj;

  itk::NormalizedCorrelationImageToImageMetric<InputType,OutputType>::Pointer NormalizedCorrelationImageToImageMetricObj =
    itk::NormalizedCorrelationImageToImageMetric<InputType,OutputType>::New();
  std:: cout << "-------------NormalizedCorrelationImageToImageMetric " << NormalizedCorrelationImageToImageMetricObj;

  itk::NormalizedCorrelationPointSetToImageMetric<InputType,OutputType>::Pointer NormalizedCorrelationPointSetToImageMetricObj =
    itk::NormalizedCorrelationPointSetToImageMetric<InputType,OutputType>::New();
  std:: cout << "-------------NormalizedCorrelationPointSetToImageMetric " << NormalizedCorrelationPointSetToImageMetricObj;

  itk::OtsuThresholdImageCalculator<InputType,OutputType>::Pointer OtsuThresholdImageCalculatorObj =
    itk::OtsuThresholdImageCalculator<InputType,OutputType>::New();
  std:: cout << "-------------OtsuThresholdImageCalculator " << OtsuThresholdImageCalculatorObj;

  itk::PDEDeformableRegistrationFilter<InputType,OutputType>::Pointer PDEDeformableRegistrationFilterObj =
    itk::PDEDeformableRegistrationFilter<InputType,OutputType>::New();
  std:: cout << "-------------PDEDeformableRegistrationFilter " << PDEDeformableRegistrationFilterObj;

  itk::PatternIntensityImageToImageMetric<InputType,OutputType>::Pointer PatternIntensityImageToImageMetricObj =
    itk::PatternIntensityImageToImageMetric<InputType,OutputType>::New();
  std:: cout << "-------------PatternIntensityImageToImageMetric " << PatternIntensityImageToImageMetricObj;

#if 0
  itk::PatternIntensityPointSetToImageMetric<InputType,OutputType>::Pointer PatternIntensityPointSetToImageMetricObj =
    itk::PatternIntensityPointSetToImageMetric<InputType,OutputType>::New();
  std:: cout << "-------------PatternIntensityPointSetToImageMetric " << PatternIntensityPointSetToImageMetricObj;
#endif
  itk::RGBGibbsPriorFilter<InputType,OutputType>::Pointer RGBGibbsPriorFilterObj =
    itk::RGBGibbsPriorFilter<InputType,OutputType>::New();
  std:: cout << "-------------RGBGibbsPriorFilter " << RGBGibbsPriorFilterObj;

  itk::RecursiveMultiResolutionPyramidImageFilter<InputType,OutputType>::Pointer RecursiveMultiResolutionPyramidImageFilterObj =
    itk::RecursiveMultiResolutionPyramidImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------RecursiveMultiResolutionPyramidImageFilter " << RecursiveMultiResolutionPyramidImageFilterObj;

  itk::RegionGrowImageFilter<InputType,OutputType>::Pointer RegionGrowImageFilterObj =
    itk::RegionGrowImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------RegionGrowImageFilter " << RegionGrowImageFilterObj;

  itk::RegistrationMethod<InputType,OutputType>::Pointer RegistrationMethodObj =
    itk::RegistrationMethod<InputType,OutputType>::New();
  std:: cout << "-------------RegistrationMethod " << RegistrationMethodObj;

  itk::ReinitializeLevelSetImageFilter<InputType,OutputType>::Pointer ReinitializeLevelSetImageFilterObj =
    itk::ReinitializeLevelSetImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------ReinitializeLevelSetImageFilter " << ReinitializeLevelSetImageFilterObj;

  itk::ShapeDetectionLevelSetFunction<InputType,OutputType>::Pointer ShapeDetectionLevelSetFunctionObj =
    itk::ShapeDetectionLevelSetFunction<InputType,OutputType>::New();
  std:: cout << "-------------ShapeDetectionLevelSetFunction " << ShapeDetectionLevelSetFunctionObj;

  itk::ShapeDetectionLevelSetImageFilter<InputType,OutputType>::Pointer ShapeDetectionLevelSetImageFilterObj =
    itk::ShapeDetectionLevelSetImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------ShapeDetectionLevelSetImageFilter " << ShapeDetectionLevelSetImageFilterObj;

  itk::SimpleFuzzyConnectednessImageFilterBase<InputType,OutputType>::Pointer SimpleFuzzyConnectednessImageFilterBaseObj =
    itk::SimpleFuzzyConnectednessImageFilterBase<InputType,OutputType>::New();
  std:: cout << "-------------SimpleFuzzyConnectednessImageFilterBase " << SimpleFuzzyConnectednessImageFilterBaseObj;

  itk::SimpleFuzzyConnectednessRGBImageFilter<InputType,OutputType>::Pointer SimpleFuzzyConnectednessRGBImageFilterObj =
    itk::SimpleFuzzyConnectednessRGBImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------SimpleFuzzyConnectednessRGBImageFilter " << SimpleFuzzyConnectednessRGBImageFilterObj;

  itk::SimpleFuzzyConnectednessScalarImageFilter<InputType,OutputType>::Pointer SimpleFuzzyConnectednessScalarImageFilterObj =
    itk::SimpleFuzzyConnectednessScalarImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------SimpleFuzzyConnectednessScalarImageFilter " << SimpleFuzzyConnectednessScalarImageFilterObj;

  itk::SphereMeshSource<InputType,OutputType>::Pointer SphereMeshSourceObj =
    itk::SphereMeshSource<InputType,OutputType>::New();
  std:: cout << "-------------SphereMeshSource " << SphereMeshSourceObj;

  itk::ThresholdSegmentationLevelSetFunction<InputType,OutputType>::Pointer ThresholdSegmentationLevelSetFunctionObj =
    itk::ThresholdSegmentationLevelSetFunction<InputType,OutputType>::New();
  std:: cout << "-------------ThresholdSegmentationLevelSetFunction " << ThresholdSegmentationLevelSetFunctionObj;

  itk::ThresholdSegmentationLevelSetImageFilter<InputType,OutputType>::Pointer ThresholdSegmentationLevelSetImageFilterObj =
    itk::ThresholdSegmentationLevelSetImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------ThresholdSegmentationLevelSetImageFilter " << ThresholdSegmentationLevelSetImageFilterObj;

  itk::VectorFuzzyConnectednessImageFilter<InputType,OutputType>::Pointer VectorFuzzyConnectednessImageFilterObj =
    itk::VectorFuzzyConnectednessImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------VectorFuzzyConnectednessImageFilter " << VectorFuzzyConnectednessImageFilterObj;

  itk::VoronoiDiagram2D<InputType,OutputType>::Pointer VoronoiDiagram2DObj =
    itk::VoronoiDiagram2D<InputType,OutputType>::New();
  std:: cout << "-------------VoronoiDiagram2D " << VoronoiDiagram2DObj;

  itk::VoronoiDiagram2DGenerator<InputType,OutputType>::Pointer VoronoiDiagram2DGeneratorObj =
    itk::VoronoiDiagram2DGenerator<InputType,OutputType>::New();
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

  itk::VoronoiSegmentationRGBImageFilter<InputType,OutputType>::Pointer VoronoiSegmentationRGBImageFilterObj =
    itk::VoronoiSegmentationRGBImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------VoronoiSegmentationRGBImageFilter " << VoronoiSegmentationRGBImageFilterObj;

  itk::WatershedBoundary<InputType,OutputType>::Pointer WatershedBoundaryObj =
    itk::WatershedBoundary<InputType,OutputType>::New();
  std:: cout << "-------------WatershedBoundary " << WatershedBoundaryObj;

  itk::WatershedBoundaryResolver<InputType,OutputType>::Pointer WatershedBoundaryResolverObj =
    itk::WatershedBoundaryResolver<InputType,OutputType>::New();
  std:: cout << "-------------WatershedBoundaryResolver " << WatershedBoundaryResolverObj;

  itk::WatershedEquivalenceRelabeler<InputType,OutputType>::Pointer WatershedEquivalenceRelabelerObj =
    itk::WatershedEquivalenceRelabeler<InputType,OutputType>::New();
  std:: cout << "-------------WatershedEquivalenceRelabeler " << WatershedEquivalenceRelabelerObj;

  itk::WatershedEquivalencyTable<InputType,OutputType>::Pointer WatershedEquivalencyTableObj =
    itk::WatershedEquivalencyTable<InputType,OutputType>::New();
  std:: cout << "-------------WatershedEquivalencyTable " << WatershedEquivalencyTableObj;

  itk::WatershedImageFilter<InputType,OutputType>::Pointer WatershedImageFilterObj =
    itk::WatershedImageFilter<InputType,OutputType>::New();
  std:: cout << "-------------WatershedImageFilter " << WatershedImageFilterObj;

  itk::WatershedMiniPipelineProgressCommand<InputType,OutputType>::Pointer WatershedMiniPipelineProgressCommandObj =
    itk::WatershedMiniPipelineProgressCommand<InputType,OutputType>::New();
  std:: cout << "-------------WatershedMiniPipelineProgressCommand " << WatershedMiniPipelineProgressCommandObj;

  itk::WatershedOneWayEquivalencyTable<InputType,OutputType>::Pointer WatershedOneWayEquivalencyTableObj =
    itk::WatershedOneWayEquivalencyTable<InputType,OutputType>::New();
  std:: cout << "-------------WatershedOneWayEquivalencyTable " << WatershedOneWayEquivalencyTableObj;

  itk::WatershedRelabeler<InputType,OutputType>::Pointer WatershedRelabelerObj =
    itk::WatershedRelabeler<InputType,OutputType>::New();
  std:: cout << "-------------WatershedRelabeler " << WatershedRelabelerObj;

  itk::WatershedSegmentTable<InputType,OutputType>::Pointer WatershedSegmentTableObj =
    itk::WatershedSegmentTable<InputType,OutputType>::New();
  std:: cout << "-------------WatershedSegmentTable " << WatershedSegmentTableObj;

  itk::WatershedSegmentTree<InputType,OutputType>::Pointer WatershedSegmentTreeObj =
    itk::WatershedSegmentTree<InputType,OutputType>::New();
  std:: cout << "-------------WatershedSegmentTree " << WatershedSegmentTreeObj;

  itk::WatershedSegmentTreeGenerator<InputType,OutputType>::Pointer WatershedSegmentTreeGeneratorObj =
    itk::WatershedSegmentTreeGenerator<InputType,OutputType>::New();
  std:: cout << "-------------WatershedSegmentTreeGenerator " << WatershedSegmentTreeGeneratorObj;

  itk::WatershedSegmenter<InputType,OutputType>::Pointer WatershedSegmenterObj =
    itk::WatershedSegmenter<InputType,OutputType>::New();
  std:: cout << "-------------WatershedSegmenter " << WatershedSegmenterObj;
#endif
  return 0;

}

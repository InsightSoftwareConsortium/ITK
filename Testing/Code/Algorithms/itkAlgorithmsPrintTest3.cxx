/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAlgorithmsPrintTest3.cxx
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
#include "itkWatershedImageFilter.h"
#include "itkWatershedMiniPipelineProgressCommand.h"
#include "itkWatershedRelabeler.h"
#include "itkWatershedSegmentTable.h"
#include "itkWatershedSegmentTree.h"
#include "itkWatershedSegmentTreeGenerator.h"
#include "itkWatershedSegmenter.h"

#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkMahalanobisDistanceMembershipFunction.h"
#include "itkGroupSpatialObject.h"
#include "itkRGBPixel.h"

int itkAlgorithmsPrintTest3(int , char* [])
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

  itk::WatershedImageFilter<InputType>::Pointer WatershedImageFilterObj =
    itk::WatershedImageFilter<InputType>::New();
  std:: cout << "-------------WatershedImageFilter " << WatershedImageFilterObj;

  itk::WatershedMiniPipelineProgressCommand::Pointer WatershedMiniPipelineProgressCommandObj =
    itk::WatershedMiniPipelineProgressCommand::New();
  std:: cout << "-------------WatershedMiniPipelineProgressCommand " << WatershedMiniPipelineProgressCommandObj;

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

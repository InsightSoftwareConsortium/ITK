/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBasicFiltersPrintTest2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImage.h"
#include "itkVector.h"
#include "itkPoint.h"
#include "itkMesh.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkAcosImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkAsinImageFilter.h"
#include "itkAtan2ImageFilter.h"
#include "itkAtanImageFilter.h"
#include "itkBSplineCenteredResampleImageFilterBase.h"
#include "itkBSplineDecompositionImageFilter.h"
#include "itkBSplineDownsampleImageFilter.h"
#include "itkBSplineInterpolateImageFunction.h"
#include "itkBSplineResampleImageFunction.h"
#include "itkBSplineUpsampleImageFilter.h"
#include "itkBilateralImageFilter.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryMagnitudeImageFilter.h"
#include "itkBinaryMedianImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkBinomialBlurImageFilter.h"
#include "itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilter.h"
#include "itkBloxBoundaryPointToCoreAtomImageFilter.h"
#include "itkCannyEdgeDetectionImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkComposeRGBImageFilter.h"
#include "itkConfidenceConnectedImageFilter.h"
#include "itkConnectedThresholdImageFilter.h"
#include "itkConstantPadImageFilter.h"
#include "itkCosImageFilter.h"
#include "itkCropImageFilter.h"
#include "itkCurvatureAnisotropicDiffusionImageFilter.h"
#include "itkCurvatureNDAnisotropicDiffusionFunction.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "itkDerivativeImageFilter.h"
#include "itkDifferenceOfGaussiansGradientImageFilter.h"
#include "itkDilateObjectMorphologyImageFilter.h"
#include "itkDirectedHausdorffDistanceImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkDivideImageFilter.h"
#include "itkEdgePotentialImageFilter.h"
#include "itkEigenAnalysis2DImageFilter.h"
#include "itkErodeObjectMorphologyImageFilter.h"
#include "itkExpImageFilter.h"
#include "itkExpNegativeImageFilter.h"
#include "itkExpandImageFilter.h"
#include "itkExtractImageFilter.h"
#include "itkFlipImageFilter.h"
#include "itkGaussianImageSource.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkGradientImageToBloxBoundaryPointImageFilter.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkGradientNDAnisotropicDiffusionFunction.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGradientToMagnitudeImageFilter.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkGrayscaleErodeImageFilter.h"
#include "itkGrayscaleFunctionDilateImageFilter.h"
#include "itkGrayscaleFunctionErodeImageFilter.h"
#include "itkHardConnectedComponentImageFilter.h"
#include "itkHausdorffDistanceImageFilter.h"
#include "itkImageToParametricSpaceFilter.h"
#include "itkImportImageFilter.h"
#include "itkIntensityWindowingImageFilter.h"
#include "itkInteriorExteriorMeshFilter.h"
#include "itkInterpolateImageFilter.h"
#include "itkInterpolateImagePointsFilter.h"
#include "itkIsolatedConnectedImageFilter.h"
#include "itkJoinImageFilter.h"
#include "itkLaplacianImageFilter.h"
#include "itkLaplacianRecursiveGaussianImageFilter.h"
#include "itkLog10ImageFilter.h"
#include "itkLogImageFilter.h"
#include "itkMaskImageFilter.h"
#include "itkMaximumImageFilter.h"
#include "itkMeanImageFilter.h"
#include "itkMedianImageFilter.h"
#include "itkMinimumImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkMinimumMaximumImageFilter.h"
#include "itkMirrorPadImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkNaryAddImageFilter.h"
#include "itkNaryFunctorImageFilter.h"
#include "itkNeighborhoodConnectedImageFilter.h"
#include "itkNeighborhoodOperatorImageFilter.h"
#include "itkNoiseImageFilter.h"
#include "itkNonThreadedShrinkImageFilter.h"
#include "itkNormalizeImageFilter.h"
#include "itkPadImageFilter.h"
#include "itkParametricSpaceToImageSpaceMeshFilter.h"
#include "itkPasteImageFilter.h"
#include "itkPermuteAxesImageFilter.h"
#include "itkPlaheImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkReflectImageFilter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkResampleImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkScalarToArrayCastImageFilter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkShiftScaleInPlaceImageFilter.h"
#include "itkShrinkImageFilter.h"
#include "itkSigmoidImageFilter.h"
#include "itkSimilarityIndexImageFilter.h"
#include "itkSinImageFilter.h"
#include "itkSmoothingRecursiveGaussianImageFilter.h"
#include "itkSobelEdgeDetectionImageFilter.h"
#include "itkSparseFieldLayer.h"
#include "itkSparseFieldLevelSetImageFilter.h"
#include "itkSpatialFunctionImageEvaluatorFilter.h"
#include "itkSpatialObjectToImageFilter.h"
#include "itkSqrtImageFilter.h"
#include "itkSquareImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"
#include "itkStatisticsImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkTanImageFilter.h"
#include "itkTernaryAddImageFilter.h"
#include "itkTernaryFunctorImageFilter.h"
#include "itkTernaryMagnitudeImageFilter.h"
#include "itkTernaryMagnitudeSquaredImageFilter.h"
#include "itkThresholdImageFilter.h"
#include "itkTobogganImageFilter.h"
#include "itkTransformMeshFilter.h"
#include "itkTwoOutputExampleImageFilter.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkVTKImageExport.h"
#include "itkVTKImageImport.h"
#include "itkVectorCastImageFilter.h"
#include "itkVectorCurvatureAnisotropicDiffusionImageFilter.h"
#include "itkVectorCurvatureNDAnisotropicDiffusionFunction.h"
#include "itkVectorExpandImageFilter.h"
#include "itkVectorGradientAnisotropicDiffusionImageFilter.h"
#include "itkVectorGradientMagnitudeImageFilter.h"
#include "itkVectorGradientNDAnisotropicDiffusionFunction.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"
#include "itkWarpImageFilter.h"
#include "itkWrapPadImageFilter.h"
#include "itkZeroCrossingBasedEdgeDetectionImageFilter.h"
#include "itkZeroCrossingImageFilter.h"

#include <itkSphereSpatialFunction.h>
#include "itkGaussianSpatialFunction.h"


struct node_type
{
  unsigned int value;
  node_type *Next;
  node_type *Previous;
};


int itkBasicFiltersPrintTest2(int , char* [])
{
  typedef itk::Image<float,2> InputType;
  typedef itk::Image<float,2> OutputType;
  typedef itk::Image<unsigned char,2> CharType;
  typedef itk::Image<unsigned char,3> CharType3D;
  
  typedef itk::Point<float,2> MeshPixelType;
  typedef itk::Mesh<MeshPixelType>  MeshType;

  typedef itk::Vector<float,2> VectorType;
  typedef itk::Image<VectorType,2> VectorImageType;

  typedef itk::CovariantVector<float,2> CovariantVectorType;
  typedef itk::Image<CovariantVectorType,2> CovariantVectorImageType;

  //typedef itk::Neighborhood<unsigned short,2> KernelType;
  typedef itk::BinaryBallStructuringElement<unsigned short,2> KernelType;

  // Used for MaskImageFilter
  typedef itk::Image<unsigned short,2> MaskImageType;

  // Used for TransformMeshFilter
  typedef itk::AffineTransform<float,3> AffineTransformType;

  // Used for InteriorExteriorMeshFilter
  typedef itk::Point<float, 3> PointType;
  typedef itk::SphereSpatialFunction< MeshType::PointDimension,
    MeshType::PointType > SphereSpatialFunctionType;

  // Used for SpatialFunctionImageEvaluator
  typedef itk::GaussianSpatialFunction<char,2> GaussianSpatialFunctionType;

  // Used for GradientImageToBloxBoundaryPointImageFilter
  typedef itk::DifferenceOfGaussiansGradientImageFilter<CharType3D,
    double> DOGFilterType;


  itk::MaskImageFilter<InputType,MaskImageType,OutputType>::Pointer MaskImageFilterObj =
    itk::MaskImageFilter<InputType,MaskImageType,OutputType>::New();
  std::cout << "-------------MaskImageFilter" << MaskImageFilterObj;

  itk::MaximumImageFilter<InputType,InputType,OutputType>::Pointer MaximumImageFilterObj =
    itk::MaximumImageFilter<InputType,InputType,OutputType>::New();
  std::cout << "-------------MaximumImageFilter" << MaximumImageFilterObj;

  itk::MeanImageFilter<InputType,OutputType>::Pointer MeanImageFilterObj =
    itk::MeanImageFilter<InputType,OutputType>::New();
  std::cout << "-------------MeanImageFilter" << MeanImageFilterObj;

  itk::MedianImageFilter<InputType,OutputType>::Pointer MedianImageFilterObj =
    itk::MedianImageFilter<InputType,OutputType>::New();
  std::cout << "-------------MedianImageFilter" << MedianImageFilterObj;

  itk::MinimumImageFilter<InputType,InputType,OutputType>::Pointer MinimumImageFilterObj =
    itk::MinimumImageFilter<InputType,InputType,OutputType>::New();
  std::cout << "-------------MinimumImageFilter" << MinimumImageFilterObj;

  itk::MinimumMaximumImageCalculator<InputType>::Pointer MinimumMaximumImageCalculatorObj =
    itk::MinimumMaximumImageCalculator<InputType>::New();
  std::cout << "-------------MinimumMaximumImageCalculator" << MinimumMaximumImageCalculatorObj;

  itk::MinimumMaximumImageFilter<InputType>::Pointer MinimumMaximumImageFilterObj =
    itk::MinimumMaximumImageFilter<InputType>::New();
  std::cout << "-------------MinimumMaximumImageFilter" << MinimumMaximumImageFilterObj;

  itk::MirrorPadImageFilter<InputType,OutputType>::Pointer MirrorPadImageFilterObj =
    itk::MirrorPadImageFilter<InputType,OutputType>::New();
  std::cout << "-------------MirrorPadImageFilter" << MirrorPadImageFilterObj;

  itk::MultiplyImageFilter<InputType,InputType,OutputType>::Pointer MultiplyImageFilterObj =
    itk::MultiplyImageFilter<InputType,InputType,OutputType>::New();
  std::cout << "-------------MultiplyImageFilter" << MultiplyImageFilterObj;

  itk::NaryAddImageFilter<InputType,OutputType>::Pointer NaryAddImageFilterObj =
    itk::NaryAddImageFilter<InputType,OutputType>::New();
  std::cout << "-------------NaryAddImageFilter" << NaryAddImageFilterObj;
#if 0
  itk::NaryFunctorImageFilter<InputType,OutputType,OutputType>::Pointer NaryFunctorImageFilterObj =
    itk::NaryFunctorImageFilter<InputType,OutputType,OutputType>::New();
  std::cout << "-------------NaryFunctorImageFilter" << NaryFunctorImageFilterObj;
#endif
  itk::NeighborhoodConnectedImageFilter<InputType,OutputType>::Pointer NeighborhoodConnectedImageFilterObj =
    itk::NeighborhoodConnectedImageFilter<InputType,OutputType>::New();
  std::cout << "-------------NeighborhoodConnectedImageFilter" << NeighborhoodConnectedImageFilterObj;

  itk::NeighborhoodOperatorImageFilter<InputType,OutputType>::Pointer NeighborhoodOperatorImageFilterObj =
    itk::NeighborhoodOperatorImageFilter<InputType,OutputType>::New();
  std::cout << "-------------NeighborhoodOperatorImageFilter" << NeighborhoodOperatorImageFilterObj;

  itk::NoiseImageFilter<InputType,OutputType>::Pointer NoiseImageFilterObj =
    itk::NoiseImageFilter<InputType,OutputType>::New();
  std::cout << "-------------NoiseImageFilter" << NoiseImageFilterObj;
#if 0
  itk::NonThreadedShrinkImageFilter<InputType,OutputType>::Pointer NonThreadedShrinkImageFilterObj =
    itk::NonThreadedShrinkImageFilter<InputType,OutputType>::New();
  std::cout << "-------------NonThreadedShrinkImageFilter" << NonThreadedShrinkImageFilterObj;
#endif
  itk::NormalizeImageFilter<InputType,OutputType>::Pointer NormalizeImageFilterObj =
    itk::NormalizeImageFilter<InputType,OutputType>::New();
  std::cout << "-------------NormalizeImageFilter" << NormalizeImageFilterObj;

  itk::PadImageFilter<InputType,OutputType>::Pointer PadImageFilterObj =
    itk::PadImageFilter<InputType,OutputType>::New();
  std::cout << "-------------PadImageFilter" << PadImageFilterObj;
#if 0
  itk::ParametricSpaceToImageSpaceMeshFilter<MeshType,MeshType>::Pointer ParametricSpaceToImageSpaceMeshFilterObj =
    itk::ParametricSpaceToImageSpaceMeshFilter<MeshType,MeshType>::New();
  std::cout << "-------------ParametricSpaceToImageSpaceMeshFilter" << ParametricSpaceToImageSpaceMeshFilterObj;
#endif
  itk::PasteImageFilter<InputType>::Pointer PasteImageFilterObj =
    itk::PasteImageFilter<InputType>::New();
  std::cout << "-------------PasteImageFilter" << PasteImageFilterObj;

  itk::PermuteAxesImageFilter<InputType>::Pointer PermuteAxesImageFilterObj =
    itk::PermuteAxesImageFilter<InputType>::New();
  std::cout << "-------------PermuteAxesImageFilter" << PermuteAxesImageFilterObj;

  itk::PlaheImageFilter<InputType>::Pointer PlaheImageFilterObj =
    itk::PlaheImageFilter<InputType>::New();
  std::cout << "-------------PlaheImageFilter" << PlaheImageFilterObj;

  itk::RandomImageSource<OutputType>::Pointer RandomImageSourceObj =
    itk::RandomImageSource<OutputType>::New();
  std::cout << "-------------RandomImageSource" << RandomImageSourceObj;

  itk::RecursiveGaussianImageFilter<InputType,OutputType>::Pointer RecursiveGaussianImageFilterObj =
    itk::RecursiveGaussianImageFilter<InputType,OutputType>::New();
  std::cout << "-------------RecursiveGaussianImageFilter" << RecursiveGaussianImageFilterObj;

  itk::ReflectImageFilter<InputType,OutputType>::Pointer ReflectImageFilterObj =
    itk::ReflectImageFilter<InputType,OutputType>::New();
  std::cout << "-------------ReflectImageFilter" << ReflectImageFilterObj;

  itk::RegionOfInterestImageFilter<InputType,OutputType>::Pointer RegionOfInterestImageFilterObj =
    itk::RegionOfInterestImageFilter<InputType,OutputType>::New();
  std::cout << "-------------RegionOfInterestImageFilter" << RegionOfInterestImageFilterObj;

  itk::ResampleImageFilter<InputType,OutputType>::Pointer ResampleImageFilterObj =
    itk::ResampleImageFilter<InputType,OutputType>::New();
  std::cout << "-------------ResampleImageFilter" << ResampleImageFilterObj;

  itk::RescaleIntensityImageFilter<InputType,OutputType>::Pointer RescaleIntensityImageFilterObj =
    itk::RescaleIntensityImageFilter<InputType,OutputType>::New();
  std::cout << "-------------RescaleIntensityImageFilter" << RescaleIntensityImageFilterObj;

  itk::ScalarToArrayCastImageFilter<InputType,VectorImageType>::Pointer ScalarToArrayCastImageFilterObj =
    itk::ScalarToArrayCastImageFilter<InputType,VectorImageType>::New();
  std::cout << "-------------ScalarToArrayCastImageFilter" << ScalarToArrayCastImageFilterObj;

  itk::ShiftScaleImageFilter<InputType,OutputType>::Pointer ShiftScaleImageFilterObj =
    itk::ShiftScaleImageFilter<InputType,OutputType>::New();
  std::cout << "-------------ShiftScaleImageFilter" << ShiftScaleImageFilterObj;

  itk::ShiftScaleInPlaceImageFilter<InputType>::Pointer ShiftScaleInPlaceImageFilterObj =
    itk::ShiftScaleInPlaceImageFilter<InputType>::New();
  std::cout << "-------------ShiftScaleInPlaceImageFilter" << ShiftScaleInPlaceImageFilterObj;

  itk::ShrinkImageFilter<InputType,OutputType>::Pointer ShrinkImageFilterObj =
    itk::ShrinkImageFilter<InputType,OutputType>::New();
  std::cout << "-------------ShrinkImageFilter" << ShrinkImageFilterObj;

  itk::SigmoidImageFilter<InputType,OutputType>::Pointer SigmoidImageFilterObj =
    itk::SigmoidImageFilter<InputType,OutputType>::New();
  std::cout << "-------------SigmoidImageFilter" << SigmoidImageFilterObj;

  itk::SimilarityIndexImageFilter<InputType,InputType>::Pointer SimilarityIndexImageFilterObj =
    itk::SimilarityIndexImageFilter<InputType,InputType>::New();
  std::cout << "-------------SimilarityIndexImageFilter" << SimilarityIndexImageFilterObj;

  itk::SinImageFilter<InputType,OutputType>::Pointer SinImageFilterObj =
    itk::SinImageFilter<InputType,OutputType>::New();
  std::cout << "-------------SinImageFilter" << SinImageFilterObj;

  itk::SmoothingRecursiveGaussianImageFilter<InputType,OutputType>::Pointer SmoothingRecursiveGaussianImageFilterObj =
    itk::SmoothingRecursiveGaussianImageFilter<InputType,OutputType>::New();
  std::cout << "-------------SmoothingRecursiveGaussianImageFilter" << SmoothingRecursiveGaussianImageFilterObj;

  itk::SobelEdgeDetectionImageFilter<InputType,OutputType>::Pointer SobelEdgeDetectionImageFilterObj =
    itk::SobelEdgeDetectionImageFilter<InputType,OutputType>::New();
  std::cout << "-------------SobelEdgeDetectionImageFilter" << SobelEdgeDetectionImageFilterObj;

  itk::SparseFieldLayer<node_type>::Pointer SparseFieldLayerObj =
    itk::SparseFieldLayer<node_type>::New();
  std::cout << "-------------SparseFieldLayer" << SparseFieldLayerObj;
#if 0
  itk::SparseFieldLevelSetImageFilter<VectorImageType,VectorImageType>::Pointer SparseFieldLevelSetImageFilterObj =
    itk::SparseFieldLevelSetImageFilter<VectorImageType,VectorImageType>::New();
  std::cout << "-------------SparseFieldLevelSetImageFilter" << SparseFieldLevelSetImageFilterObj;
#endif
  itk::SpatialFunctionImageEvaluatorFilter<GaussianSpatialFunctionType,InputType,OutputType>::Pointer SpatialFunctionImageEvaluatorFilterObj =
    itk::SpatialFunctionImageEvaluatorFilter<GaussianSpatialFunctionType,InputType,OutputType>::New();
  std::cout << "-------------SpatialFunctionImageEvaluatorFilter" << SpatialFunctionImageEvaluatorFilterObj;
#if 0
  itk::SpatialObjectToImageFilter<SpatialObjectType,OutputType>::Pointer SpatialObjectToImageFilterObj =
    itk::SpatialObjectToImageFilter<SpatialObjectType,OutputType>::New();
  std::cout << "-------------SpatialObjectToImageFilter" << SpatialObjectToImageFilterObj;
#endif
  itk::SqrtImageFilter<InputType,OutputType>::Pointer SqrtImageFilterObj =
    itk::SqrtImageFilter<InputType,OutputType>::New();
  std::cout << "-------------SqrtImageFilter" << SqrtImageFilterObj;

  itk::SquareImageFilter<InputType,OutputType>::Pointer SquareImageFilterObj =
    itk::SquareImageFilter<InputType,OutputType>::New();
  std::cout << "-------------SquareImageFilter" << SquareImageFilterObj;

  itk::SquaredDifferenceImageFilter<InputType,InputType,OutputType>::Pointer SquaredDifferenceImageFilterObj =
    itk::SquaredDifferenceImageFilter<InputType,InputType,OutputType>::New();
  std::cout << "-------------SquaredDifferenceImageFilter" << SquaredDifferenceImageFilterObj;

  itk::StatisticsImageFilter<InputType>::Pointer StatisticsImageFilterObj =
    itk::StatisticsImageFilter<InputType>::New();
  std::cout << "-------------StatisticsImageFilter" << StatisticsImageFilterObj;

  itk::StreamingImageFilter<InputType,OutputType>::Pointer StreamingImageFilterObj =
    itk::StreamingImageFilter<InputType,OutputType>::New();
  std::cout << "-------------StreamingImageFilter" << StreamingImageFilterObj;

  itk::SubtractImageFilter<InputType,InputType,OutputType>::Pointer SubtractImageFilterObj =
    itk::SubtractImageFilter<InputType,InputType,OutputType>::New();
  std::cout << "-------------SubtractImageFilter" << SubtractImageFilterObj;

  itk::TanImageFilter<InputType,OutputType>::Pointer TanImageFilterObj =
    itk::TanImageFilter<InputType,OutputType>::New();
  std::cout << "-------------TanImageFilter" << TanImageFilterObj;

  itk::TernaryAddImageFilter<InputType,InputType,InputType,OutputType>::Pointer TernaryAddImageFilterObj =
    itk::TernaryAddImageFilter<InputType,InputType,InputType,OutputType>::New();
  std::cout << "-------------TernaryAddImageFilter" << TernaryAddImageFilterObj;

  itk::TernaryMagnitudeImageFilter<InputType,InputType,InputType,OutputType>::Pointer TernaryMagnitudeImageFilterObj =
    itk::TernaryMagnitudeImageFilter<InputType,InputType,InputType,OutputType>::New();
  std::cout << "-------------TernaryMagnitudeImageFilter" << TernaryMagnitudeImageFilterObj;

  itk::TernaryMagnitudeSquaredImageFilter<InputType,InputType,InputType,OutputType>::Pointer TernaryMagnitudeSquaredImageFilterObj =
    itk::TernaryMagnitudeSquaredImageFilter<InputType,InputType,InputType,OutputType>::New();
  std::cout << "-------------TernaryMagnitudeSquaredImageFilter" << TernaryMagnitudeSquaredImageFilterObj;

  itk::ThresholdImageFilter<InputType>::Pointer ThresholdImageFilterObj =
    itk::ThresholdImageFilter<InputType>::New();
  std::cout << "-------------ThresholdImageFilter" << ThresholdImageFilterObj;

  itk::TobogganImageFilter<InputType>::Pointer TobogganImageFilterObj =
    itk::TobogganImageFilter<InputType>::New();
  std::cout << "-------------TobogganImageFilter" << TobogganImageFilterObj;

  itk::TransformMeshFilter<MeshType,MeshType,AffineTransformType>::Pointer TransformMeshFilterObj =
    itk::TransformMeshFilter<MeshType,MeshType,AffineTransformType>::New();
  std::cout << "-------------TransformMeshFilter" << TransformMeshFilterObj;

  itk::TwoOutputExampleImageFilter<InputType>::Pointer TwoOutputExampleImageFilterObj =
    itk::TwoOutputExampleImageFilter<InputType>::New();
  std::cout << "-------------TwoOutputExampleImageFilter" << TwoOutputExampleImageFilterObj;
#if 0
  itk::UnaryFunctorImageFilter<InputType,OutputType>::Pointer UnaryFunctorImageFilterObj =
    itk::UnaryFunctorImageFilter<InputType,OutputType>::New();
  std::cout << "-------------UnaryFunctorImageFilter" << UnaryFunctorImageFilterObj;
#endif
  itk::VTKImageExport<InputType>::Pointer VTKImageExportObj =
    itk::VTKImageExport<InputType>::New();
  std::cout << "-------------VTKImageExport" << VTKImageExportObj;

  itk::VTKImageImport<OutputType>::Pointer VTKImageImportObj =
    itk::VTKImageImport<OutputType>::New();
  std::cout << "-------------VTKImageImport" << VTKImageImportObj;

  itk::VectorCastImageFilter<VectorImageType,VectorImageType>::Pointer VectorCastImageFilterObj =
    itk::VectorCastImageFilter<VectorImageType,VectorImageType>::New();
  std::cout << "-------------VectorCastImageFilter" << VectorCastImageFilterObj;

  itk::VectorCurvatureAnisotropicDiffusionImageFilter<VectorImageType,VectorImageType>::Pointer VectorCurvatureAnisotropicDiffusionImageFilterObj =
    itk::VectorCurvatureAnisotropicDiffusionImageFilter<VectorImageType,VectorImageType>::New();
  std::cout << "-------------VectorCurvatureAnisotropicDiffusionImageFilter" << VectorCurvatureAnisotropicDiffusionImageFilterObj;

  itk::VectorCurvatureNDAnisotropicDiffusionFunction<VectorImageType>::Pointer VectorCurvatureNDAnisotropicDiffusionFunctionObj =
    itk::VectorCurvatureNDAnisotropicDiffusionFunction<VectorImageType>::New();
  std::cout << "-------------VectorCurvatureNDAnisotropicDiffusionFunction" << VectorCurvatureNDAnisotropicDiffusionFunctionObj;

  itk::VectorExpandImageFilter<VectorImageType,VectorImageType>::Pointer VectorExpandImageFilterObj =
    itk::VectorExpandImageFilter<VectorImageType,VectorImageType>::New();
  std::cout << "-------------VectorExpandImageFilter" << VectorExpandImageFilterObj;

  itk::VectorGradientAnisotropicDiffusionImageFilter<VectorImageType,VectorImageType>::Pointer VectorGradientAnisotropicDiffusionImageFilterObj =
    itk::VectorGradientAnisotropicDiffusionImageFilter<VectorImageType,VectorImageType>::New();
  std::cout << "-------------VectorGradientAnisotropicDiffusionImageFilter" << VectorGradientAnisotropicDiffusionImageFilterObj;

  itk::VectorGradientMagnitudeImageFilter<VectorImageType>::Pointer VectorGradientMagnitudeImageFilterObj =
    itk::VectorGradientMagnitudeImageFilter<VectorImageType>::New();
  std::cout << "-------------VectorGradientMagnitudeImageFilter" << VectorGradientMagnitudeImageFilterObj;

  itk::VectorGradientNDAnisotropicDiffusionFunction<VectorImageType>::Pointer VectorGradientNDAnisotropicDiffusionFunctionObj =
    itk::VectorGradientNDAnisotropicDiffusionFunction<VectorImageType>::New();
  std::cout << "-------------VectorGradientNDAnisotropicDiffusionFunction" << VectorGradientNDAnisotropicDiffusionFunctionObj;

  itk::VectorIndexSelectionCastImageFilter<VectorImageType,VectorImageType>::Pointer VectorIndexSelectionCastImageFilterObj =
    itk::VectorIndexSelectionCastImageFilter<VectorImageType,VectorImageType>::New();
  std::cout << "-------------VectorIndexSelectionCastImageFilter" << VectorIndexSelectionCastImageFilterObj;

  itk::VectorNeighborhoodOperatorImageFilter<VectorImageType,VectorImageType>::Pointer VectorNeighborhoodOperatorImageFilterObj =
    itk::VectorNeighborhoodOperatorImageFilter<VectorImageType,VectorImageType>::New();
  std::cout << "-------------VectorNeighborhoodOperatorImageFilter" << VectorNeighborhoodOperatorImageFilterObj;

  itk::WarpImageFilter<InputType,OutputType,VectorImageType>::Pointer WarpImageFilterObj =
    itk::WarpImageFilter<InputType,OutputType,VectorImageType>::New();
  std::cout << "-------------WarpImageFilter" << WarpImageFilterObj;

  itk::WrapPadImageFilter<InputType,OutputType>::Pointer WrapPadImageFilterObj =
    itk::WrapPadImageFilter<InputType,OutputType>::New();
  std::cout << "-------------WrapPadImageFilter" << WrapPadImageFilterObj;

  itk::ZeroCrossingBasedEdgeDetectionImageFilter<InputType,OutputType>::Pointer ZeroCrossingBasedEdgeDetectionImageFilterObj =
    itk::ZeroCrossingBasedEdgeDetectionImageFilter<InputType,OutputType>::New();
  std::cout << "-------------ZeroCrossingBasedEdgeDetectionImageFilter" << ZeroCrossingBasedEdgeDetectionImageFilterObj;

  itk::ZeroCrossingImageFilter<InputType,OutputType>::Pointer ZeroCrossingImageFilterObj =
    itk::ZeroCrossingImageFilter<InputType,OutputType>::New();
  std::cout << "-------------ZeroCrossingImageFilter" << ZeroCrossingImageFilterObj;

  return 0;
}

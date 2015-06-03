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

#include "itkAffineTransform.h"
#include "itkPoint.h"
#include "itkMesh.h"

#include "itkImageToParametricSpaceFilter.h"
#include "itkImportImageFilter.h"
#include "itkIntensityWindowingImageFilter.h"
#include "itkInteriorExteriorMeshFilter.h"
#include "itkInterpolateImageFilter.h"
#include "itkInterpolateImagePointsFilter.h"
#include "itkIsolatedConnectedImageFilter.h"
#include "itkJoinImageFilter.h"
#include "itkJoinSeriesImageFilter.h"
#include "itkLaplacianImageFilter.h"
#include "itkLaplacianRecursiveGaussianImageFilter.h"
#include "itkLog10ImageFilter.h"
#include "itkLogImageFilter.h"

#include "itkMaskImageFilter.h"
#include "itkMaskNegatedImageFilter.h"
#include "itkMaximumImageFilter.h"
#include "itkMeanImageFilter.h"
#include "itkMedianImageFilter.h"
#include "itkMinimumImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkMinimumMaximumImageFilter.h"
#include "itkMirrorPadImageFilter.h"
#include "itkMultiplyImageFilter.h"
#include "itkNaryAddImageFilter.h"
#include "itkNeighborhoodConnectedImageFilter.h"
#include "itkMaskNeighborhoodOperatorImageFilter.h"
#include "itkNoiseImageFilter.h"
#include "itkNonThreadedShrinkImageFilter.h"
#include "itkNormalizeImageFilter.h"
#include "itkParametricSpaceToImageSpaceMeshFilter.h"
#include "itkPasteImageFilter.h"
#include "itkPermuteAxesImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkReflectImageFilter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkResampleImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkComposeImageFilter.h"
#include "itkShiftScaleInPlaceImageFilter.h"
#include "itkSigmoidImageFilter.h"
#include "itkSimilarityIndexImageFilter.h"
#include "itkSinImageFilter.h"
#include "itkSmoothingRecursiveGaussianImageFilter.h"
#include "itkSobelEdgeDetectionImageFilter.h"
#include "itkSparseFieldLevelSetImageFilter.h"
#include "itkSpatialFunctionImageEvaluatorFilter.h"
#include "itkSpatialObjectToImageFilter.h"
#include "itkSqrtImageFilter.h"
#include "itkSquareImageFilter.h"
#include "itkSquaredDifferenceImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkSubtractImageFilter.h"
#include "itkTanImageFilter.h"
#include "itkTernaryAddImageFilter.h"
#include "itkTernaryMagnitudeImageFilter.h"
#include "itkTernaryMagnitudeSquaredImageFilter.h"
#include "itkThresholdImageFilter.h"
#include "itkTobogganImageFilter.h"
#include "itkTransformMeshFilter.h"
#include "itkTwoOutputExampleImageFilter.h"
#include "ITKVTKImageExport.h"
#include "itkVTKImageImport.h"
#include "itkVectorCastImageFilter.h"
#include "itkVectorCurvatureAnisotropicDiffusionImageFilter.h"
#include "itkVectorExpandImageFilter.h"
#include "itkVectorGradientAnisotropicDiffusionImageFilter.h"
#include "itkVectorGradientMagnitudeImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkVectorNeighborhoodOperatorImageFilter.h"
#include "itkWarpImageFilter.h"
#include "itkWrapPadImageFilter.h"
#include "itkZeroCrossingBasedEdgeDetectionImageFilter.h"
#include "itkZeroCrossingImageFilter.h"

#include "itkSphereSpatialFunction.h"
#include "itkGaussianSpatialFunction.h"


struct node_type
{
  unsigned int value;
  node_type *Next;
  node_type *Previous;
};


int itkBasicFiltersPrintTest2(int , char* [])
{
  typedef itk::Image<float,2>         InputType;
  typedef itk::Image<float,2>         OutputType;
  typedef itk::Image<unsigned char,2> CharType;
  typedef itk::Image<unsigned char,3> CharType3D;

  typedef itk::Point<float,2>       MeshPixelType;
  typedef itk::Mesh<MeshPixelType>  MeshType;

  typedef itk::Vector<float,2>     VectorType;
  typedef itk::Image<VectorType,2> VectorImageType;

  typedef itk::CovariantVector<float,2>     CovariantVectorType;
  typedef itk::Image<CovariantVectorType,2> CovariantVectorImageType;

  // Used for TransformMeshFilter
  typedef itk::AffineTransform<float,3> AffineTransformType;

  // Used for InteriorExteriorMeshFilter
  typedef itk::Point<float, 3> PointType;
  typedef itk::SphereSpatialFunction< MeshType::PointDimension,
    MeshType::PointType > SphereSpatialFunctionType;

  // Used for SpatialFunctionImageEvaluator
  typedef itk::GaussianSpatialFunction<char,2> GaussianSpatialFunctionType;

  // Used for MaskImageFilter
  typedef itk::Image<unsigned char,2> MaskImageType;

  itk::ImageToParametricSpaceFilter<InputType,MeshType>::Pointer ImageToParametricSpaceFilterObj =
    itk::ImageToParametricSpaceFilter<InputType,MeshType>::New();
  std::cout << "-------------ImageToParametricSpaceFilter" << ImageToParametricSpaceFilterObj;

  itk::ImportImageFilter<float>::Pointer ImportImageFilterObj =
    itk::ImportImageFilter<float>::New();
  std::cout << "-------------ImportImageFilter" << ImportImageFilterObj;

  itk::IntensityWindowingImageFilter<InputType,OutputType>::Pointer IntensityWindowingImageFilterObj =
    itk::IntensityWindowingImageFilter<InputType,OutputType>::New();
  std::cout << "-------------IntensityWindowingImageFilter" << IntensityWindowingImageFilterObj;

  itk::InteriorExteriorMeshFilter<MeshType,MeshType,SphereSpatialFunctionType>::Pointer InteriorExteriorMeshFilterObj =
    itk::InteriorExteriorMeshFilter<MeshType,MeshType,SphereSpatialFunctionType>::New();
  std::cout << "-------------InteriorExteriorMeshFilter" << InteriorExteriorMeshFilterObj;

  itk::InterpolateImageFilter<InputType,OutputType>::Pointer InterpolateImageFilterObj =
    itk::InterpolateImageFilter<InputType,OutputType>::New();
  std::cout << "-------------InterpolateImageFilter" << InterpolateImageFilterObj;

  itk::InterpolateImagePointsFilter<InputType,OutputType>::Pointer InterpolateImagePointsFilterObj =
    itk::InterpolateImagePointsFilter<InputType,OutputType>::New();
  std::cout << "-------------InterpolateImagePointsFilter" << InterpolateImagePointsFilterObj;

  itk::IsolatedConnectedImageFilter<InputType,OutputType>::Pointer IsolatedConnectedImageFilterObj =
    itk::IsolatedConnectedImageFilter<InputType,OutputType>::New();
  std::cout << "-------------IsolatedConnectedImageFilter" << IsolatedConnectedImageFilterObj;

  itk::JoinImageFilter<InputType,OutputType>::Pointer JoinImageFilterObj =
    itk::JoinImageFilter<InputType,OutputType>::New();
  std::cout << "-------------JoinImageFilter" << JoinImageFilterObj;

  // NOTE: A compile error should be here (by extending itk::Concept?),
  // because InputImageDimension must be less than OutputImageDimension.
  itk::JoinSeriesImageFilter<InputType,OutputType>::Pointer JoinSeriesImageFilterObj =
    itk::JoinSeriesImageFilter<InputType,OutputType>::New();
  std::cout << "-------------JoinSeriesImageFilter" << JoinSeriesImageFilterObj;

  itk::LaplacianImageFilter<InputType,OutputType>::Pointer LaplacianImageFilterObj =
    itk::LaplacianImageFilter<InputType,OutputType>::New();
  std::cout << "-------------LaplacianImageFilter" << LaplacianImageFilterObj;

  itk::LaplacianRecursiveGaussianImageFilter<InputType,OutputType>::Pointer LaplacianRecursiveGaussianImageFilterObj =
    itk::LaplacianRecursiveGaussianImageFilter<InputType,OutputType>::New();
  std::cout << "-------------LaplacianRecursiveGaussianImageFilter" << LaplacianRecursiveGaussianImageFilterObj;

  itk::Log10ImageFilter<InputType,OutputType>::Pointer Log10ImageFilterObj =
    itk::Log10ImageFilter<InputType,OutputType>::New();
  std::cout << "-------------Log10ImageFilter" << Log10ImageFilterObj;

  itk::LogImageFilter<InputType,OutputType>::Pointer LogImageFilterObj =
    itk::LogImageFilter<InputType,OutputType>::New();
  std::cout << "-------------LogImageFilter" << LogImageFilterObj;

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

  itk::NeighborhoodConnectedImageFilter<InputType,OutputType>::Pointer NeighborhoodConnectedImageFilterObj =
    itk::NeighborhoodConnectedImageFilter<InputType,OutputType>::New();
  std::cout << "-------------NeighborhoodConnectedImageFilter" << NeighborhoodConnectedImageFilterObj;

  itk::NeighborhoodOperatorImageFilter<InputType,OutputType>::Pointer NeighborhoodOperatorImageFilterObj =
    itk::NeighborhoodOperatorImageFilter<InputType,OutputType>::New();
  std::cout << "-------------NeighborhoodOperatorImageFilter" << NeighborhoodOperatorImageFilterObj;

  itk::MaskNeighborhoodOperatorImageFilter<InputType,InputType,OutputType>::Pointer MaskNeighborhoodOperatorImageFilterObj =
    itk::MaskNeighborhoodOperatorImageFilter<InputType,InputType,OutputType>::New();
  std::cout << "-------------MaskNeighborhoodOperatorImageFilter" << MaskNeighborhoodOperatorImageFilterObj;

  itk::NoiseImageFilter<InputType,OutputType>::Pointer NoiseImageFilterObj =
    itk::NoiseImageFilter<InputType,OutputType>::New();
  std::cout << "-------------NoiseImageFilter" << NoiseImageFilterObj;

  itk::NormalizeImageFilter<InputType,OutputType>::Pointer NormalizeImageFilterObj =
    itk::NormalizeImageFilter<InputType,OutputType>::New();
  std::cout << "-------------NormalizeImageFilter" << NormalizeImageFilterObj;

  itk::PadImageFilter<InputType,OutputType>::Pointer PadImageFilterObj =
    itk::PadImageFilter<InputType,OutputType>::New();
  std::cout << "-------------PadImageFilter" << PadImageFilterObj;

  itk::PasteImageFilter<InputType>::Pointer PasteImageFilterObj =
    itk::PasteImageFilter<InputType>::New();
  std::cout << "-------------PasteImageFilter" << PasteImageFilterObj;

  itk::PermuteAxesImageFilter<InputType>::Pointer PermuteAxesImageFilterObj =
    itk::PermuteAxesImageFilter<InputType>::New();
  std::cout << "-------------PermuteAxesImageFilter" << PermuteAxesImageFilterObj;

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

  itk::ComposeImageFilter<InputType,VectorImageType>::Pointer ComposeImageFilterObj =
    itk::ComposeImageFilter<InputType,VectorImageType>::New();
  std::cout << "-------------ComposeImageFilter" << ComposeImageFilterObj;

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

  itk::SpatialFunctionImageEvaluatorFilter<GaussianSpatialFunctionType,InputType,OutputType>::Pointer SpatialFunctionImageEvaluatorFilterObj =
    itk::SpatialFunctionImageEvaluatorFilter<GaussianSpatialFunctionType,InputType,OutputType>::New();
  std::cout << "-------------SpatialFunctionImageEvaluatorFilter" << SpatialFunctionImageEvaluatorFilterObj;

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

  return EXIT_SUCCESS;
}

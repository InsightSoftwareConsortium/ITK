/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBasicFiltersPrintTest.cxx
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


int itkBasicFiltersPrintTest(int , char* [])
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


  itk::AcosImageFilter<InputType,OutputType>::Pointer AcosImageFilterObj =
    itk::AcosImageFilter<InputType,OutputType>::New();
  std::cout << "-------------AcosImageFilter" << AcosImageFilterObj;

  itk::AddImageFilter<InputType,InputType,OutputType>::Pointer AddImageFilterObj =
    itk::AddImageFilter<InputType,InputType,OutputType>::New();
  std::cout << "-------------AddImageFilter" << AddImageFilterObj;

  itk::AsinImageFilter<InputType,OutputType>::Pointer AsinImageFilterObj =
    itk::AsinImageFilter<InputType,OutputType>::New();
  std::cout << "-------------AsinImageFilter" << AsinImageFilterObj;

  itk::Atan2ImageFilter<InputType,InputType,OutputType>::Pointer Atan2ImageFilterObj =
    itk::Atan2ImageFilter<InputType,InputType,OutputType>::New();
  std::cout << "-------------Atan2ImageFilter" << Atan2ImageFilterObj;

  itk::AtanImageFilter<InputType,OutputType>::Pointer AtanImageFilterObj =
    itk::AtanImageFilter<InputType,OutputType>::New();
  std::cout << "-------------AtanImageFilter" << AtanImageFilterObj;

  itk::BSplineDecompositionImageFilter<InputType,OutputType>::Pointer BSplineDecompositionImageFilterObj =
    itk::BSplineDecompositionImageFilter<InputType,OutputType>::New();
  std::cout << "-------------BSplineDecompositionImageFilter" << BSplineDecompositionImageFilterObj;

  itk::BSplineDownsampleImageFilter<InputType,OutputType>::Pointer BSplineDownsampleImageFilterObj =
    itk::BSplineDownsampleImageFilter<InputType,OutputType>::New();
  std::cout << "-------------BSplineDownsampleImageFilter" << BSplineDownsampleImageFilterObj;

  itk::BSplineInterpolateImageFunction<InputType>::Pointer BSplineInterpolateImageFunctionObj =
    itk::BSplineInterpolateImageFunction<InputType>::New();
  std::cout << "-------------BSplineInterpolateImageFunction" << BSplineInterpolateImageFunctionObj;

  itk::BSplineResampleImageFunction<InputType>::Pointer BSplineResampleImageFunctionObj =
    itk::BSplineResampleImageFunction<InputType>::New();
  std::cout << "-------------BSplineResampleImageFunction" << BSplineResampleImageFunctionObj;

  itk::BSplineUpsampleImageFilter<InputType,OutputType>::Pointer BSplineUpsampleImageFilterObj =
    itk::BSplineUpsampleImageFilter<InputType,OutputType>::New();
  std::cout << "-------------BSplineUpsampleImageFilter" << BSplineUpsampleImageFilterObj;

  itk::BilateralImageFilter<InputType,OutputType>::Pointer BilateralImageFilterObj =
    itk::BilateralImageFilter<InputType,OutputType>::New();
  std::cout << "-------------BilateralImageFilter" << BilateralImageFilterObj;

  itk::BinaryDilateImageFilter<InputType,OutputType,KernelType>::Pointer BinaryDilateImageFilterObj =
    itk::BinaryDilateImageFilter<InputType,OutputType,KernelType>::New();
  std::cout << "-------------BinaryDilateImageFilter" << BinaryDilateImageFilterObj;

  itk::BinaryErodeImageFilter<InputType,OutputType,KernelType>::Pointer BinaryErodeImageFilterObj =
    itk::BinaryErodeImageFilter<InputType,OutputType,KernelType>::New();
  std::cout << "-------------BinaryErodeImageFilter" << BinaryErodeImageFilterObj;

  itk::BinaryMagnitudeImageFilter<InputType,InputType,OutputType>::Pointer BinaryMagnitudeImageFilterObj =
    itk::BinaryMagnitudeImageFilter<InputType,InputType,OutputType>::New();
  std::cout << "-------------BinaryMagnitudeImageFilter" << BinaryMagnitudeImageFilterObj;

  itk::BinaryMedianImageFilter<InputType,OutputType>::Pointer BinaryMedianImageFilterObj =
    itk::BinaryMedianImageFilter<InputType,OutputType>::New();
  std::cout << "-------------BinaryMedianImageFilter" << BinaryMedianImageFilterObj;

  itk::BinaryThresholdImageFilter<InputType,OutputType>::Pointer BinaryThresholdImageFilterObj =
    itk::BinaryThresholdImageFilter<InputType,OutputType>::New();
  std::cout << "-------------BinaryThresholdImageFilter" << BinaryThresholdImageFilterObj;

  itk::BinomialBlurImageFilter<InputType,OutputType>::Pointer BinomialBlurImageFilterObj =
    itk::BinomialBlurImageFilter<InputType,OutputType>::New();
  std::cout << "-------------BinomialBlurImageFilter" << BinomialBlurImageFilterObj;

  itk::BloxBoundaryPointImageToBloxBoundaryProfileImageFilter<CharType3D>::Pointer BloxBoundaryPointImageToBloxBoundaryProfileImageFilterObj =
    itk::BloxBoundaryPointImageToBloxBoundaryProfileImageFilter<CharType3D>::New();
  std::cout << "-------------BloxBoundaryPointImageToBloxBoundaryProfileImageFilter" << BloxBoundaryPointImageToBloxBoundaryProfileImageFilterObj;

  itk::BloxBoundaryPointToCoreAtomImageFilter<3>::Pointer BloxBoundaryPointToCoreAtomImageFilterObj =
    itk::BloxBoundaryPointToCoreAtomImageFilter<3>::New();
  std::cout << "-------------BloxBoundaryPointToCoreAtomImageFilter" << BloxBoundaryPointToCoreAtomImageFilterObj;

  itk::CannyEdgeDetectionImageFilter<InputType,OutputType>::Pointer CannyEdgeDetectionImageFilterObj =
    itk::CannyEdgeDetectionImageFilter<InputType,OutputType>::New();
  std::cout << "-------------CannyEdgeDetectionImageFilter" << CannyEdgeDetectionImageFilterObj;

  itk::CastImageFilter<InputType,OutputType>::Pointer CastImageFilterObj =
    itk::CastImageFilter<InputType,OutputType>::New();
  std::cout << "-------------CastImageFilter" << CastImageFilterObj;

  itk::ChangeInformationImageFilter<InputType>::Pointer ChangeInformationImageFilterObj =
    itk::ChangeInformationImageFilter<InputType>::New();
  std::cout << "-------------ChangeInformationImageFilter" << ChangeInformationImageFilterObj;

  itk::ComposeRGBImageFilter<InputType>::Pointer ComposeRGBImageFilterObj =
    itk::ComposeRGBImageFilter<InputType>::New();
  std::cout << "-------------ComposeRGBImageFilter" << ComposeRGBImageFilterObj;

  itk::ConfidenceConnectedImageFilter<InputType,OutputType>::Pointer ConfidenceConnectedImageFilterObj =
    itk::ConfidenceConnectedImageFilter<InputType,OutputType>::New();
  std::cout << "-------------ConfidenceConnectedImageFilter" << ConfidenceConnectedImageFilterObj;

  itk::ConnectedThresholdImageFilter<InputType,OutputType>::Pointer ConnectedThresholdImageFilterObj =
    itk::ConnectedThresholdImageFilter<InputType,OutputType>::New();
  std::cout << "-------------ConnectedThresholdImageFilter" << ConnectedThresholdImageFilterObj;

  itk::ConstantPadImageFilter<InputType,OutputType>::Pointer ConstantPadImageFilterObj =
    itk::ConstantPadImageFilter<InputType,OutputType>::New();
  std::cout << "-------------ConstantPadImageFilter" << ConstantPadImageFilterObj;

  itk::CosImageFilter<InputType,OutputType>::Pointer CosImageFilterObj =
    itk::CosImageFilter<InputType,OutputType>::New();
  std::cout << "-------------CosImageFilter" << CosImageFilterObj;

  itk::CropImageFilter<InputType,OutputType>::Pointer CropImageFilterObj =
    itk::CropImageFilter<InputType,OutputType>::New();
  std::cout << "-------------CropImageFilter" << CropImageFilterObj;

  itk::CurvatureAnisotropicDiffusionImageFilter<InputType,OutputType>::Pointer CurvatureAnisotropicDiffusionImageFilterObj =
    itk::CurvatureAnisotropicDiffusionImageFilter<InputType,OutputType>::New();
  std::cout << "-------------CurvatureAnisotropicDiffusionImageFilter" << CurvatureAnisotropicDiffusionImageFilterObj;

  itk::CurvatureNDAnisotropicDiffusionFunction<InputType>::Pointer CurvatureNDAnisotropicDiffusionFunctionObj =
    itk::CurvatureNDAnisotropicDiffusionFunction<InputType>::New();
  std::cout << "-------------CurvatureNDAnisotropicDiffusionFunction" << CurvatureNDAnisotropicDiffusionFunctionObj;

  itk::DanielssonDistanceMapImageFilter<InputType,OutputType>::Pointer DanielssonDistanceMapImageFilterObj =
    itk::DanielssonDistanceMapImageFilter<InputType,OutputType>::New();
  std::cout << "-------------DanielssonDistanceMapImageFilter" << DanielssonDistanceMapImageFilterObj;

  itk::DerivativeImageFilter<InputType,OutputType>::Pointer DerivativeImageFilterObj =
    itk::DerivativeImageFilter<InputType,OutputType>::New();
  std::cout << "-------------DerivativeImageFilter" << DerivativeImageFilterObj;

  itk::DifferenceOfGaussiansGradientImageFilter<InputType,VectorType>::Pointer DifferenceOfGaussiansGradientImageFilterObj =
    itk::DifferenceOfGaussiansGradientImageFilter<InputType,VectorType>::New();
  std::cout << "-------------DifferenceOfGaussiansGradientImageFilter" << DifferenceOfGaussiansGradientImageFilterObj;

  itk::DilateObjectMorphologyImageFilter<InputType,OutputType,KernelType>::Pointer DilateObjectMorphologyImageFilterObj =
    itk::DilateObjectMorphologyImageFilter<InputType,OutputType,KernelType>::New();
  std::cout << "-------------DilateObjectMorphologyImageFilter" << DilateObjectMorphologyImageFilterObj;

  itk::DirectedHausdorffDistanceImageFilter<InputType,OutputType>::Pointer DirectedHausdorffDistanceImageFilterObj =
    itk::DirectedHausdorffDistanceImageFilter<InputType,OutputType>::New();
  std::cout << "-------------DirectedHausdorffDistanceImageFilter" << DirectedHausdorffDistanceImageFilterObj;

  itk::DiscreteGaussianImageFilter<InputType,OutputType>::Pointer DiscreteGaussianImageFilterObj =
    itk::DiscreteGaussianImageFilter<InputType,OutputType>::New();
  std::cout << "-------------DiscreteGaussianImageFilter" << DiscreteGaussianImageFilterObj;

  itk::DivideImageFilter<InputType,InputType,OutputType>::Pointer DivideImageFilterObj =
    itk::DivideImageFilter<InputType,InputType,OutputType>::New();
  std::cout << "-------------DivideImageFilter" << DivideImageFilterObj;

  itk::EdgePotentialImageFilter<CovariantVectorImageType,OutputType>::Pointer EdgePotentialImageFilterObj =
    itk::EdgePotentialImageFilter<CovariantVectorImageType,OutputType>::New();
  std::cout << "-------------EdgePotentialImageFilter" << EdgePotentialImageFilterObj;

  itk::EigenAnalysis2DImageFilter<InputType,InputType,VectorImageType>::Pointer EigenAnalysis2DImageFilterObj =
    itk::EigenAnalysis2DImageFilter<InputType,InputType,VectorImageType>::New();
  std::cout << "-------------EigenAnalysis2DImageFilter" << EigenAnalysis2DImageFilterObj;

  itk::ErodeObjectMorphologyImageFilter<InputType,OutputType,KernelType>::Pointer ErodeObjectMorphologyImageFilterObj =
    itk::ErodeObjectMorphologyImageFilter<InputType,OutputType,KernelType>::New();
  std::cout << "-------------ErodeObjectMorphologyImageFilter" << ErodeObjectMorphologyImageFilterObj;

  itk::ExpImageFilter<InputType,OutputType>::Pointer ExpImageFilterObj =
    itk::ExpImageFilter<InputType,OutputType>::New();
  std::cout << "-------------ExpImageFilter" << ExpImageFilterObj;

  itk::ExpNegativeImageFilter<InputType,OutputType>::Pointer ExpNegativeImageFilterObj =
    itk::ExpNegativeImageFilter<InputType,OutputType>::New();
  std::cout << "-------------ExpNegativeImageFilter" << ExpNegativeImageFilterObj;

  itk::ExpandImageFilter<InputType,OutputType>::Pointer ExpandImageFilterObj =
    itk::ExpandImageFilter<InputType,OutputType>::New();
  std::cout << "-------------ExpandImageFilter" << ExpandImageFilterObj;

  itk::ExtractImageFilter<InputType,OutputType>::Pointer ExtractImageFilterObj =
    itk::ExtractImageFilter<InputType,OutputType>::New();
  std::cout << "-------------ExtractImageFilter" << ExtractImageFilterObj;

  itk::FlipImageFilter<InputType>::Pointer FlipImageFilterObj =
    itk::FlipImageFilter<InputType>::New();
  std::cout << "-------------FlipImageFilter" << FlipImageFilterObj;

  itk::GaussianImageSource<OutputType>::Pointer GaussianImageSourceObj =
    itk::GaussianImageSource<OutputType>::New();
  std::cout << "-------------GaussianImageSource" << GaussianImageSourceObj;

  itk::GradientAnisotropicDiffusionImageFilter<InputType,OutputType>::Pointer GradientAnisotropicDiffusionImageFilterObj =
    itk::GradientAnisotropicDiffusionImageFilter<InputType,OutputType>::New();
  std::cout << "-------------GradientAnisotropicDiffusionImageFilter" << GradientAnisotropicDiffusionImageFilterObj;

  itk::GradientImageFilter<InputType>::Pointer GradientImageFilterObj =
    itk::GradientImageFilter<InputType>::New();
  std::cout << "-------------GradientImageFilter" << GradientImageFilterObj;

  itk::GradientImageToBloxBoundaryPointImageFilter<DOGFilterType::TOutputImage>::Pointer GradientImageToBloxBoundaryPointImageFilterObj =
    itk::GradientImageToBloxBoundaryPointImageFilter<DOGFilterType::TOutputImage>::New();
  std::cout << "-------------GradientImageToBloxBoundaryPointImageFilter" << GradientImageToBloxBoundaryPointImageFilterObj;

  itk::GradientMagnitudeImageFilter<InputType,OutputType>::Pointer GradientMagnitudeImageFilterObj =
    itk::GradientMagnitudeImageFilter<InputType,OutputType>::New();
  std::cout << "-------------GradientMagnitudeImageFilter" << GradientMagnitudeImageFilterObj;

  itk::GradientMagnitudeRecursiveGaussianImageFilter<InputType>::Pointer GradientMagnitudeRecursiveGaussianImageFilterObj =
    itk::GradientMagnitudeRecursiveGaussianImageFilter<InputType>::New();
  std::cout << "-------------GradientMagnitudeRecursiveGaussianImageFilter" << GradientMagnitudeRecursiveGaussianImageFilterObj;

  itk::GradientNDAnisotropicDiffusionFunction<InputType>::Pointer GradientNDAnisotropicDiffusionFunctionObj =
    itk::GradientNDAnisotropicDiffusionFunction<InputType>::New();
  std::cout << "-------------GradientNDAnisotropicDiffusionFunction" << GradientNDAnisotropicDiffusionFunctionObj;

  itk::GradientRecursiveGaussianImageFilter<InputType>::Pointer GradientRecursiveGaussianImageFilterObj =
    itk::GradientRecursiveGaussianImageFilter<InputType>::New();
  std::cout << "-------------GradientRecursiveGaussianImageFilter" << GradientRecursiveGaussianImageFilterObj;

  itk::GradientToMagnitudeImageFilter<VectorImageType,OutputType>::Pointer GradientToMagnitudeImageFilterObj =
    itk::GradientToMagnitudeImageFilter<VectorImageType,OutputType>::New();
  std::cout << "-------------GradientToMagnitudeImageFilter" << GradientToMagnitudeImageFilterObj;

  itk::GrayscaleDilateImageFilter<InputType,OutputType,KernelType>::Pointer GrayscaleDilateImageFilterObj =
    itk::GrayscaleDilateImageFilter<InputType,OutputType,KernelType>::New();
  std::cout << "-------------GrayscaleDilateImageFilter" << GrayscaleDilateImageFilterObj;

  itk::GrayscaleErodeImageFilter<InputType,OutputType,KernelType>::Pointer GrayscaleErodeImageFilterObj =
    itk::GrayscaleErodeImageFilter<InputType,OutputType,KernelType>::New();
  std::cout << "-------------GrayscaleErodeImageFilter" << GrayscaleErodeImageFilterObj;

  itk::GrayscaleFunctionDilateImageFilter<InputType,OutputType,KernelType>::Pointer GrayscaleFunctionDilateImageFilterObj =
    itk::GrayscaleFunctionDilateImageFilter<InputType,OutputType,KernelType>::New();
  std::cout << "-------------GrayscaleFunctionDilateImageFilter" << GrayscaleFunctionDilateImageFilterObj;

  itk::GrayscaleFunctionErodeImageFilter<InputType,OutputType,KernelType>::Pointer GrayscaleFunctionErodeImageFilterObj =
    itk::GrayscaleFunctionErodeImageFilter<InputType,OutputType,KernelType>::New();
  std::cout << "-------------GrayscaleFunctionErodeImageFilter" << GrayscaleFunctionErodeImageFilterObj;

  itk::HardConnectedComponentImageFilter<InputType,CharType>::Pointer HardConnectedComponentImageFilterObj =
    itk::HardConnectedComponentImageFilter<InputType,CharType>::New();
  std::cout << "-------------HardConnectedComponentImageFilter" << HardConnectedComponentImageFilterObj;

  itk::HausdorffDistanceImageFilter<InputType,OutputType>::Pointer HausdorffDistanceImageFilterObj =
    itk::HausdorffDistanceImageFilter<InputType,OutputType>::New();
  std::cout << "-------------HausdorffDistanceImageFilter" << HausdorffDistanceImageFilterObj;

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

  return 0;
}

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

#include "itkMesh.h"
#include "itkAcosImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkAdaptiveHistogramEqualizationImageFilter.h"
#include "itkAsinImageFilter.h"
#include "itkAtan2ImageFilter.h"
#include "itkAtanImageFilter.h"
#include "itkBSplineDownsampleImageFilter.h"
#include "itkBSplineResampleImageFunction.h"
#include "itkBSplineUpsampleImageFilter.h"
#include "itkBilateralImageFilter.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryMagnitudeImageFilter.h"
#include "itkBinaryMedianImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkBinomialBlurImageFilter.h"
#include "itkCannyEdgeDetectionImageFilter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkComposeImageFilter.h"
#include "itkConfidenceConnectedImageFilter.h"
#include "itkConnectedThresholdImageFilter.h"
#include "itkConstantPadImageFilter.h"
#include "itkCosImageFilter.h"
#include "itkCropImageFilter.h"
#include "itkCurvatureAnisotropicDiffusionImageFilter.h"
#include "itkSignedDanielssonDistanceMapImageFilter.h"
#include "itkDerivativeImageFilter.h"
#include "itkDifferenceOfGaussiansGradientImageFilter.h"
#include "itkDiffusionTensor3DReconstructionImageFilter.h"
#include "itkDilateObjectMorphologyImageFilter.h"
#include "itkDivideImageFilter.h"
#include "itkEdgePotentialImageFilter.h"
#include "itkEigenAnalysis2DImageFilter.h"
#include "itkErodeObjectMorphologyImageFilter.h"
#include "itkExpImageFilter.h"
#include "itkExpNegativeImageFilter.h"
#include "itkExpandImageFilter.h"
#include "itkFlipImageFilter.h"
#include "itkGradientAnisotropicDiffusionImageFilter.h"
#include "itkGradientImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkGrayscaleDilateImageFilter.h"
#include "itkGrayscaleErodeImageFilter.h"
#include "itkGrayscaleFunctionDilateImageFilter.h"
#include "itkGrayscaleFunctionErodeImageFilter.h"
#include "itkHardConnectedComponentImageFilter.h"
#include "itkHausdorffDistanceImageFilter.h"

#include "itkSphereSpatialFunction.h"


struct node_type
{
  unsigned int value;
  node_type *Next;
  node_type *Previous;
};


int itkBasicFiltersPrintTest(int , char* [])
{
  typedef itk::Image<float,2>         InputType;
  typedef itk::Image<float,2>         OutputType;
  typedef itk::Image<unsigned char,2> CharType;

  typedef itk::Vector<float,2>     VectorType;
  typedef itk::Image<VectorType,2> VectorImageType;

  typedef itk::CovariantVector<float,2>     CovariantVectorType;
  typedef itk::Image<CovariantVectorType,2> CovariantVectorImageType;

  //typedef itk::Neighborhood<unsigned short,2> KernelType;
  typedef itk::BinaryBallStructuringElement<unsigned short,2> KernelType;


  itk::AcosImageFilter<InputType,OutputType>::Pointer AcosImageFilterObj =
    itk::AcosImageFilter<InputType,OutputType>::New();
  std::cout << "-------------AcosImageFilter" << AcosImageFilterObj;

  itk::AdaptiveHistogramEqualizationImageFilter<InputType>::Pointer AdaptiveHistogramEqualizationImageFilterObj =
    itk::AdaptiveHistogramEqualizationImageFilter<InputType>::New();
  std::cout << "-------------AdaptiveHistogramEqualizationImageFilter" << AdaptiveHistogramEqualizationImageFilterObj;

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

  itk::CannyEdgeDetectionImageFilter<InputType,OutputType>::Pointer CannyEdgeDetectionImageFilterObj =
    itk::CannyEdgeDetectionImageFilter<InputType,OutputType>::New();
  std::cout << "-------------CannyEdgeDetectionImageFilter" << CannyEdgeDetectionImageFilterObj;

  itk::CastImageFilter<InputType,OutputType>::Pointer CastImageFilterObj =
    itk::CastImageFilter<InputType,OutputType>::New();
  std::cout << "-------------CastImageFilter" << CastImageFilterObj;

  itk::ChangeInformationImageFilter<InputType>::Pointer ChangeInformationImageFilterObj =
    itk::ChangeInformationImageFilter<InputType>::New();
  std::cout << "-------------ChangeInformationImageFilter" << ChangeInformationImageFilterObj;

  itk::ComposeImageFilter<InputType>::Pointer ComposeImageFilterObj =
    itk::ComposeImageFilter<InputType>::New();
  std::cout << "-------------ComposeImageFilter" << ComposeImageFilterObj;

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

  itk::SignedDanielssonDistanceMapImageFilter<InputType,OutputType>::Pointer SignedDanielssonDistanceMapImageFilterObj =
    itk::SignedDanielssonDistanceMapImageFilter<InputType,OutputType>::New();
  std::cout << "-------------SignedDanielssonDistanceMapImageFilter" << SignedDanielssonDistanceMapImageFilterObj;

  itk::DerivativeImageFilter<InputType,OutputType>::Pointer DerivativeImageFilterObj =
    itk::DerivativeImageFilter<InputType,OutputType>::New();
  std::cout << "-------------DerivativeImageFilter" << DerivativeImageFilterObj;

  itk::DifferenceOfGaussiansGradientImageFilter<InputType,float>::Pointer DifferenceOfGaussiansGradientImageFilterObj =
    itk::DifferenceOfGaussiansGradientImageFilter<InputType,float>::New();
  std::cout << "-------------DifferenceOfGaussiansGradientImageFilter" << DifferenceOfGaussiansGradientImageFilterObj;

  itk::DiffusionTensor3DReconstructionImageFilter<float,double,float>::Pointer DiffusionTensor3DReconstructionImageFilterObj =
    itk::DiffusionTensor3DReconstructionImageFilter<float,double,float>::New();
  std::cout << "-------------DiffusionTensor3DReconstructionImageFilter" << DiffusionTensor3DReconstructionImageFilterObj;

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

  return EXIT_SUCCESS;
}

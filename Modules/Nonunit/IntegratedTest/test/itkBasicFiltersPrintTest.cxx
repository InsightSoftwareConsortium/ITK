/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
  node_type *  Next;
  node_type *  Previous;
};


int
itkBasicFiltersPrintTest(int, char *[])
{
  using InputType = itk::Image<float, 2>;
  using OutputType = itk::Image<float, 2>;
  using CharType = itk::Image<unsigned char, 2>;

  using VectorType = itk::Vector<float, 2>;
  using VectorImageType = itk::Image<VectorType, 2>;

  using CovariantVectorType = itk::CovariantVector<float, 2>;
  using CovariantVectorImageType = itk::Image<CovariantVectorType, 2>;

  // using KernelType = itk::Neighborhood<unsigned short,2>;
  using KernelType = itk::BinaryBallStructuringElement<unsigned short, 2>;


  const itk::AcosImageFilter<InputType, OutputType>::Pointer AcosImageFilterObj =
    itk::AcosImageFilter<InputType, OutputType>::New();
  std::cout << "-------------AcosImageFilter" << AcosImageFilterObj;

  const itk::AdaptiveHistogramEqualizationImageFilter<InputType>::Pointer AdaptiveHistogramEqualizationImageFilterObj =
    itk::AdaptiveHistogramEqualizationImageFilter<InputType>::New();
  std::cout << "-------------AdaptiveHistogramEqualizationImageFilter" << AdaptiveHistogramEqualizationImageFilterObj;

  const itk::AddImageFilter<InputType, InputType, OutputType>::Pointer AddImageFilterObj =
    itk::AddImageFilter<InputType, InputType, OutputType>::New();
  std::cout << "-------------AddImageFilter" << AddImageFilterObj;

  const itk::AsinImageFilter<InputType, OutputType>::Pointer AsinImageFilterObj =
    itk::AsinImageFilter<InputType, OutputType>::New();
  std::cout << "-------------AsinImageFilter" << AsinImageFilterObj;

  const itk::Atan2ImageFilter<InputType, InputType, OutputType>::Pointer Atan2ImageFilterObj =
    itk::Atan2ImageFilter<InputType, InputType, OutputType>::New();
  std::cout << "-------------Atan2ImageFilter" << Atan2ImageFilterObj;

  const itk::AtanImageFilter<InputType, OutputType>::Pointer AtanImageFilterObj =
    itk::AtanImageFilter<InputType, OutputType>::New();
  std::cout << "-------------AtanImageFilter" << AtanImageFilterObj;

  const itk::BSplineDecompositionImageFilter<InputType, OutputType>::Pointer BSplineDecompositionImageFilterObj =
    itk::BSplineDecompositionImageFilter<InputType, OutputType>::New();
  std::cout << "-------------BSplineDecompositionImageFilter" << BSplineDecompositionImageFilterObj;

  const itk::BSplineDownsampleImageFilter<InputType, OutputType>::Pointer BSplineDownsampleImageFilterObj =
    itk::BSplineDownsampleImageFilter<InputType, OutputType>::New();
  std::cout << "-------------BSplineDownsampleImageFilter" << BSplineDownsampleImageFilterObj;

  const itk::BSplineInterpolateImageFunction<InputType>::Pointer BSplineInterpolateImageFunctionObj =
    itk::BSplineInterpolateImageFunction<InputType>::New();
  std::cout << "-------------BSplineInterpolateImageFunction" << BSplineInterpolateImageFunctionObj;

  const itk::BSplineResampleImageFunction<InputType>::Pointer BSplineResampleImageFunctionObj =
    itk::BSplineResampleImageFunction<InputType>::New();
  std::cout << "-------------BSplineResampleImageFunction" << BSplineResampleImageFunctionObj;

  const itk::BSplineUpsampleImageFilter<InputType, OutputType>::Pointer BSplineUpsampleImageFilterObj =
    itk::BSplineUpsampleImageFilter<InputType, OutputType>::New();
  std::cout << "-------------BSplineUpsampleImageFilter" << BSplineUpsampleImageFilterObj;

  const itk::BilateralImageFilter<InputType, OutputType>::Pointer BilateralImageFilterObj =
    itk::BilateralImageFilter<InputType, OutputType>::New();
  std::cout << "-------------BilateralImageFilter" << BilateralImageFilterObj;

  const itk::BinaryDilateImageFilter<InputType, OutputType, KernelType>::Pointer BinaryDilateImageFilterObj =
    itk::BinaryDilateImageFilter<InputType, OutputType, KernelType>::New();
  std::cout << "-------------BinaryDilateImageFilter" << BinaryDilateImageFilterObj;

  const itk::BinaryErodeImageFilter<InputType, OutputType, KernelType>::Pointer BinaryErodeImageFilterObj =
    itk::BinaryErodeImageFilter<InputType, OutputType, KernelType>::New();
  std::cout << "-------------BinaryErodeImageFilter" << BinaryErodeImageFilterObj;

  const itk::BinaryMagnitudeImageFilter<InputType, InputType, OutputType>::Pointer BinaryMagnitudeImageFilterObj =
    itk::BinaryMagnitudeImageFilter<InputType, InputType, OutputType>::New();
  std::cout << "-------------BinaryMagnitudeImageFilter" << BinaryMagnitudeImageFilterObj;

  const itk::BinaryMedianImageFilter<InputType, OutputType>::Pointer BinaryMedianImageFilterObj =
    itk::BinaryMedianImageFilter<InputType, OutputType>::New();
  std::cout << "-------------BinaryMedianImageFilter" << BinaryMedianImageFilterObj;

  const itk::BinaryThresholdImageFilter<InputType, OutputType>::Pointer BinaryThresholdImageFilterObj =
    itk::BinaryThresholdImageFilter<InputType, OutputType>::New();
  std::cout << "-------------BinaryThresholdImageFilter" << BinaryThresholdImageFilterObj;

  const itk::BinomialBlurImageFilter<InputType, OutputType>::Pointer BinomialBlurImageFilterObj =
    itk::BinomialBlurImageFilter<InputType, OutputType>::New();
  std::cout << "-------------BinomialBlurImageFilter" << BinomialBlurImageFilterObj;

  const itk::CannyEdgeDetectionImageFilter<InputType, OutputType>::Pointer CannyEdgeDetectionImageFilterObj =
    itk::CannyEdgeDetectionImageFilter<InputType, OutputType>::New();
  std::cout << "-------------CannyEdgeDetectionImageFilter" << CannyEdgeDetectionImageFilterObj;

  const itk::CastImageFilter<InputType, OutputType>::Pointer CastImageFilterObj =
    itk::CastImageFilter<InputType, OutputType>::New();
  std::cout << "-------------CastImageFilter" << CastImageFilterObj;

  const itk::ChangeInformationImageFilter<InputType>::Pointer ChangeInformationImageFilterObj =
    itk::ChangeInformationImageFilter<InputType>::New();
  std::cout << "-------------ChangeInformationImageFilter" << ChangeInformationImageFilterObj;

  const itk::ComposeImageFilter<InputType>::Pointer ComposeImageFilterObj = itk::ComposeImageFilter<InputType>::New();
  std::cout << "-------------ComposeImageFilter" << ComposeImageFilterObj;

  const itk::ConfidenceConnectedImageFilter<InputType, OutputType>::Pointer ConfidenceConnectedImageFilterObj =
    itk::ConfidenceConnectedImageFilter<InputType, OutputType>::New();
  std::cout << "-------------ConfidenceConnectedImageFilter" << ConfidenceConnectedImageFilterObj;

  const itk::ConnectedThresholdImageFilter<InputType, OutputType>::Pointer ConnectedThresholdImageFilterObj =
    itk::ConnectedThresholdImageFilter<InputType, OutputType>::New();
  std::cout << "-------------ConnectedThresholdImageFilter" << ConnectedThresholdImageFilterObj;

  const itk::ConstantPadImageFilter<InputType, OutputType>::Pointer ConstantPadImageFilterObj =
    itk::ConstantPadImageFilter<InputType, OutputType>::New();
  std::cout << "-------------ConstantPadImageFilter" << ConstantPadImageFilterObj;

  const itk::CosImageFilter<InputType, OutputType>::Pointer CosImageFilterObj =
    itk::CosImageFilter<InputType, OutputType>::New();
  std::cout << "-------------CosImageFilter" << CosImageFilterObj;

  const itk::CropImageFilter<InputType, OutputType>::Pointer CropImageFilterObj =
    itk::CropImageFilter<InputType, OutputType>::New();
  std::cout << "-------------CropImageFilter" << CropImageFilterObj;

  const itk::CurvatureAnisotropicDiffusionImageFilter<InputType, OutputType>::Pointer
    CurvatureAnisotropicDiffusionImageFilterObj =
      itk::CurvatureAnisotropicDiffusionImageFilter<InputType, OutputType>::New();
  std::cout << "-------------CurvatureAnisotropicDiffusionImageFilter" << CurvatureAnisotropicDiffusionImageFilterObj;

  const itk::CurvatureNDAnisotropicDiffusionFunction<InputType>::Pointer CurvatureNDAnisotropicDiffusionFunctionObj =
    itk::CurvatureNDAnisotropicDiffusionFunction<InputType>::New();
  std::cout << "-------------CurvatureNDAnisotropicDiffusionFunction" << CurvatureNDAnisotropicDiffusionFunctionObj;

  const itk::DanielssonDistanceMapImageFilter<InputType, OutputType>::Pointer DanielssonDistanceMapImageFilterObj =
    itk::DanielssonDistanceMapImageFilter<InputType, OutputType>::New();
  std::cout << "-------------DanielssonDistanceMapImageFilter" << DanielssonDistanceMapImageFilterObj;

  const itk::SignedDanielssonDistanceMapImageFilter<InputType, OutputType>::Pointer
    SignedDanielssonDistanceMapImageFilterObj =
      itk::SignedDanielssonDistanceMapImageFilter<InputType, OutputType>::New();
  std::cout << "-------------SignedDanielssonDistanceMapImageFilter" << SignedDanielssonDistanceMapImageFilterObj;

  const itk::DerivativeImageFilter<InputType, OutputType>::Pointer DerivativeImageFilterObj =
    itk::DerivativeImageFilter<InputType, OutputType>::New();
  std::cout << "-------------DerivativeImageFilter" << DerivativeImageFilterObj;

  const itk::DifferenceOfGaussiansGradientImageFilter<InputType, float>::Pointer
    DifferenceOfGaussiansGradientImageFilterObj =
      itk::DifferenceOfGaussiansGradientImageFilter<InputType, float>::New();
  std::cout << "-------------DifferenceOfGaussiansGradientImageFilter" << DifferenceOfGaussiansGradientImageFilterObj;

  const itk::DiffusionTensor3DReconstructionImageFilter<float, double, float>::Pointer
    DiffusionTensor3DReconstructionImageFilterObj =
      itk::DiffusionTensor3DReconstructionImageFilter<float, double, float>::New();
  std::cout << "-------------DiffusionTensor3DReconstructionImageFilter"
            << DiffusionTensor3DReconstructionImageFilterObj;

  const itk::DilateObjectMorphologyImageFilter<InputType, OutputType, KernelType>::Pointer
    DilateObjectMorphologyImageFilterObj =
      itk::DilateObjectMorphologyImageFilter<InputType, OutputType, KernelType>::New();
  std::cout << "-------------DilateObjectMorphologyImageFilter" << DilateObjectMorphologyImageFilterObj;

  const itk::DirectedHausdorffDistanceImageFilter<InputType, OutputType>::Pointer
    DirectedHausdorffDistanceImageFilterObj = itk::DirectedHausdorffDistanceImageFilter<InputType, OutputType>::New();
  std::cout << "-------------DirectedHausdorffDistanceImageFilter" << DirectedHausdorffDistanceImageFilterObj;

  const itk::DiscreteGaussianImageFilter<InputType, OutputType>::Pointer DiscreteGaussianImageFilterObj =
    itk::DiscreteGaussianImageFilter<InputType, OutputType>::New();
  std::cout << "-------------DiscreteGaussianImageFilter" << DiscreteGaussianImageFilterObj;

  const itk::DivideImageFilter<InputType, InputType, OutputType>::Pointer DivideImageFilterObj =
    itk::DivideImageFilter<InputType, InputType, OutputType>::New();
  std::cout << "-------------DivideImageFilter" << DivideImageFilterObj;

  const itk::EdgePotentialImageFilter<CovariantVectorImageType, OutputType>::Pointer EdgePotentialImageFilterObj =
    itk::EdgePotentialImageFilter<CovariantVectorImageType, OutputType>::New();
  std::cout << "-------------EdgePotentialImageFilter" << EdgePotentialImageFilterObj;

  const itk::EigenAnalysis2DImageFilter<InputType, InputType, VectorImageType>::Pointer EigenAnalysis2DImageFilterObj =
    itk::EigenAnalysis2DImageFilter<InputType, InputType, VectorImageType>::New();
  std::cout << "-------------EigenAnalysis2DImageFilter" << EigenAnalysis2DImageFilterObj;

  const itk::ErodeObjectMorphologyImageFilter<InputType, OutputType, KernelType>::Pointer
    ErodeObjectMorphologyImageFilterObj =
      itk::ErodeObjectMorphologyImageFilter<InputType, OutputType, KernelType>::New();
  std::cout << "-------------ErodeObjectMorphologyImageFilter" << ErodeObjectMorphologyImageFilterObj;

  const itk::ExpImageFilter<InputType, OutputType>::Pointer ExpImageFilterObj =
    itk::ExpImageFilter<InputType, OutputType>::New();
  std::cout << "-------------ExpImageFilter" << ExpImageFilterObj;

  const itk::ExpNegativeImageFilter<InputType, OutputType>::Pointer ExpNegativeImageFilterObj =
    itk::ExpNegativeImageFilter<InputType, OutputType>::New();
  std::cout << "-------------ExpNegativeImageFilter" << ExpNegativeImageFilterObj;

  const itk::ExpandImageFilter<InputType, OutputType>::Pointer ExpandImageFilterObj =
    itk::ExpandImageFilter<InputType, OutputType>::New();
  std::cout << "-------------ExpandImageFilter" << ExpandImageFilterObj;

  const itk::ExtractImageFilter<InputType, OutputType>::Pointer ExtractImageFilterObj =
    itk::ExtractImageFilter<InputType, OutputType>::New();
  std::cout << "-------------ExtractImageFilter" << ExtractImageFilterObj;

  const itk::FlipImageFilter<InputType>::Pointer FlipImageFilterObj = itk::FlipImageFilter<InputType>::New();
  std::cout << "-------------FlipImageFilter" << FlipImageFilterObj;

  const itk::GaussianImageSource<OutputType>::Pointer GaussianImageSourceObj =
    itk::GaussianImageSource<OutputType>::New();
  std::cout << "-------------GaussianImageSource" << GaussianImageSourceObj;

  const itk::GradientAnisotropicDiffusionImageFilter<InputType, OutputType>::Pointer
    GradientAnisotropicDiffusionImageFilterObj =
      itk::GradientAnisotropicDiffusionImageFilter<InputType, OutputType>::New();
  std::cout << "-------------GradientAnisotropicDiffusionImageFilter" << GradientAnisotropicDiffusionImageFilterObj;

  const itk::GradientImageFilter<InputType>::Pointer GradientImageFilterObj =
    itk::GradientImageFilter<InputType>::New();
  std::cout << "-------------GradientImageFilter" << GradientImageFilterObj;

  const itk::GradientMagnitudeImageFilter<InputType, OutputType>::Pointer GradientMagnitudeImageFilterObj =
    itk::GradientMagnitudeImageFilter<InputType, OutputType>::New();
  std::cout << "-------------GradientMagnitudeImageFilter" << GradientMagnitudeImageFilterObj;

  const itk::GradientMagnitudeRecursiveGaussianImageFilter<InputType>::Pointer
    GradientMagnitudeRecursiveGaussianImageFilterObj =
      itk::GradientMagnitudeRecursiveGaussianImageFilter<InputType>::New();
  std::cout << "-------------GradientMagnitudeRecursiveGaussianImageFilter"
            << GradientMagnitudeRecursiveGaussianImageFilterObj;

  const itk::GradientNDAnisotropicDiffusionFunction<InputType>::Pointer GradientNDAnisotropicDiffusionFunctionObj =
    itk::GradientNDAnisotropicDiffusionFunction<InputType>::New();
  std::cout << "-------------GradientNDAnisotropicDiffusionFunction" << GradientNDAnisotropicDiffusionFunctionObj;

  const itk::GradientRecursiveGaussianImageFilter<InputType>::Pointer GradientRecursiveGaussianImageFilterObj =
    itk::GradientRecursiveGaussianImageFilter<InputType>::New();
  std::cout << "-------------GradientRecursiveGaussianImageFilter" << GradientRecursiveGaussianImageFilterObj;

  const itk::GrayscaleDilateImageFilter<InputType, OutputType, KernelType>::Pointer GrayscaleDilateImageFilterObj =
    itk::GrayscaleDilateImageFilter<InputType, OutputType, KernelType>::New();
  std::cout << "-------------GrayscaleDilateImageFilter" << GrayscaleDilateImageFilterObj;

  const itk::GrayscaleErodeImageFilter<InputType, OutputType, KernelType>::Pointer GrayscaleErodeImageFilterObj =
    itk::GrayscaleErodeImageFilter<InputType, OutputType, KernelType>::New();
  std::cout << "-------------GrayscaleErodeImageFilter" << GrayscaleErodeImageFilterObj;

  const itk::GrayscaleFunctionDilateImageFilter<InputType, OutputType, KernelType>::Pointer
    GrayscaleFunctionDilateImageFilterObj =
      itk::GrayscaleFunctionDilateImageFilter<InputType, OutputType, KernelType>::New();
  std::cout << "-------------GrayscaleFunctionDilateImageFilter" << GrayscaleFunctionDilateImageFilterObj;

  const itk::GrayscaleFunctionErodeImageFilter<InputType, OutputType, KernelType>::Pointer
    GrayscaleFunctionErodeImageFilterObj =
      itk::GrayscaleFunctionErodeImageFilter<InputType, OutputType, KernelType>::New();
  std::cout << "-------------GrayscaleFunctionErodeImageFilter" << GrayscaleFunctionErodeImageFilterObj;

  const itk::HardConnectedComponentImageFilter<InputType, CharType>::Pointer HardConnectedComponentImageFilterObj =
    itk::HardConnectedComponentImageFilter<InputType, CharType>::New();
  std::cout << "-------------HardConnectedComponentImageFilter" << HardConnectedComponentImageFilterObj;

  const itk::HausdorffDistanceImageFilter<InputType, OutputType>::Pointer HausdorffDistanceImageFilterObj =
    itk::HausdorffDistanceImageFilter<InputType, OutputType>::New();
  std::cout << "-------------HausdorffDistanceImageFilter" << HausdorffDistanceImageFilterObj;

  return EXIT_SUCCESS;
}

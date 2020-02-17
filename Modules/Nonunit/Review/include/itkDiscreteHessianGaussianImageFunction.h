/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkDiscreteHessianGaussianImageFunction_h
#define itkDiscreteHessianGaussianImageFunction_h

#include "itkNeighborhoodOperatorImageFunction.h"
#include "itkGaussianDerivativeOperator.h"
#include "itkSymmetricSecondRankTensor.h"

namespace itk
{
/**
 * \class DiscreteHessianGaussianImageFunction
 * \brief Compute the Hessian Gaussian of an image at a specific location in space
          by calculating discrete second-order gaussian derivatives.
 * This class is templated over the input image type.
 *
 * The Initialize() method must be called after setting the parameters and before
 * evaluating the function.
 *
 * \author Ivan Macia, VICOMTech, Spain, http://www.vicomtech.es
 *
 * This implementation was taken from the Insight Journal paper:
 * https://hdl.handle.net/1926/1290
 *
 * \sa NeighborhoodOperator
 * \sa ImageFunction
 * \ingroup ITKReview
 */
template <typename TInputImage, typename TOutput = double>
class ITK_TEMPLATE_EXPORT DiscreteHessianGaussianImageFunction
  : public ImageFunction<TInputImage, SymmetricSecondRankTensor<TOutput, TInputImage::ImageDimension>, TOutput>
{
public:
  /**Standard "Self" type alias */
  using Self = DiscreteHessianGaussianImageFunction;

  /** Standard "Superclass" type alias */
  using Superclass =
    ImageFunction<TInputImage, SymmetricSecondRankTensor<TOutput, TInputImage::ImageDimension>, TOutput>;

  /** Smart pointer type alias support */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(DiscreteHessianGaussianImageFunction, ImageFunction);

  /** Image dependent types */
  using InputImageType = typename Superclass::InputImageType;
  using InputPixelType = typename Superclass::InputPixelType;
  using IndexType = typename Superclass::IndexType;
  using IndexValueType = typename Superclass::IndexValueType;
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;
  using PointType = typename Superclass::PointType;

  /** Dimension of the underlying image */
  static constexpr unsigned int ImageDimension2 = InputImageType::ImageDimension;

  /** Output type */
  using TensorType = SymmetricSecondRankTensor<TOutput, TInputImage::ImageDimension>;
  using OutputType = typename Superclass::OutputType;

  using VarianceArrayType = FixedArray<double, Self::ImageDimension2>;

  using GaussianDerivativeOperatorType = itk::GaussianDerivativeOperator<TOutput, Self::ImageDimension2>;

  /** Array to store gaussian derivative operators from zero to second order
   * (3*ImageDimension operators) */
  using GaussianDerivativeOperatorArrayType = FixedArray<GaussianDerivativeOperatorType, 3 * Self::ImageDimension2>;

  using KernelType = Neighborhood<TOutput, Self::ImageDimension2>;

  /** Array to store precomputed N-dimensional kernels for the hessian
   * components  */
  using KernelArrayType = FixedArray<KernelType, Self::ImageDimension2 *(Self::ImageDimension2 + 1) / 2>;

  /** Image function that performs convolution with the neighborhood
   * operator  */
  using OperatorImageFunctionType = NeighborhoodOperatorImageFunction<InputImageType, TOutput>;
  using OperatorImageFunctionPointer = typename OperatorImageFunctionType::Pointer;

  using InterpolationModeEnum = itk::GaussianDerivativeOperatorEnums::InterpolationMode;
#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr InterpolationModeEnum NearestNeighbourInterpolation =
    InterpolationModeEnum::NearestNeighbourInterpolation;
  static constexpr InterpolationModeEnum LinearInterpolation = InterpolationModeEnum::LinearInterpolation;
#endif

public:
  /** Evalutate the  in the given dimension at specified point */
  OutputType
  Evaluate(const PointType & point) const override;

  /** Evaluate the function at specified Index position */
  OutputType
  EvaluateAtIndex(const IndexType & index) const override;

  /** Evaluate the function at specified ContinuousIndex position */
  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType & index) const override;

  /** Set/Get the variance for the discrete Gaussian kernel.
   * Sets the variance for individual dimensions. The default is 0.0 in each dimension.
   * If UseImageSpacing is true, the units are the physical units of your image.
   * If UseImageSpacing is false then the units are pixels. */
  itkSetMacro(Variance, VarianceArrayType);
  itkGetConstMacro(Variance, const VarianceArrayType);
  itkSetVectorMacro(Variance, double, VarianceArrayType::Length);

  /** Convenience method for setting the variance for all dimensions */
  virtual void
  SetVariance(double variance)
  {
    m_Variance.Fill(variance);
    this->Modified();
  }

  /** Convenience method for setting the variance through the standard deviation
   */
  void
  SetSigma(const double sigma)
  {
    SetVariance(sigma * sigma);
  }

  /** Set/Get the desired maximum error of the gaussian approximation.  Maximum
   * error is the difference between the area under the discrete Gaussian curve
   * and the area under the continuous Gaussian. Maximum error affects the
   * Gaussian operator size. The value is clamped between 0.00001 and
   * 0.99999. */
  itkSetClampMacro(MaximumError, double, 0.00001, 0.99999);
  itkGetConstMacro(MaximumError, double);

  /** Set/Get the flag for calculating scale-space normalized derivatives.
   * Normalized derivatives are obtained multiplying by the scale
   * parameter t. */
  itkSetMacro(NormalizeAcrossScale, bool);
  itkGetConstMacro(NormalizeAcrossScale, bool);
  itkBooleanMacro(NormalizeAcrossScale);

  /** Set/Get the flag for using image spacing when calculating derivatives. */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);

  /** Set/Get a limit for growth of the kernel. Small maximum error values with
   *  large variances will yield very large kernel sizes. This value can be
   *  used to truncate a kernel in such instances. A warning will be given on
   *  truncation of the kernel. */
  itkSetMacro(MaximumKernelWidth, unsigned int);
  itkGetConstMacro(MaximumKernelWidth, unsigned int);

  /** Set/Get the interpolation mode. */
  itkSetEnumMacro(InterpolationMode, InterpolationModeEnum);
  itkGetEnumMacro(InterpolationMode, InterpolationModeEnum);

  /** Set the input image.
   * \warning this method caches BufferedRegion information.
   * If the BufferedRegion has changed, user must call
   * SetInputImage again to update cached values. */
  void
  SetInputImage(const InputImageType * ptr) override;

  /** Initialize the Gaussian kernel. Call this method before evaluating the function.
   * This method MUST be called after any changes to function parameters. */
  virtual void
  Initialize()
  {
    RecomputeGaussianKernel();
  }

protected:
  DiscreteHessianGaussianImageFunction();
  DiscreteHessianGaussianImageFunction(const Self &) {}

  ~DiscreteHessianGaussianImageFunction() override = default;

  void
  operator=(const Self &)
  {}
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  RecomputeGaussianKernel();

private:
  /** Desired variance of the discrete Gaussian function */
  VarianceArrayType m_Variance;

  /** Difference between the areas under the curves of the continuous and
   * discrete Gaussian functions */
  double m_MaximumError{ 0.005 };

  /** Maximum kernel size allowed.  This value is used to truncate a kernel
   *  that has grown too large.  A warning is given when the specified maximum
   *  error causes the kernel to exceed this size */
  unsigned int m_MaximumKernelWidth{ 30 };

  /** Array of derivative operators, one for each dimension and order.
   * First N zero-order operators are stored, then N first-order and
   * then N second-order making 3*N operators altogether where
   * N=ImageDimension. */
  mutable GaussianDerivativeOperatorArrayType m_OperatorArray;

  /** Array of N-dimensional kernels which are the result of
   * convolving the operators for calculating hessian matrix
   * derivatives */
  KernelArrayType m_KernelArray;

  /** OperatorImageFunction */
  OperatorImageFunctionPointer m_OperatorImageFunction;

  /** Flag for scale-space normalization of derivatives */
  bool m_NormalizeAcrossScale{ true };

  /** Flag to indicate whether to use image spacing */
  bool m_UseImageSpacing{ true };

  /** Interpolation mode */
  InterpolationModeEnum m_InterpolationMode{ InterpolationModeEnum::NearestNeighbourInterpolation };
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDiscreteHessianGaussianImageFunction.hxx"
#endif

#endif

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
#ifndef itkDiscreteGradientMagnitudeGaussianImageFunction_h
#define itkDiscreteGradientMagnitudeGaussianImageFunction_h

#include "itkNeighborhoodOperatorImageFunction.h"
#include "itkGaussianDerivativeOperator.h"

namespace itk
{
/**
 * \class DiscreteGradientMagnitudeGaussianImageFunction
 * \brief Compute the discrete gradient magnitude gaussian of an the image
 *        at a specific location in space, i.e. point, index or continuous
 *        index. This class computes a single derivative given the order in
 *        each direction (by default zero).
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
template< typename TInputImage, typename TOutput = double >
class ITK_TEMPLATE_EXPORT DiscreteGradientMagnitudeGaussianImageFunction:
  public ImageFunction< TInputImage, TOutput, TOutput >
{
public:

  /**Standard "Self" typedef */
  typedef DiscreteGradientMagnitudeGaussianImageFunction Self;

  /** Standard "Superclass" typedef */
  typedef ImageFunction< TInputImage, TOutput, TOutput > Superclass;

  /** Smart pointer typedef support */
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(DiscreteGradientMagnitudeGaussianImageFunction, ImageFunction);

  /** Image dependent types */
  typedef typename Superclass::InputImageType      InputImageType;
  typedef typename Superclass::InputPixelType      InputPixelType;
  typedef typename Superclass::IndexType           IndexType;
  typedef typename Superclass::IndexValueType      IndexValueType;
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;
  typedef typename Superclass::PointType           PointType;

  /** Dimension of the underlying image */
  itkStaticConstMacro(ImageDimension2, unsigned int,
                      InputImageType::ImageDimension);

  /** Output type */
  typedef typename Superclass::OutputType OutputType;

  /** Arrays for native types */
  typedef FixedArray< double, itkGetStaticConstMacro(ImageDimension2) >       VarianceArrayType;
  typedef FixedArray< unsigned int, itkGetStaticConstMacro(ImageDimension2) > OrderArrayType;

  typedef itk::GaussianDerivativeOperator< TOutput,
                                           itkGetStaticConstMacro(ImageDimension2) >    GaussianDerivativeOperatorType;

  /** Array to store gaussian derivative operators one for each dimension */
  typedef FixedArray< GaussianDerivativeOperatorType,
                      2 *itkGetStaticConstMacro(ImageDimension2) >            GaussianDerivativeOperatorArrayType;

  /** Precomputed N-dimensional derivative kernel */
  typedef Neighborhood< TOutput, itkGetStaticConstMacro(ImageDimension2) > KernelType;

  /** Array to store precomputed N-dimensional kernels for the gradient
    components */
  typedef FixedArray< KernelType, itkGetStaticConstMacro(ImageDimension2) > KernelArrayType;

  /** Image function that performs convolution with the neighborhood operator */
  typedef NeighborhoodOperatorImageFunction
  < InputImageType, TOutput >                           OperatorImageFunctionType;
  typedef typename OperatorImageFunctionType::Pointer OperatorImageFunctionPointer;

  /** Interpolation modes */
  enum InterpolationModeType { NearestNeighbourInterpolation, LinearInterpolation };

public:

  /** Evaluate the function in the given dimension at specified point */
  OutputType Evaluate(const PointType & point) const ITK_OVERRIDE;

  /** Evaluate the function at specified Index position */
  OutputType EvaluateAtIndex(const IndexType & index) const ITK_OVERRIDE;

  /** Evaluate the function at specified ContinuousIndex position */
  OutputType EvaluateAtContinuousIndex(
    const ContinuousIndexType & index) const ITK_OVERRIDE;

  /** Set/Get the variance for the discrete Gaussian kernel.
   * Sets the variance for individual dimensions. The default is 0.0 in each dimension.
   * If UseImageSpacing is true, the units are the physical units of your image.
   * If UseImageSpacing is false then the units are pixels */
  itkSetMacro(Variance, VarianceArrayType);
  itkGetConstMacro(Variance, const VarianceArrayType);
  itkSetVectorMacro(Variance, double, VarianceArrayType::Length);

  /** Convenience method for setting the variance for all dimensions */
  virtual void SetVariance(double variance)
  {
    m_Variance.Fill(variance);
    this->Modified();
  }

  /** Convenience method for setting the variance through the standard deviation
    */
  void SetSigma(const double sigma)
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
  itkSetMacro(InterpolationMode, InterpolationModeType);
  itkGetConstMacro(InterpolationMode, InterpolationModeType);

  /** Set the input image.
   * \warning this method caches BufferedRegion information.
   * If the BufferedRegion has changed, user must call
   * SetInputImage again to update cached values. */
  void SetInputImage(const InputImageType *ptr) ITK_OVERRIDE;

  /** Initialize the Gaussian kernel. Call this method before evaluating the function.
   * This method MUST be called after any changes to function parameters. */
  virtual void Initialize() { RecomputeGaussianKernel(); }

protected:

  DiscreteGradientMagnitudeGaussianImageFunction();
  DiscreteGradientMagnitudeGaussianImageFunction(const Self &){}

  ~DiscreteGradientMagnitudeGaussianImageFunction() ITK_OVERRIDE {}

  void operator=(const Self &){}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void RecomputeGaussianKernel();

  // void RecomputeContinuousGaussianKernel(
  //        const double* offset) const;

private:

  /** Desired variance of the discrete Gaussian function */
  VarianceArrayType m_Variance;

  /** Difference between the areas under the curves of the continuous and
   * discrete Gaussian functions */
  double m_MaximumError;

  /** Maximum kernel size allowed.  This value is used to truncate a kernel
   *  that has grown too large.  A warning is given when the specified maximum
   *  error causes the kernel to exceed this size */
  unsigned int m_MaximumKernelWidth;

  /** Array of derivative operators, one for each dimension and order.
    * First N zero-rder operators are stored, then N first-order making
    * 2*N operators altogether where N=ImageDimension */
  GaussianDerivativeOperatorArrayType m_OperatorArray;

  /** Array of N-dimensional kernels used to calculate gradient components */
  KernelArrayType m_KernelArray;

  /** OperatorImageFunction */
  OperatorImageFunctionPointer m_OperatorImageFunction;

  /** Flag for scale-space normalization of derivatives */
  bool m_NormalizeAcrossScale;

  /** Flag to indicate whether to use image spacing */
  bool m_UseImageSpacing;

  /** Interpolation mode */
  InterpolationModeType m_InterpolationMode;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDiscreteGradientMagnitudeGaussianImageFunction.hxx"
#endif

#endif

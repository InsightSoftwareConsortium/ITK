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
#ifndef itkGaussianBlurImageFunction_h
#define itkGaussianBlurImageFunction_h

#include "itkNeighborhoodOperatorImageFunction.h"
#include "itkGaussianOperator.h"
#include "itkGaussianSpatialFunction.h"

namespace itk
{
/**
 * \class GaussianBlurImageFunction
 * \brief Compute the convolution of a neighborhood operator with the image
 *        at a specific location in space, i.e. point, index or continuous
 *        index.
 * This class is templated over the input image type.
 * \sa NeighborhoodOperator
 * \sa ImageFunction
 * \ingroup ITKImageFunction
 *
 * \sphinx
 * \sphinxexample{Core/ImageFunction/GaussianBlueImageFunction,GaussianBlurImageFunction}
 * \endsphinx
 */
template <typename TInputImage, typename TOutput = double>
class ITK_TEMPLATE_EXPORT GaussianBlurImageFunction : public ImageFunction<TInputImage, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(GaussianBlurImageFunction);

  /**Standard "Self" type alias */
  using Self = GaussianBlurImageFunction;

  /** Standard "Superclass" type alias */
  using Superclass = ImageFunction<TInputImage, TOutput>;

  /** Smart pointer type alias support */
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianBlurImageFunction, ImageFunction);

  /** InputImageType type alias support */
  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using IndexType = typename Superclass::IndexType;
  using ContinuousIndexType = typename Superclass::ContinuousIndexType;

  /** Dimension of the underlying image. */
  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  using GaussianOperatorType = GaussianOperator<TOutput, Self::ImageDimension>;
  using NeighborhoodType = Neighborhood<TOutput, Self::ImageDimension>;
  using OperatorArrayType = FixedArray<NeighborhoodType, Self::ImageDimension>;

  using GaussianFunctionType = GaussianSpatialFunction<TOutput, 1>;
  using GaussianFunctionPointer = typename GaussianFunctionType::Pointer;
  using InputPixelRealType = typename NumericTraits<InputPixelType>::RealType;
  using InternalImageType = itk::Image<InputPixelRealType, Self::ImageDimension>;
  using InternalImagePointer = typename InternalImageType::Pointer;

  using OperatorImageFunctionType = NeighborhoodOperatorImageFunction<InputImageType, TOutput>;
  using OperatorImageFunctionPointer = typename OperatorImageFunctionType::Pointer;

  using OperatorInternalImageFunctionType = NeighborhoodOperatorImageFunction<InternalImageType, TOutput>;
  using OperatorInternalImageFunctionPointer = typename OperatorInternalImageFunctionType::Pointer;

  using ErrorArrayType = itk::FixedArray<double, Self::ImageDimension>;
  using ExtentArrayType = itk::FixedArray<double, Self::ImageDimension>;
  using SigmaArrayType = itk::FixedArray<double, Self::ImageDimension>;

  /** Point type alias support */
  using PointType = typename Superclass::PointType;

  /** Evalutate the  in the given dimension at specified point */
  TOutput
  Evaluate(const PointType & point) const override;

  /** Evaluate the function at specified Index position */
  TOutput
  EvaluateAtIndex(const IndexType & index) const override;

  /** Evaluate the function at specified ContinuousIndex position. */
  TOutput
  EvaluateAtContinuousIndex(const ContinuousIndexType & index) const override;

  /** The standard deviation for the discrete Gaussian kernel.  Sets the
   * standard deviation independently for each dimension.
   * The default is 1.0 in each dimension.
   * If UseImageSpacing is true (default), the units are the physical units
   * of your image.  If UseImageSpacing is false then the units are pixels.
   */
  void
  SetSigma(const double * sigma);

  void
  SetSigma(const float * sigma);

  void
  SetSigma(const double sigma);

  itkSetMacro(Sigma, SigmaArrayType);
  itkGetConstReferenceMacro(Sigma, SigmaArrayType);

  /** Set the input image.
   * \warning this method caches BufferedRegion information.
   * If the BufferedRegion has changed, user must call
   * SetInputImage again to update cached values. */
  void
  SetInputImage(const InputImageType * ptr) override;

  /** Set/Get the Extent of the array holding the coefficients
   *  of the Gaussian kernel computed by the GaussianOperator.
   */
  itkSetMacro(Extent, ExtentArrayType);
  itkGetConstReferenceMacro(Extent, ExtentArrayType);
  void
  SetExtent(const double * extent);

  void
  SetExtent(const double extent);

  /** Set/Get the maximum error acceptable for the approximation
   *  of the Gaussian kernel with the GaussianOperator.
   */
  itkSetMacro(MaximumError, ErrorArrayType);
  itkGetConstReferenceMacro(MaximumError, ErrorArrayType);

  /** Set/GetMaximumKernelWidth() This value is used by the underling
   *  GaussianOperator for computing the number of coefficients to be
   *  used in the Gaussian kernel
   */
  itkSetMacro(MaximumKernelWidth, int);
  itkGetConstMacro(MaximumKernelWidth, int);

  /** Set/GetUseImageSpacing() This flag is used by the underling
   *  GaussianOperator to decide if the image spacing should be used
   *  to scale the value of sigma or not. The methods UseImageSpacingOn()
   *  and UseImageSpacingOff() provide a similar functionality.
   */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);

protected:
  GaussianBlurImageFunction();
  ~GaussianBlurImageFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  RecomputeGaussianKernel();

  void
  RecomputeContinuousGaussianKernel(const double * offset) const;

private:
  virtual TOutput
  EvaluateAtIndex(const IndexType & index, const OperatorArrayType & operatorArray) const;

  SigmaArrayType                       m_Sigma;
  OperatorImageFunctionPointer         m_OperatorImageFunction;
  OperatorInternalImageFunctionPointer m_OperatorInternalImageFunction;
  mutable OperatorArrayType            m_OperatorArray;
  mutable OperatorArrayType            m_ContinuousOperatorArray;
  InternalImagePointer                 m_InternalImage;

  /** The maximum error of the gaussian blurring kernel in each dimensional
   * direction. For definition of maximum error, see GaussianOperator.
   * \sa GaussianOperator */
  ErrorArrayType  m_MaximumError;
  ExtentArrayType m_Extent;

  /** Maximum allowed kernel width for any dimension of the discrete Gaussian
      approximation */
  int m_MaximumKernelWidth;

  /** Number of dimensions to process. Default is all dimensions */
  unsigned int m_FilterDimensionality;

  /** Flag to indicate whether to use image spacing */
  bool m_UseImageSpacing;

  /** Neighborhood Image Function */
  GaussianFunctionPointer m_GaussianFunction;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGaussianBlurImageFunction.hxx"
#endif

#endif

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
#ifndef itkGaussianDerivativeImageFunction_h
#define itkGaussianDerivativeImageFunction_h

#include "itkContinuousIndex.h"
#include "itkFixedArray.h"
#include "itkGaussianDerivativeSpatialFunction.h"
#include "itkGaussianSpatialFunction.h"
#include "itkImage.h"
#include "itkImageFunction.h"
#include "itkNeighborhood.h"
#include "itkOffset.h"
#include "itkVector.h"

#include <vector>

namespace itk
{
/**
 * \class GaussianDerivativeImageFunction
 * \brief Compute the Gaussian derivatives of an the image
 *        at a specific location in space, i.e. point, index or continuous
 *        index.
 * \note From ITK 5, the Evaluate member functions of this class are concurrent
 * thread safe: It is safe to have multiple simultaneous Evaluate calls on a
 * GaussianDerivativeImageFunction object.
 *
 * This class is templated over the input image type.
 * \sa NeighborhoodOperator
 * \sa ImageFunction
 * \ingroup ITKImageFunction
 */
template <typename TInputImage, typename TOutput = double>
class ITK_TEMPLATE_EXPORT GaussianDerivativeImageFunction
  : public ImageFunction<TInputImage, Vector<TOutput, TInputImage::ImageDimension>, TOutput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GaussianDerivativeImageFunction);

  /** Standard class type aliases. */
  using Self = GaussianDerivativeImageFunction;
  using Superclass = ImageFunction<TInputImage, Vector<TOutput, TInputImage::ImageDimension>, TOutput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianDerivativeImageFunction, ImageFunction);

  /** InputImageType type alias support */
  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using IndexType = typename InputImageType::IndexType;

  /** Dimension of the underlying image. */
  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

#if !defined(ITK_LEGACY_REMOVE)
  static constexpr unsigned int ImageDimension2 = ImageDimension;
#endif

  using ContinuousIndexType = ContinuousIndex<SpacePrecisionType, Self::ImageDimension>;

  using NeighborhoodType = Neighborhood<InputPixelType, Self::ImageDimension>;
  using OperatorNeighborhoodType = Neighborhood<TOutput, Self::ImageDimension>;

  using VectorType = Vector<TOutput, Self::ImageDimension>;
  using OutputType = typename Superclass::OutputType;
  using OperatorArrayType = FixedArray<OperatorNeighborhoodType, Self::ImageDimension>;

  using GaussianDerivativeSpatialFunctionType = GaussianDerivativeSpatialFunction<TOutput, 1>;
  using GaussianDerivativeSpatialFunctionPointer = typename GaussianDerivativeSpatialFunctionType::Pointer;

#if !defined(ITK_LEGACY_REMOVE)
  using GaussianDerivativeFunctionType = GaussianDerivativeSpatialFunctionType;
  using GaussianDerivativeFunctionPointer = GaussianDerivativeSpatialFunctionPointer;
#endif

  /** Point type alias support */
  // using PointType = Point< TOutput, Self::ImageDimension >;
  using PointType = typename InputImageType::PointType;

  /** Evaluate the function at the specified point. */
  OutputType
  Evaluate(const PointType & point) const override;

  /** Evaluate the function at specified Index position. */
  OutputType
  EvaluateAtIndex(const IndexType & index) const override;

  /** Evaluate the function at specified ContinuousIndex position. */
  OutputType
  EvaluateAtContinuousIndex(const ContinuousIndexType & cindex) const override;

  /**
   * UseImageSpacing controls the extent of the computations.
   * Set UseImageSpacing to true to set the units to physical units of the image.
   * Set UseImageSpacing to false to set the units of pixels. */
  void
  SetUseImageSpacing(const bool val)
  {
    if (val != this->m_UseImageSpacing)
    {
      this->m_UseImageSpacing = val;
      this->RecomputeGaussianKernel();
    }
  }
  itkBooleanMacro(UseImageSpacing);
  itkGetMacro(UseImageSpacing, bool);

  /** The variance for the discrete Gaussian kernel. Sets the variance
   * independently for each dimension, but see also
   * SetVariance(const double v). The default is 0.0 in each dimension.
   * The extent of the kernel is controlled by UseImageSpacing.
   */
  void
  SetSigma(const double * sigma);

  void
  SetSigma(const double sigma);

  const double *
  GetSigma() const
  {
    return m_Sigma;
  }

  /** Set the extent of the discrete Gaussian kernel. */
  void
  SetExtent(const double * extent);

  void
  SetExtent(const double extent);

  const double *
  GetExtent() const
  {
    return m_Extent;
  }

  /** Set the input image.
   * \warning this method caches BufferedRegion information.
   * If the BufferedRegion has changed, user must call
   * SetInputImage again to update cached values. */
  void
  SetInputImage(const InputImageType * ptr) override;

protected:
  GaussianDerivativeImageFunction();
  ~GaussianDerivativeImageFunction() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Recompute the Gaussian kernel used to evaluate indexes. This should use
   * a fastest Derivative Gaussian operator. */
  void
  RecomputeGaussianKernel();

private:
  double m_Sigma[ImageDimension];

  /** Array of 1D operators. Contains a derivative kernel for
   * each dimension. Note: A future version of ITK could extend this array
   * to include a Gaussian blurring kernel for each dimension.*/
  OperatorArrayType m_OperatorArray;

  std::vector<Offset<ImageDimension>> m_ImageNeighborhoodOffsets[ImageDimension];

  double m_Extent[ImageDimension];

  /** Flag to indicate whether to use image spacing. */
  bool m_UseImageSpacing{ true };

  /** Neighborhood Image Function. */
  const GaussianDerivativeSpatialFunctionPointer m_GaussianDerivativeSpatialFunction{
    GaussianDerivativeSpatialFunctionType::New()
  };
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGaussianDerivativeImageFunction.hxx"
#endif

#endif

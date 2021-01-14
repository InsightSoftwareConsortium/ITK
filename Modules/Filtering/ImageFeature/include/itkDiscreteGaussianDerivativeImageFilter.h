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
#ifndef itkDiscreteGaussianDerivativeImageFilter_h
#define itkDiscreteGaussianDerivativeImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class DiscreteGaussianDerivativeImageFilter
 * \brief Calculates image derivatives using discrete derivative gaussian kernels.
 * This filter calculates Gaussian derivative by separable convolution of an image
 * and a discrete Gaussian derivative operator (kernel).
 *
 * The Gaussian operators used here were described by Tony Lindeberg (Discrete
 * Scale-Space Theory and the Scale-Space Primal Sketch.  Dissertation. Royal
 * Institute of Technology, Stockholm, Sweden. May 1991.)
 *
 * The variance or standard deviation (sigma) will be evaluated as pixel units
 * if SetUseImageSpacing is off (false) or as physical units if
 * SetUseImageSpacing is on (true, default). The variance can be set
 * independently in each dimension.
 *
 * When the Gaussian kernel is small, this filter tends to run faster than
 * itk::RecursiveGaussianImageFilter.
 *
 * \author Ivan Macia, Vicomtech, Spain, https://www.vicomtech.org/en
 *
 * This implementation was taken from the Insight Journal paper:
 * https://www.insight-journal.org/browse/publication/179
 *
 * \sa GaussianDerivativeOperator
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 *
 * \ingroup ImageEnhancement
 * \ingroup ITKImageFeature
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT DiscreteGaussianDerivativeImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DiscreteGaussianDerivativeImageFilter);

  /** Standard class type aliases. */
  using Self = DiscreteGaussianDerivativeImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DiscreteGaussianDerivativeImageFilter, ImageToImageFilter);

  /** Image type information. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Typedef of double containers */
  using ArrayType = FixedArray<double, Self::ImageDimension>;

  /** Array for storing desired order of derivatives */
  using OrderArrayType = FixedArray<unsigned int, Self::ImageDimension>;

  /** Order of derivatives in each dimension. Sets the derivative order
   * independently for each dimension, but see also
   * SetOrder(const unsigned int v). The default is 1 in each dimension. */
  itkSetMacro(Order, OrderArrayType);
  itkGetConstMacro(Order, const OrderArrayType);

  /** The variance for the discrete Gaussian kernel.  Sets the variance
   * independently for each dimension, but
   * see also SetVariance(const double v). The default is 0.0 in each
   * dimension. If UseImageSpacing is true, the units are the physical units
   * of your image.  If UseImageSpacing is false then the units are
   * pixels. */
  itkSetMacro(Variance, ArrayType);
  itkGetConstMacro(Variance, const ArrayType);

  /** The algorithm will size the discrete kernel so that the error
   * resulting from truncation of the kernel is no greater than
   * MaximumError. The default is 0.01 in each dimension. */
  itkSetMacro(MaximumError, ArrayType);
  itkGetConstMacro(MaximumError, const ArrayType);

  /** Set the kernel to be no wider than MaximumKernelWidth pixels,
   *  even if MaximumError demands it. The default is 32 pixels. */
  itkGetConstMacro(MaximumKernelWidth, int);
  itkSetMacro(MaximumKernelWidth, int);

  /** \brief Set/Get number of pieces to divide the input for the
   * internal composite pipeline. The upstream pipeline will not be
   * effected.
   *
   * The default value is $ImageDimension^2$.
   *
   * This parameter was introduced to reduce the memory used by images
   * internally, at the cost of performance.
   */
  itkSetMacro(InternalNumberOfStreamDivisions, unsigned int);
  itkGetConstMacro(InternalNumberOfStreamDivisions, unsigned int);

  /** Convenience Set methods for setting all dimensional parameters
   *  to the same values.
   */
  /*@{*/
  void
  SetOrder(const typename OrderArrayType::ValueType v)
  {
    OrderArrayType a;

    a.Fill(v);
    this->SetOrder(a);
  }

  void
  SetVariance(const typename ArrayType::ValueType v)
  {
    ArrayType a;

    a.Fill(v);
    this->SetVariance(a);
  }

  void
  SetMaximumError(const typename ArrayType::ValueType v)
  {
    ArrayType a;

    a.Fill(v);
    this->SetMaximumError(a);
  }

  /*@}*/

  /** Set/Get whether or not the filter will use the spacing of the input
      image in its calculations. Default is ImageSpacingOn. */
  itkSetMacro(UseImageSpacing, bool);
  itkGetConstMacro(UseImageSpacing, bool);
  itkBooleanMacro(UseImageSpacing);

  /** Set/Get the flag for calculating scale-space normalized derivatives.
   * Normalized derivatives are obtained multiplying by the scale
   * parameter t. */
  itkSetMacro(NormalizeAcrossScale, bool);
  itkGetConstMacro(NormalizeAcrossScale, bool);
  itkBooleanMacro(NormalizeAcrossScale);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(OutputHasNumericTraitsCheck, (Concept::HasNumericTraits<OutputPixelType>));
  // End concept checking
#endif

protected:
  DiscreteGaussianDerivativeImageFilter()
  {
    m_Order.Fill(1);
    m_Variance.Fill(0.0);
    m_MaximumError.Fill(0.01);
    m_MaximumKernelWidth = 32;
    m_UseImageSpacing = true;
    m_NormalizeAcrossScale = false;
    m_InternalNumberOfStreamDivisions = ImageDimension * ImageDimension;
  }

  ~DiscreteGaussianDerivativeImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** DiscreteGaussianDerivativeImageFilter needs a larger input requested region
   * than the output requested region (larger by the size of the
   * Gaussian kernel).  As such, DiscreteGaussianDerivativeImageFilter needs to
   * provide an implementation for GenerateInputRequestedRegion() in
   * order to inform the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

  /** Standard pipeline method. While this class does not implement a
   * ThreadedGenerateData(), its GenerateData() delegates all
   * calculations to an NeighborhoodOperatorImageFilter.  Since the
   * NeighborhoodOperatorImageFilter is multithreaded, this filter is
   * multithreaded by default. */
  void
  GenerateData() override;

private:
  /** The order of the derivatives in each dimensional direction. */
  OrderArrayType m_Order;

  /** The variance of the gaussian blurring kernel in each dimensional
    direction. */
  ArrayType m_Variance;

  /** The maximum error of the gaussian blurring kernel in each dimensional
   * direction. For definition of maximum error, see GaussianOperator.
   * \sa GaussianOperator */
  ArrayType m_MaximumError;

  /** Maximum allowed kernel width for any dimension of the discrete Gaussian
      approximation */
  int m_MaximumKernelWidth;

  /** Flag to indicate whether to use image spacing */
  bool m_UseImageSpacing;

  /** Flag for scale-space normalization of derivatives. */
  bool m_NormalizeAcrossScale;

  /** Number of pieces to divide the input on the internal composite
  pipeline. The upstream pipeline will not be effected. */
  unsigned int m_InternalNumberOfStreamDivisions;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDiscreteGaussianDerivativeImageFilter.hxx"
#endif

#endif

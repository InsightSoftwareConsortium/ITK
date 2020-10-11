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
#ifndef itkTikhonovDeconvolutionImageFilter_h
#define itkTikhonovDeconvolutionImageFilter_h

#include "itkInverseDeconvolutionImageFilter.h"

namespace itk
{
/**
 *\class TikhonovDeconvolutionImageFilter
 * \brief An inverse deconvolution filter regularized in the Tikhonov sense.
 *
 * The Tikhonov deconvolution filter is the inverse deconvolution
 * filter with a regularization term added to the denominator.
 * The filter minimizes the equation
 * \f[ ||\hat{f} \otimes h - g||_{L_2}^2 + \mu||\hat{f}||^2
 * \f]
 * where \f$\hat{f}\f$ is the estimate of the unblurred image,
 * \f$h\f$ is the blurring kernel, \f$g\f$ is the blurred image, and
 * \f$\mu\f$ is a non-negative real regularization function.
 *
 * The filter applies a kernel described in the Fourier domain as
 * \f$H^*(\omega) / (|H(\omega)|^2 + \mu)\f$ where \f$H(\omega)\f$ is
 * the Fourier transform of \f$h\f$. The term \f$\mu\f$ is called
 * RegularizationConstant in this filter. If \f$\mu\f$ is set to zero,
 * this filter is equivalent to the InverseDeconvolutionImageFilter.
 *
 * \author Gaetan Lehmann, Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France
 * \author Cory Quammen, The University of North Carolina at Chapel Hill
 *
 * \ingroup ITKDeconvolution
 *
 */
template <typename TInputImage,
          typename TKernelImage = TInputImage,
          typename TOutputImage = TInputImage,
          typename TInternalPrecision = double>
class ITK_TEMPLATE_EXPORT TikhonovDeconvolutionImageFilter
  : public InverseDeconvolutionImageFilter<TInputImage, TKernelImage, TOutputImage, TInternalPrecision>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TikhonovDeconvolutionImageFilter);

  using Self = TikhonovDeconvolutionImageFilter;
  using Superclass = InverseDeconvolutionImageFilter<TInputImage, TKernelImage, TOutputImage, TInternalPrecision>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information ( and related methods ) */
  itkTypeMacro(TikhonovDeconvolutionImageFilter, InverseDeconvolutionImageFilter);

  /** Dimensionality of input and output data is assumed to be the same. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using KernelImageType = TKernelImage;
  using InputPixelType = typename Superclass::InputPixelType;
  using OutputPixelType = typename Superclass::OutputPixelType;
  using KernelPixelType = typename Superclass::KernelPixelType;
  using InputIndexType = typename Superclass::InputIndexType;
  using OutputIndexType = typename Superclass::OutputIndexType;
  using KernelIndexType = typename Superclass::KernelIndexType;
  using InputSizeType = typename Superclass::InputSizeType;
  using OutputSizeType = typename Superclass::OutputSizeType;
  using KernelSizeType = typename Superclass::KernelSizeType;
  using SizeValueType = typename Superclass::SizeValueType;
  using InputRegionType = typename Superclass::InputRegionType;
  using OutputRegionType = typename Superclass::OutputRegionType;
  using KernelRegionType = typename Superclass::KernelRegionType;

  /** Internal image types. */
  using InternalImageType = typename Superclass::InternalImageType;
  using InternalImagePointerType = typename Superclass::InternalImagePointerType;
  using InternalComplexType = typename Superclass::InternalComplexType;
  using InternalComplexImageType = typename Superclass::InternalComplexImageType;
  using InternalComplexImagePointerType = typename Superclass::InternalComplexImagePointerType;

  /** The regularization factor. Larger values reduce the dominance of
   * noise in the solution, but results in higher approximation error
   * in the deblurred image. Default value is 0.0, yielding the same
   * results as the InverseDeconvolutionImageFilter. */
  itkSetMacro(RegularizationConstant, double);
  itkGetConstMacro(RegularizationConstant, double);

protected:
  TikhonovDeconvolutionImageFilter();
  ~TikhonovDeconvolutionImageFilter() override = default;

  /** This filter uses a minipipeline to compute the output. */
  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  double m_RegularizationConstant;
};

namespace Functor
{
template <typename TInput1, typename TInput2, typename TOutput>
class ITK_TEMPLATE_EXPORT TikhonovDeconvolutionFunctor
{
public:
  TikhonovDeconvolutionFunctor() = default;
  ~TikhonovDeconvolutionFunctor() = default;
  TikhonovDeconvolutionFunctor(const TikhonovDeconvolutionFunctor & f)
    : m_RegularizationConstant(f.m_RegularizationConstant)
    , m_KernelZeroMagnitudeThreshold(f.m_KernelZeroMagnitudeThreshold)
  {}


  bool
  operator!=(const TikhonovDeconvolutionFunctor &) const
  {
    return false;
  }
  bool
  operator==(const TikhonovDeconvolutionFunctor & other) const
  {
    return !(*this != other);
  }
  inline TOutput
  operator()(const TInput1 & I, const TInput2 & H) const
  {
    typename TOutput::value_type normH = std::norm(H);
    typename TOutput::value_type denominator = normH + m_RegularizationConstant;
    TOutput                      value = NumericTraits<TOutput>::ZeroValue();
    if (denominator >= m_KernelZeroMagnitudeThreshold)
    {
      value = static_cast<TOutput>(I * (std::conj(H) / denominator));
    }

    return value;
  }

  /** Set/get the regular constant. This needs to be a non-negative
   * real value. */
  void
  SetRegularizationConstant(double constant)
  {
    m_RegularizationConstant = constant;
  }
  double
  GetRegularizationConstant() const
  {
    return m_RegularizationConstant;
  }

  /** Set/get the threshold value below which complex magnitudes are considered
   * to be zero. */
  void
  SetKernelZeroMagnitudeThreshold(double mu)
  {
    m_KernelZeroMagnitudeThreshold = mu;
  }
  double
  GetKernelZeroMagnitudeThreshold() const
  {
    return m_KernelZeroMagnitudeThreshold;
  }

private:
  double m_RegularizationConstant = 0.0;
  double m_KernelZeroMagnitudeThreshold = 0.0;
};
} // namespace Functor

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTikhonovDeconvolutionImageFilter.hxx"
#endif

#endif

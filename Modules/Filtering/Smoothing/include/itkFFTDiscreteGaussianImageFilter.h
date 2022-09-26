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
#ifndef itkFFTDiscreteGaussianImageFilter_h
#define itkFFTDiscreteGaussianImageFilter_h

#include "itkDiscreteGaussianImageFilter.h"
#include "itkFFTConvolutionImageFilter.h"
#include "itkImage.h"
#include "itkMacro.h"
#include "ITKSmoothingExport.h"

namespace itk
{
/** \class FFTDiscreteGaussianImageFilterEnums
 * \brief Contains all enum classes used by FFTDiscreteGaussianImageFilter class.
 * \ingroup ITKSmoothing
 */
class FFTDiscreteGaussianImageFilterEnums
{
public:
  /**
   * \class KernelSource
   * \ingroup ITKSmoothing
   * ITK defines multiple possible sources for generating a
   * Gaussian kernel for smoothing.
   *
   * Generating an ND kernel by multiplying
   * N 1D directional operators produces nearly identical
   * results to the DiscreteGaussianImageFilter class.
   *
   * Generating an ND kernel image directly with
   * GaussianImageSource relies on a slightly different
   * spatial function and will produce different smoothing,
   * but kernel generation will be faster.
   */
  enum class KernelSource : uint8_t
  {
    OPERATORS = 0,
    IMAGE_SOURCE,
  };
};
// Define how to print enumeration
extern ITKSmoothing_EXPORT std::ostream &
                           operator<<(std::ostream & out, const FFTDiscreteGaussianImageFilterEnums::KernelSource value);

/**
 * \class FFTDiscreteGaussianImageFilter
 * \brief Blurs an image by convolution with a discrete gaussian kernel
 * in the frequency domain.
 *
 * This filter performs Gaussian blurring by convolution of an image
 * and a discrete Gaussian operator (kernel) in the frequency domain
 * by way of Fast Fourier Transform forward and inverse operations.
 *
 * \sa DiscreteGaussianImageFilter
 * \sa GaussianImageSource
 * \sa FFTConvolutionImageFilter
 * \sa RecursiveGaussianImageFilter
 * \sa ZeroFluxNeumannBoundaryCondition
 *
 * \ingroup ImageEnhancement
 * \ingroup ImageFeatureExtraction
 * \ingroup ITKSmoothing
 */

template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT FFTDiscreteGaussianImageFilter : public DiscreteGaussianImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FFTDiscreteGaussianImageFilter);

  /** Standard class type aliases. */
  using Self = FFTDiscreteGaussianImageFilter;
  using Superclass = DiscreteGaussianImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FFTDiscreteGaussianImageFilter, DiscreteGaussianImageFilter);

  /** Image type information. */
  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  using OutputPixelType = typename Superclass::OutputPixelType;
  using OutputInternalPixelType = typename Superclass::OutputInternalPixelType;
  using InputPixelType = typename Superclass::InputPixelType;
  using InputInternalPixelType = typename Superclass::InputInternalPixelType;

  /** Pixel value type for Vector pixel types **/
  using InputPixelValueType = typename Superclass::InputPixelValueType;
  using OutputPixelValueType = typename Superclass::OutputPixelValueType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  static constexpr unsigned int ImageDimension = Superclass::ImageDimension;

  /** Type of the pixel to use for intermediate results */
  using typename Superclass::RealOutputPixelType;
  using typename Superclass::RealOutputImageType;
  using typename Superclass::RealOutputPixelValueType;
  using RealPixelType = RealOutputPixelType;
  using RealImageType = RealOutputImageType;

  /** Typedef to describe the boundary condition. */
  using typename Superclass::BoundaryConditionType;
  using typename Superclass::InputBoundaryConditionPointerType;
  using typename Superclass::InputDefaultBoundaryConditionType;
  using typename Superclass::RealBoundaryConditionPointerType;
  using typename Superclass::RealDefaultBoundaryConditionType;

  /** Typedef of double containers */
  using typename Superclass::ArrayType;
  using typename Superclass::SigmaArrayType;
  using typename Superclass::ScalarRealType;

  /** Typedef to describe kernel parameters */
  using typename Superclass::KernelType;
  using typename Superclass::RadiusType;

  /** Typedef for convolution */
  using ConvolutionImageFilterType = FFTConvolutionImageFilter<RealImageType, RealImageType, OutputImageType>;

  /** Overridden accessors for unused parameters */

  void
  SetInputBoundaryCondition(const InputBoundaryConditionPointerType) override;

  itkSetMacro(KernelSource, FFTDiscreteGaussianImageFilterEnums::KernelSource);
  itkGetConstMacro(KernelSource, FFTDiscreteGaussianImageFilterEnums::KernelSource);

  itkGetConstMacro(KernelImage, typename RealImageType::Pointer);

protected:
  FFTDiscreteGaussianImageFilter() = default;
  ~FFTDiscreteGaussianImageFilter() override = default;

  /** Pad input region to kernel image size */
  void
  GenerateInputRequestedRegion() override;

  /** Standard pipeline method. While this class does not implement a
   * ThreadedGenerateData(), its GenerateData() delegates all
   * calculations to an NeighborhoodOperatorImageFilter.  Since the
   * NeighborhoodOperatorImageFilter is multithreaded, this filter is
   * multithreaded by default. */
  void
  GenerateData() override;

  /** Create a kernel image matching user input specifications.
   *  Returns a reference to the member m_KernelImage. */
  auto
  GenerateKernelImage() -> RealImageType *;

private:
  /* Enum for choosing among available kernel generation methods */
  FFTDiscreteGaussianImageFilterEnums::KernelSource m_KernelSource =
    FFTDiscreteGaussianImageFilterEnums::KernelSource::OPERATORS;

  /* Kernel image is allocated with GenerateKernelImage() */
  typename RealImageType::Pointer m_KernelImage;

  /* Persist mini-pipeline filter to minimize construction costs
   * on repeated calls to GenerateData() */
  typename ConvolutionImageFilterType::Pointer m_ConvolutionImageFilter = ConvolutionImageFilterType::New();
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFFTDiscreteGaussianImageFilter.hxx"
#endif

#endif

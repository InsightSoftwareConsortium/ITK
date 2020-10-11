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
#ifndef itkGPUDiscreteGaussianImageFilter_h
#define itkGPUDiscreteGaussianImageFilter_h

#include "itkGPUImage.h"
#include "itkGPUImageToImageFilter.h"
#include "itkGPUNeighborhoodOperatorImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"

namespace itk
{
/**
 * \class GPUDiscreteGaussianImageFilter
 * \brief Blurs an image by separable convolution with discrete gaussian kernels.
 * This filter performs Gaussian blurring by separable convolution of an image
 * and a discrete Gaussian operator (kernel). GPUNeighborhoodOperatorImageFilter
 * is used to compute 1D directional discrete Gaussian filtering for each axis.
 *
 * The variance or standard deviation (sigma) will be evaluated as pixel units
 * if SetUseImageSpacing is off (false) or as physical units if
 * SetUseImageSpacing is on (true, default). The variance can be set
 * independently in each dimension.
 *
 * When the Gaussian kernel is small, this filter tends to run faster than
 * itk::RecursiveGaussianImageFilter.
 *
 * \ingroup ITKGPUSmoothing
 *
 * \sphinx
 * \sphinxexample{Filtering/Smoothing/SmoothImageWithDiscreteGaussianFilter,Smooth Image With Discrete Gaussian Filter}
 * \endsphinx
 */

template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT GPUDiscreteGaussianImageFilter
  : public GPUImageToImageFilter<TInputImage, TOutputImage, DiscreteGaussianImageFilter<TInputImage, TOutputImage>>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GPUDiscreteGaussianImageFilter);

  /** Standard class type aliases. */
  using Self = GPUDiscreteGaussianImageFilter;
  using CPUSuperclass = DiscreteGaussianImageFilter<TInputImage, TOutputImage>;
  using GPUSuperclass = GPUImageToImageFilter<TInputImage, TOutputImage, CPUSuperclass>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GPUDiscreteGaussianImageFilter, GPUImageToImageFilter);

  /** Image type information. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same.   */
  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  using OutputPixelType = typename TOutputImage::PixelType;
  using OutputInternalPixelType = typename TOutputImage::InternalPixelType;
  using InputPixelType = typename TInputImage::PixelType;
  using InputInternalPixelType = typename TInputImage::InternalPixelType;

  /** Pixel value type for Vector pixel types **/
  using InputPixelValueType = typename NumericTraits<InputPixelType>::ValueType;
  using OutputPixelValueType = typename NumericTraits<OutputPixelType>::ValueType;

  using RealOutputPixelType = OutputPixelType;
  using RealOutputImageType = GPUImage<OutputPixelType, ImageDimension>;
  using RealOutputPixelValueType = typename NumericTraits<RealOutputPixelType>::ValueType;
  using FirstFilterType =
    GPUNeighborhoodOperatorImageFilter<InputImageType, RealOutputImageType, RealOutputPixelValueType>;
  using IntermediateFilterType =
    GPUNeighborhoodOperatorImageFilter<RealOutputImageType, RealOutputImageType, RealOutputPixelValueType>;
  using LastFilterType =
    GPUNeighborhoodOperatorImageFilter<RealOutputImageType, OutputImageType, RealOutputPixelValueType>;
  using SingleFilterType =
    GPUNeighborhoodOperatorImageFilter<InputImageType, OutputImageType, RealOutputPixelValueType>;

  void
  GenerateInputRequestedRegion() override;

protected:
  GPUDiscreteGaussianImageFilter();
  ~GPUDiscreteGaussianImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Standard GPU pipeline method. */
  void
  GPUGenerateData() override;

private:
  /** Intermediate 1D Gaussian filters */
  typename FirstFilterType::Pointer                     m_FirstFilter;
  typename LastFilterType::Pointer                      m_LastFilter;
  std::vector<typename IntermediateFilterType::Pointer> m_IntermediateFilters;
  typename SingleFilterType::Pointer                    m_SingleFilter;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGPUDiscreteGaussianImageFilter.hxx"
#endif

#endif

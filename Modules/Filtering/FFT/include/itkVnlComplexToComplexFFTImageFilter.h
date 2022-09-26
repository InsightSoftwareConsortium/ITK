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
#ifndef itkVnlComplexToComplexFFTImageFilter_h
#define itkVnlComplexToComplexFFTImageFilter_h

#include "itkComplexToComplexFFTImageFilter.h"
#include "itkFFTImageFilterFactory.h"

namespace itk
{
/**
 * \class VnlComplexToComplexFFTImageFilter
 *
 * \brief VNL based complex to complex Fast Fourier Transform.
 *
 * This filter requires input images with sizes which are a power of two.
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 *
 * \sa ComplexToComplexFFTImageFilter
 * \sa FFTWComplexToComplexFFTImageFilter
 * \sa VnlForwardFFTImageFilter
 * \sa VnlInverseFFTImageFilter
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT VnlComplexToComplexFFTImageFilter
  : public ComplexToComplexFFTImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VnlComplexToComplexFFTImageFilter);

  /** Standard class type aliases. */
  using Self = VnlComplexToComplexFFTImageFilter;
  using Superclass = ComplexToComplexFFTImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using typename Superclass::ImageType;
  using PixelType = typename ImageType::PixelType;
  using typename Superclass::InputImageType;
  using typename Superclass::OutputImageType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VnlComplexToComplexFFTImageFilter, ComplexToComplexFFTImageFilter);

  static constexpr unsigned int ImageDimension = ImageType::ImageDimension;

protected:
  VnlComplexToComplexFFTImageFilter();
  ~VnlComplexToComplexFFTImageFilter() override = default;

  void
  BeforeThreadedGenerateData() override;
  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;
};

template <>
struct FFTImageFilterTraits<VnlComplexToComplexFFTImageFilter>
{
  template <typename TUnderlying>
  using InputPixelType = std::complex<TUnderlying>;
  template <typename TUnderlying>
  using OutputPixelType = std::complex<TUnderlying>;
  using FilterDimensions = std::integer_sequence<unsigned int, 4, 3, 2, 1>;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVnlComplexToComplexFFTImageFilter.hxx"
#endif

#endif // itkVnlComplexToComplexFFTImageFilter_h

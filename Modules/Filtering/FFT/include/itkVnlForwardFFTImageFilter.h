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
#ifndef itkVnlForwardFFTImageFilter_h
#define itkVnlForwardFFTImageFilter_h

#include "itkPocketFFTForwardFFTImageFilter.h"

#if defined(ITK_LEGACY_SILENT)
#  define ITK_VNL_FFT_DEPRECATED
#else
#  define ITK_VNL_FFT_DEPRECATED \
    [[deprecated("VnlForwardFFTImageFilter is deprecated; it now routes to itk::PocketFFTForwardFFTImageFilter.")]]
#endif

#if !defined(ITK_LEGACY_REMOVE) && !defined(ITK_FUTURE_LEGACY_REMOVE)
namespace itk
{
/** \class VnlForwardFFTImageFilter
 * \brief Deprecated compatibility wrapper that routes to PocketFFTForwardFFTImageFilter.
 *
 * \deprecated The VNL/Temperton FFT backend was removed; this name now derives
 * from itk::PocketFFTForwardFFTImageFilter. Migrate to the PocketFFT class or the
 * factory-default itk::ForwardFFTImageFilter.
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 */
template <typename TInputImage,
          typename TOutputImage = Image<std::complex<typename TInputImage::PixelType>, TInputImage::ImageDimension>>
class ITK_VNL_FFT_DEPRECATED ITK_TEMPLATE_EXPORT VnlForwardFFTImageFilter
  : public PocketFFTForwardFFTImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VnlForwardFFTImageFilter);

  using Self = VnlForwardFFTImageFilter;
  using Superclass = PocketFFTForwardFFTImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(VnlForwardFFTImageFilter);

protected:
  VnlForwardFFTImageFilter() = default;
  ~VnlForwardFFTImageFilter() override = default;
};

/** \cond HIDE_SPECIALIZATION */
#  if defined(__GNUC__) || defined(__clang__)
#    pragma GCC diagnostic push
#    pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#  endif
template <>
struct FFTImageFilterTraits<VnlForwardFFTImageFilter> : public FFTImageFilterTraits<PocketFFTForwardFFTImageFilter>
{};
#  if defined(__GNUC__) || defined(__clang__)
#    pragma GCC diagnostic pop
#  endif
/** \endcond */
} // namespace itk
#endif // !ITK_LEGACY_REMOVE && !ITK_FUTURE_LEGACY_REMOVE

#undef ITK_VNL_FFT_DEPRECATED
#endif // itkVnlForwardFFTImageFilter_h

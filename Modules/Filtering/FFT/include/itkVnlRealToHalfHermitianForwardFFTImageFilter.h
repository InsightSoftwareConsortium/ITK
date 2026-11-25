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
#ifndef itkVnlRealToHalfHermitianForwardFFTImageFilter_h
#define itkVnlRealToHalfHermitianForwardFFTImageFilter_h

#include "itkPocketFFTRealToHalfHermitianForwardFFTImageFilter.h"

#if defined(ITK_LEGACY_SILENT)
#  define ITK_VNL_FFT_DEPRECATED
#else
#  define ITK_VNL_FFT_DEPRECATED                                                                \
    [[deprecated("VnlRealToHalfHermitianForwardFFTImageFilter is deprecated; it now routes to " \
                 "itk::PocketFFTRealToHalfHermitianForwardFFTImageFilter.")]]
#endif

#if !defined(ITK_LEGACY_REMOVE) && !defined(ITK_FUTURE_LEGACY_REMOVE)
namespace itk
{
/** \class VnlRealToHalfHermitianForwardFFTImageFilter
 * \brief Deprecated compatibility wrapper that routes to PocketFFTRealToHalfHermitianForwardFFTImageFilter.
 *
 * \deprecated The VNL/Temperton FFT backend was removed; this name now derives
 * from itk::PocketFFTRealToHalfHermitianForwardFFTImageFilter. Migrate to the PocketFFT class or the
 * factory-default itk::RealToHalfHermitianForwardFFTImageFilter.
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 */
template <typename TInputImage,
          typename TOutputImage = Image<std::complex<typename TInputImage::PixelType>, TInputImage::ImageDimension>>
class ITK_VNL_FFT_DEPRECATED ITK_TEMPLATE_EXPORT VnlRealToHalfHermitianForwardFFTImageFilter
  : public PocketFFTRealToHalfHermitianForwardFFTImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VnlRealToHalfHermitianForwardFFTImageFilter);

  using Self = VnlRealToHalfHermitianForwardFFTImageFilter;
  using Superclass = PocketFFTRealToHalfHermitianForwardFFTImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(VnlRealToHalfHermitianForwardFFTImageFilter);

protected:
  VnlRealToHalfHermitianForwardFFTImageFilter() = default;
  ~VnlRealToHalfHermitianForwardFFTImageFilter() override = default;
};

/** \cond HIDE_SPECIALIZATION */
#  if defined(__GNUC__) || defined(__clang__)
#    pragma GCC diagnostic push
#    pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#  elif defined(_MSC_VER)
#    pragma warning(push)
#    pragma warning(disable : 4996)
#  endif
template <>
struct FFTImageFilterTraits<VnlRealToHalfHermitianForwardFFTImageFilter>
  : public FFTImageFilterTraits<PocketFFTRealToHalfHermitianForwardFFTImageFilter>
{};
#  if defined(__GNUC__) || defined(__clang__)
#    pragma GCC diagnostic pop
#  elif defined(_MSC_VER)
#    pragma warning(pop)
#  endif
/** \endcond */
} // namespace itk
#endif // !ITK_LEGACY_REMOVE && !ITK_FUTURE_LEGACY_REMOVE

#undef ITK_VNL_FFT_DEPRECATED
#endif // itkVnlRealToHalfHermitianForwardFFTImageFilter_h

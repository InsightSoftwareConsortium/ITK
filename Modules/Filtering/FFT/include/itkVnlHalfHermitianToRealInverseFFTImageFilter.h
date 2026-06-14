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
#ifndef itkVnlHalfHermitianToRealInverseFFTImageFilter_h
#define itkVnlHalfHermitianToRealInverseFFTImageFilter_h

#include "itkPocketFFTHalfHermitianToRealInverseFFTImageFilter.h"

#if defined(ITK_LEGACY_SILENT)
#  define ITK_VNL_FFT_DEPRECATED
#else
#  define ITK_VNL_FFT_DEPRECATED                                                                \
    [[deprecated("VnlHalfHermitianToRealInverseFFTImageFilter is deprecated; it now routes to " \
                 "itk::PocketFFTHalfHermitianToRealInverseFFTImageFilter.")]]
#endif

#if !defined(ITK_LEGACY_REMOVE) && !defined(ITK_FUTURE_LEGACY_REMOVE)
namespace itk
{
/** \class VnlHalfHermitianToRealInverseFFTImageFilter
 * \brief Deprecated compatibility wrapper that routes to PocketFFTHalfHermitianToRealInverseFFTImageFilter.
 *
 * \deprecated The VNL/Temperton FFT backend was removed; this name now derives
 * from itk::PocketFFTHalfHermitianToRealInverseFFTImageFilter. Migrate to the PocketFFT class or the
 * factory-default itk::HalfHermitianToRealInverseFFTImageFilter.
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 */
template <typename TInputImage,
          typename TOutputImage = Image<typename TInputImage::PixelType::value_type, TInputImage::ImageDimension>>
class ITK_VNL_FFT_DEPRECATED ITK_TEMPLATE_EXPORT VnlHalfHermitianToRealInverseFFTImageFilter
  : public PocketFFTHalfHermitianToRealInverseFFTImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VnlHalfHermitianToRealInverseFFTImageFilter);

  using Self = VnlHalfHermitianToRealInverseFFTImageFilter;
  using Superclass = PocketFFTHalfHermitianToRealInverseFFTImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(VnlHalfHermitianToRealInverseFFTImageFilter);

protected:
  VnlHalfHermitianToRealInverseFFTImageFilter() = default;
  ~VnlHalfHermitianToRealInverseFFTImageFilter() override = default;
};

/** \cond HIDE_SPECIALIZATION */
#  if defined(__GNUC__) || defined(__clang__)
#    pragma GCC diagnostic push
#    pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#  endif
template <>
struct FFTImageFilterTraits<VnlHalfHermitianToRealInverseFFTImageFilter>
  : public FFTImageFilterTraits<PocketFFTHalfHermitianToRealInverseFFTImageFilter>
{};
#  if defined(__GNUC__) || defined(__clang__)
#    pragma GCC diagnostic pop
#  endif
/** \endcond */
} // namespace itk
#endif // !ITK_LEGACY_REMOVE && !ITK_FUTURE_LEGACY_REMOVE

#undef ITK_VNL_FFT_DEPRECATED
#endif // itkVnlHalfHermitianToRealInverseFFTImageFilter_h

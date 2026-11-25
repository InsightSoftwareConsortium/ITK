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

#include "itkPocketFFTComplexToComplexFFTImageFilter.h"

#if defined(ITK_LEGACY_SILENT)
#  define ITK_VNL_FFT_DEPRECATED
#else
#  define ITK_VNL_FFT_DEPRECATED                                                      \
    [[deprecated("VnlComplexToComplexFFTImageFilter is deprecated; it now routes to " \
                 "itk::PocketFFTComplexToComplexFFTImageFilter.")]]
#endif

#if !defined(ITK_LEGACY_REMOVE) && !defined(ITK_FUTURE_LEGACY_REMOVE)
namespace itk
{
/** \class VnlComplexToComplexFFTImageFilter
 * \brief Deprecated compatibility wrapper that routes to PocketFFTComplexToComplexFFTImageFilter.
 *
 * \deprecated The VNL/Temperton FFT backend was removed; this name now derives
 * from itk::PocketFFTComplexToComplexFFTImageFilter. Migrate to the PocketFFT class or the
 * factory-default itk::ComplexToComplexFFTImageFilter.
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_VNL_FFT_DEPRECATED ITK_TEMPLATE_EXPORT VnlComplexToComplexFFTImageFilter
  : public PocketFFTComplexToComplexFFTImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VnlComplexToComplexFFTImageFilter);

  using Self = VnlComplexToComplexFFTImageFilter;
  using Superclass = PocketFFTComplexToComplexFFTImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(VnlComplexToComplexFFTImageFilter);

protected:
  VnlComplexToComplexFFTImageFilter() = default;
  ~VnlComplexToComplexFFTImageFilter() override = default;
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
struct FFTImageFilterTraits<VnlComplexToComplexFFTImageFilter>
  : public FFTImageFilterTraits<PocketFFTComplexToComplexFFTImageFilter>
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
#endif // itkVnlComplexToComplexFFTImageFilter_h

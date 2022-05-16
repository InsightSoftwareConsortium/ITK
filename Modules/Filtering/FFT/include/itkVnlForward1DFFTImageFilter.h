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
#ifndef itkVnlForward1DFFTImageFilter_h
#define itkVnlForward1DFFTImageFilter_h

#include "itkForward1DFFTImageFilter.h"
#include <complex>
#include "itkFFTImageFilterFactory.h"

namespace itk
{

/** \class VnlForward1DFFTImageFilter
 *
 * \brief Perform the FFT along one dimension of an image using Vnl as a
 * backend.
 *
 * \ingroup ITKFFT
 * \ingroup FourierTransform
 */
template <typename TInputImage,
          typename TOutputImage = Image<std::complex<typename TInputImage::PixelType>, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT VnlForward1DFFTImageFilter : public Forward1DFFTImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VnlForward1DFFTImageFilter);

  /** Standard class type alias. */
  using Self = VnlForward1DFFTImageFilter;
  using Superclass = Forward1DFFTImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputImageType = typename Superclass::InputImageType;
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VnlForward1DFFTImageFilter, Forward1DFFTImageFilter);

protected:
  void
  GenerateData() override;

  VnlForward1DFFTImageFilter() = default;
  ~VnlForward1DFFTImageFilter() override = default;

private:
};

// Describe whether input/output are real- or complex-valued
// for factory registration
template <>
struct FFTImageFilterTraits<VnlForward1DFFTImageFilter>
{
  template <typename TUnderlying>
  using InputPixelType = TUnderlying;
  template <typename TUnderlying>
  using OutputPixelType = std::complex<TUnderlying>;
  using FilterDimensions = std::integer_sequence<unsigned int, 4, 3, 2, 1>;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVnlForward1DFFTImageFilter.hxx"
#endif

#endif

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
#ifndef itkPocketFFTComplexToComplexFFTImageFilter_h
#define itkPocketFFTComplexToComplexFFTImageFilter_h

#include "itkComplexToComplexFFTImageFilter.h"
#include "itkFFTImageFilterFactory.h"

namespace itk
{
/**
 * \class PocketFFTComplexToComplexFFTImageFilter
 *
 * \brief PocketFFT-based complex-to-complex Fast Fourier Transform.
 *
 * Supports images of any size in each dimension.
 *
 * \ingroup FourierTransform
 *
 * \sa ComplexToComplexFFTImageFilter
 * \ingroup ITKFFT
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT PocketFFTComplexToComplexFFTImageFilter
  : public ComplexToComplexFFTImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PocketFFTComplexToComplexFFTImageFilter);

  using Self = PocketFFTComplexToComplexFFTImageFilter;
  using Superclass = ComplexToComplexFFTImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using typename Superclass::ImageType;
  using PixelType = typename ImageType::PixelType;
  using typename Superclass::InputImageType;
  using typename Superclass::OutputImageType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  itkNewMacro(Self);

  itkOverrideGetNameOfClassMacro(PocketFFTComplexToComplexFFTImageFilter);

  static constexpr unsigned int ImageDimension = ImageType::ImageDimension;

protected:
  PocketFFTComplexToComplexFFTImageFilter() = default;
  ~PocketFFTComplexToComplexFFTImageFilter() override = default;

  void
  GenerateData() override;
};

// Describe whether input/output are real- or complex-valued
// for factory registration
template <>
struct FFTImageFilterTraits<PocketFFTComplexToComplexFFTImageFilter>
{
  template <typename TUnderlying>
  using InputPixelType = std::complex<TUnderlying>;
  template <typename TUnderlying>
  using OutputPixelType = std::complex<TUnderlying>;
  using FilterDimensions = std::integer_sequence<unsigned int, 4, 3, 2, 1>;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPocketFFTComplexToComplexFFTImageFilter.hxx"
#endif

#endif

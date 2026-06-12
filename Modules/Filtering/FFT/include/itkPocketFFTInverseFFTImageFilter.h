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
#ifndef itkPocketFFTInverseFFTImageFilter_h
#define itkPocketFFTInverseFFTImageFilter_h

#include "itkInverseFFTImageFilter.h"
#include "itkFFTImageFilterFactory.h"

namespace itk
{
/**
 * \class PocketFFTInverseFFTImageFilter
 *
 * \brief PocketFFT-based inverse Fast Fourier Transform.
 *
 * Supports images of any size in each dimension.
 *
 * \ingroup FourierTransform
 *
 * \sa InverseFFTImageFilter
 * \ingroup ITKFFT
 */
template <typename TInputImage,
          typename TOutputImage = Image<typename TInputImage::PixelType::value_type, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT PocketFFTInverseFFTImageFilter : public InverseFFTImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PocketFFTInverseFFTImageFilter);

  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;
  using OutputSizeType = typename OutputImageType::SizeType;

  using Self = PocketFFTInverseFFTImageFilter;
  using Superclass = InverseFFTImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);

  itkOverrideGetNameOfClassMacro(PocketFFTInverseFFTImageFilter);

  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  [[nodiscard]] SizeValueType
  GetSizeGreatestPrimeFactor() const override;

  itkConceptMacro(ImageDimensionsMatchCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));

protected:
  PocketFFTInverseFFTImageFilter() = default;
  ~PocketFFTInverseFFTImageFilter() override = default;

  void
  GenerateData() override;
};

// Describe whether input/output are real- or complex-valued
// for factory registration
template <>
struct FFTImageFilterTraits<PocketFFTInverseFFTImageFilter>
{
  template <typename TUnderlying>
  using InputPixelType = std::complex<TUnderlying>;
  template <typename TUnderlying>
  using OutputPixelType = TUnderlying;
  using FilterDimensions = std::integer_sequence<unsigned int, 4, 3, 2, 1>;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPocketFFTInverseFFTImageFilter.hxx"
#endif

#endif

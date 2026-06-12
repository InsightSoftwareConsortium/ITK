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
#ifndef itkPocketFFTRealToHalfHermitianForwardFFTImageFilter_h
#define itkPocketFFTRealToHalfHermitianForwardFFTImageFilter_h

#include "itkRealToHalfHermitianForwardFFTImageFilter.h"
#include "itkFFTImageFilterFactory.h"

namespace itk
{
/**
 * \class PocketFFTRealToHalfHermitianForwardFFTImageFilter
 *
 * \brief PocketFFT-based real-to-half-Hermitian forward Fast Fourier Transform.
 *
 * Uses the native pocketfft r2c transform; supports images of any size.
 *
 * \ingroup FourierTransform
 *
 * \sa RealToHalfHermitianForwardFFTImageFilter
 * \ingroup ITKFFT
 */
template <typename TInputImage,
          typename TOutputImage = Image<std::complex<typename TInputImage::PixelType>, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT PocketFFTRealToHalfHermitianForwardFFTImageFilter
  : public RealToHalfHermitianForwardFFTImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PocketFFTRealToHalfHermitianForwardFFTImageFilter);

  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using InputSizeType = typename InputImageType::SizeType;
  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;
  using OutputSizeType = typename OutputImageType::SizeType;

  using Self = PocketFFTRealToHalfHermitianForwardFFTImageFilter;
  using Superclass = RealToHalfHermitianForwardFFTImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);

  itkOverrideGetNameOfClassMacro(PocketFFTRealToHalfHermitianForwardFFTImageFilter);

  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  [[nodiscard]] SizeValueType
  GetSizeGreatestPrimeFactor() const override;

  itkConceptMacro(ImageDimensionsMatchCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));

protected:
  PocketFFTRealToHalfHermitianForwardFFTImageFilter() = default;
  ~PocketFFTRealToHalfHermitianForwardFFTImageFilter() override = default;

  void
  GenerateData() override;
};

// Describe whether input/output are real- or complex-valued
// for factory registration
template <>
struct FFTImageFilterTraits<PocketFFTRealToHalfHermitianForwardFFTImageFilter>
{
  template <typename TUnderlying>
  using InputPixelType = TUnderlying;
  template <typename TUnderlying>
  using OutputPixelType = std::complex<TUnderlying>;
  using FilterDimensions = std::integer_sequence<unsigned int, 4, 3, 2, 1>;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPocketFFTRealToHalfHermitianForwardFFTImageFilter.hxx"
#endif

#endif

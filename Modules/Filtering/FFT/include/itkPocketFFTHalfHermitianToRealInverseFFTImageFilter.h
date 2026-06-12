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
#ifndef itkPocketFFTHalfHermitianToRealInverseFFTImageFilter_h
#define itkPocketFFTHalfHermitianToRealInverseFFTImageFilter_h

#include "itkHalfHermitianToRealInverseFFTImageFilter.h"
#include "itkFFTImageFilterFactory.h"

namespace itk
{
/**
 * \class PocketFFTHalfHermitianToRealInverseFFTImageFilter
 *
 * \brief PocketFFT-based half-Hermitian-to-real inverse Fast Fourier Transform.
 *
 * Uses the native pocketfft c2r transform; supports images of any size.
 *
 * \ingroup FourierTransform
 *
 * \sa HalfHermitianToRealInverseFFTImageFilter
 * \ingroup ITKFFT
 */
template <typename TInputImage,
          typename TOutputImage = Image<typename TInputImage::PixelType::value_type, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT PocketFFTHalfHermitianToRealInverseFFTImageFilter
  : public HalfHermitianToRealInverseFFTImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PocketFFTHalfHermitianToRealInverseFFTImageFilter);

  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using InputSizeType = typename InputImageType::SizeType;
  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;
  using OutputSizeType = typename OutputImageType::SizeType;

  using Self = PocketFFTHalfHermitianToRealInverseFFTImageFilter;
  using Superclass = HalfHermitianToRealInverseFFTImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  itkNewMacro(Self);

  itkOverrideGetNameOfClassMacro(PocketFFTHalfHermitianToRealInverseFFTImageFilter);

  static constexpr unsigned int ImageDimension = TOutputImage::ImageDimension;
  static constexpr unsigned int InputImageDimension = TInputImage::ImageDimension;
  static constexpr unsigned int OutputImageDimension = TOutputImage::ImageDimension;

  [[nodiscard]] SizeValueType
  GetSizeGreatestPrimeFactor() const override;

  itkConceptMacro(ImageDimensionsMatchCheck, (Concept::SameDimension<InputImageDimension, OutputImageDimension>));

protected:
  PocketFFTHalfHermitianToRealInverseFFTImageFilter() = default;
  ~PocketFFTHalfHermitianToRealInverseFFTImageFilter() override = default;

  void
  GenerateData() override;
};

// Describe whether input/output are real- or complex-valued
// for factory registration
template <>
struct FFTImageFilterTraits<PocketFFTHalfHermitianToRealInverseFFTImageFilter>
{
  template <typename TUnderlying>
  using InputPixelType = std::complex<TUnderlying>;
  template <typename TUnderlying>
  using OutputPixelType = TUnderlying;
  using FilterDimensions = std::integer_sequence<unsigned int, 4, 3, 2, 1>;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPocketFFTHalfHermitianToRealInverseFFTImageFilter.hxx"
#endif

#endif

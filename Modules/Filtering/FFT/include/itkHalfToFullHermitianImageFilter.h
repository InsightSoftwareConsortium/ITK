/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkHalfToFullHermitianImageFilter_h
#define itkHalfToFullHermitianImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
/**
 *\class HalfToFullHermitianImageFilter
 *
 * \brief Expands a half image produced from a real-to-complex
 * discrete Fourier transform (DFT) to the full complex image.
 *
 * The subclasses of RealToHalfHermitianForwardFFTImageFilter produce only
 * the non-redundant half of the image resulting from a
 * real-to-complex DFT. This filter takes the non-redundant half image
 * and generates the full complex image that includes the redundant
 * half. It requires additional information about the output image
 * size, namely, whether the size in the first dimension of the output
 * image is odd.
 *
 * \ingroup FourierTransform
 *
 * \sa RealToHalfHermitianForwardFFTImageFilter
 * \ingroup ITKFFT
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT HalfToFullHermitianImageFilter : public ImageToImageFilter<TInputImage, TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(HalfToFullHermitianImageFilter);

  /** Standard class type aliases. */
  using InputImageType = TInputImage;
  using InputImagePixelType = typename InputImageType::PixelType;
  using InputImageIndexType = typename InputImageType::IndexType;
  using InputImageIndexValueType = typename InputImageType::IndexValueType;
  using InputImageSizeType = typename InputImageType::SizeType;
  using InputImageSizeValueType = typename InputImageType::SizeValueType;
  using InputImageRegionType = typename InputImageType::RegionType;
  using OutputImageType = TInputImage;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using OutputImageIndexType = typename OutputImageType::IndexType;
  using OutputImageIndexValueType = typename OutputImageType::IndexValueType;
  using OutputImageSizeType = typename OutputImageType::SizeType;
  using OutputImageSizeValueType = typename OutputImageType::SizeValueType;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  using Self = HalfToFullHermitianImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(HalfToFullHermitianImageFilter, ImageToImageFilter);

  /** Extract the dimensionality of the input and output images. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Was the original truncated dimension size in the x-dimension odd? */
  itkSetGetDecoratedInputMacro(ActualXDimensionIsOdd, bool);
  itkBooleanMacro(ActualXDimensionIsOdd);

protected:
  HalfToFullHermitianImageFilter();
  ~HalfToFullHermitianImageFilter() override = default;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  /** The output is a different size from the input. */
  void
  GenerateOutputInformation() override;

  /** This class requires the entire input. */
  void
  GenerateInputRequestedRegion() override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHalfToFullHermitianImageFilter.hxx"
#endif

#endif // itkHalfToFullHermitianImageFilter_h

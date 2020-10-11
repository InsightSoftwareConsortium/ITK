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
#ifndef itkFullToHalfHermitianImageFilter_h
#define itkFullToHalfHermitianImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
/**
 *\class FullToHalfHermitianImageFilter
 *
 * \brief Reduces the size of a full complex image produced from a
 * forward discrete Fourier transform of a real image to only the
 * non-redundant half of the image.
 *
 * In particular, this filter reduces the size of the image in the
 * first dimension to \f$\lfloor N/2 \rfloor + 1 \f$.
 *
 * \ingroup FourierTransform
 *
 * \sa HalfToFullHermitianImageFilter
 * \sa ForwardFFTImageFilter
 * \sa InverseFFTImageFilter
 * \sa RealToHalfHermitianForwardFFTImageFilter
 * \sa HalfHermitianToRealInverseFFTImageFilter
 * \ingroup ITKFFT
 */
template <typename TInputImage>
class ITK_TEMPLATE_EXPORT FullToHalfHermitianImageFilter : public ImageToImageFilter<TInputImage, TInputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FullToHalfHermitianImageFilter);

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

  using Self = FullToHalfHermitianImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TInputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FullToHalfHermitianImageFilter, ImageToImageFilter);

  /** Extract the dimensionality of the input and output images. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Get whether the actual X dimension of the image is odd or not in the full
   * representation */
  itkGetDecoratedOutputMacro(ActualXDimensionIsOdd, bool);

protected:
  FullToHalfHermitianImageFilter();
  ~FullToHalfHermitianImageFilter() override = default;

  void
  DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread) override;


  /** The output is a different size from the input. */
  void
  GenerateOutputInformation() override;

  /** This class requires the entire input. */
  void
  GenerateInputRequestedRegion() override;

  itkSetDecoratedOutputMacro(ActualXDimensionIsOdd, bool);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFullToHalfHermitianImageFilter.hxx"
#endif

#endif // itkFullToHalfHermitianImageFilter_h

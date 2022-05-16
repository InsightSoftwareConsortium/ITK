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
#ifndef itkInverse1DFFTImageFilter_h
#define itkInverse1DFFTImageFilter_h

#include <complex>

#include "itkImageToImageFilter.h"
#include "itkMacro.h"

namespace itk
{
/** \class Inverse1DFFTImageFilter
 * \brief Perform the Fast Fourier Transform, in the reverse direction, with
 * real output, but only along one dimension.
 *
 * \ingroup ITKFFT
 * \ingroup FourierTransform
 */
template <typename TInputImage,
          typename TOutputImage =
            Image<typename NumericTraits<typename TInputImage::PixelType>::ValueType, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT Inverse1DFFTImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(Inverse1DFFTImageFilter);

  /** Standard class type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using OutputImageRegionType = typename OutputImageType::RegionType;

  using Self = Inverse1DFFTImageFilter;
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Dimension of the underlying image. */
  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  itkTypeMacro(Inverse1DFFTImageFilter, ImageToImageFilter);

  /** Customized object creation methods that support configuration-based
   * selection of FFT implementation.
   *
   * Default implementation is VnlFFT1D.
   */
  itkFactoryOnlyNewMacro(Self);

  /** Get the direction in which the filter is to be applied. */
  itkGetConstMacro(Direction, unsigned int);

  /** Set the direction in which the filter is to be applied. */
  itkSetClampMacro(Direction, unsigned int, 0, InputImageType::ImageDimension - 1);

  /** Get the greatest supported prime factor. */
  virtual SizeValueType
  GetSizeGreatestPrimeFactor() const
  {
    return 2;
  }

protected:
  Inverse1DFFTImageFilter();
  ~Inverse1DFFTImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateInputRequestedRegion() override;
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  /** Direction in which the filter is to be applied
   * this should be in the range [0,ImageDimension-1]. */
  unsigned int m_Direction{ 0 };

private:
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkInverse1DFFTImageFilter.hxx"
#endif

#ifdef ITK_FFTIMAGEFILTERINIT_FACTORY_REGISTER_MANAGER
#  include "itkFFTImageFilterInitFactoryRegisterManager.h"
#endif

#endif // itkInverse1DFFTImageFilter_h

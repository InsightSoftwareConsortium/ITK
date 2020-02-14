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
#ifndef itkInverseFFTImageFilter_h
#define itkInverseFFTImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class InverseFFTImageFilter
 *
 * \brief Base class for inverse Fast Fourier Transform.
 *
 * This is a base class for the "inverse" or "reverse" Discrete Fourier
 * Transform.  This is an abstract base class: the actual implementation is
 * provided by the best child available on the system when the object is
 * created via the object factory system.
 *
 * This class transforms a full complex image with Hermitian symmetry into
 * its real spatial domain representation.  If the input does not have
 * Hermitian symmetry, the imaginary component is discarded.
 *
 * \ingroup FourierTransform
 *
 * \sa ForwardFFTImageFilter, InverseFFTImageFilter
 * \ingroup ITKFFT
 *
 * \sphinx
 * \sphinxexample{Filtering/FFT/ComputeInverseFFTOfImage,Compute Inverse FFT Of Image}
 * \endsphinx
 */
template <typename TInputImage,
          typename TOutputImage = Image<typename TInputImage::PixelType::value_type, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT InverseFFTImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>

{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(InverseFFTImageFilter);

  /** Standard class type aliases. */
  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;

  using Self = InverseFFTImageFilter;
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

  /** Customized object creation methods that support configuration-based
   * selection of FFT implementation.
   *
   * Default implementation is VnlFFT. */
  static Pointer
  New();

  /* Return the preferred greatest prime factor supported for the input image
   * size. Defaults to 2 as many implementations work only for sizes that are
   * power of 2.
   */
  virtual SizeValueType
  GetSizeGreatestPrimeFactor() const;

protected:
  InverseFFTImageFilter() = default;
  ~InverseFFTImageFilter() override = default;

  /** This class requires the entire input. */
  void
  GenerateInputRequestedRegion() override;

  /** Sets the output requested region to the largest possible output
   * region. */
  void
  EnlargeOutputRequestedRegion(DataObject * itkNotUsed(output)) override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkInverseFFTImageFilter.hxx"
#endif

#endif

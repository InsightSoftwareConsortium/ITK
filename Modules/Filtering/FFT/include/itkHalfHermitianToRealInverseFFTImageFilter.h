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
#ifndef itkHalfHermitianToRealInverseFFTImageFilter_h
#define itkHalfHermitianToRealInverseFFTImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
/**
 *\class HalfHermitianToRealInverseFFTImageFilter
 *
 * \brief Base class for specialized complex-to-real inverse Fast Fourier Transform.
 *
 * This is a base class for the "inverse" or "reverse" Discrete
 * Fourier Transform.  This is an abstract base class: the actual
 * implementation is provided by the best child class available on the
 * system when the object is created via the object factory system.
 *
 * The input to this filter is assumed to have the same format as the
 * output of the RealToHalfHermitianForwardFFTImageFilter. That is, the
 * input is assumed to consist of roughly half the full complex image
 * resulting from a real-to-complex discrete Fourier transform. This
 * half is expected to be the first half of the image in the
 * X-dimension. Because this filter assumes that the input stores only
 * about half of the non-redundant complex pixels, the output is
 * larger in the X-dimension than it is in the input. To determine the
 * actual size of the output image, this filter needs additional
 * information in the form of a flag indicating whether the output
 * image has an odd size in the X-dimension. Use
 * SetActualXDimensionIsOdd() to set this flag.
 *
 * \ingroup FourierTransform
 *
 * \sa ForwardFFTImageFilter, HalfHermitianToRealInverseFFTImageFilter
 * \ingroup ITKFFT
 */
template <typename TInputImage,
          typename TOutputImage = Image<typename TInputImage::PixelType::value_type, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT HalfHermitianToRealInverseFFTImageFilter
  : public ImageToImageFilter<TInputImage, TOutputImage>

{
public:
  ITK_DISALLOW_COPY_AND_MOVE(HalfHermitianToRealInverseFFTImageFilter);

  /** Standard class type aliases. */
  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;

  using Self = HalfHermitianToRealInverseFFTImageFilter;
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

  /** Was the original truncated dimension size odd? */
  itkSetGetDecoratedInputMacro(ActualXDimensionIsOdd, bool);
  itkBooleanMacro(ActualXDimensionIsOdd);

  /* Return the preferred greatest prime factor supported for the input image
   * size. Defaults to 2 as many implementations work only for sizes that are
   * power of 2.
   */
  virtual SizeValueType
  GetSizeGreatestPrimeFactor() const;

protected:
  HalfHermitianToRealInverseFFTImageFilter();
  ~HalfHermitianToRealInverseFFTImageFilter() override = default;

  /** The output may be a different size from the input if complex conjugate
   * symmetry is implicit. */
  void
  GenerateOutputInformation() override;

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
#  include "itkHalfHermitianToRealInverseFFTImageFilter.hxx"
#endif

#endif

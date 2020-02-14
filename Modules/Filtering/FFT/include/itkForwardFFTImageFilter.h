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
#ifndef itkForwardFFTImageFilter_h
#define itkForwardFFTImageFilter_h

#include "itkImageToImageFilter.h"

namespace itk
{
/**
 *\class ForwardFFTImageFilter
 *
 * \brief Base class for forward Fast Fourier Transform.
 *
 * This is a base class for the "forward" or "direct" discrete Fourier
 * Transform.  This is an abstract base class: the actual
 * implementation is provided by the best child class available on the
 * system when the object is created via the object factory system.
 *
 * This class transforms a real input image into its full complex Fourier
 * transform.  The Fourier transform of a real input image has
 * Hermitian symmetry: \f$ f(\mathbf{x}) = f^*(-\mathbf{x}) \f$. That
 * is, when the result of the transform is split in half along the
 * x-dimension, the values in the second half of the transform are the
 * complex conjugates of values in the first half reflected about the
 * center of the image in each dimension.
 *
 * This filter works only for real single-component input image types.
 *
 * The output generated from a ForwardFFTImageFilter is in the
 * dual space or frequency domain.
 * Refer to FrequencyFFTLayoutImageRegionConstIteratorWithIndex
 * for a description of the layout of frequencies generated after a forward FFT.
 * Also see ITKImageFrequency for a set of filters requiring input images in the frequency domain.
 *
 * \ingroup FourierTransform
 *
 * \sa InverseFFTImageFilter, FFTComplexToComplexImageFilter
 * \ingroup ITKFFT
 *
 * \sphinx
 * \sphinxexample{Filtering/FFT/ComputeForwardFFT,Compute Forward FFT}
 * \endsphinx
 */
template <typename TInputImage,
          typename TOutputImage = Image<std::complex<typename TInputImage::PixelType>, TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT ForwardFFTImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ForwardFFTImageFilter);

  /** Standard class type aliases. */
  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using InputIndexType = typename InputImageType::IndexType;
  using InputSizeType = typename InputImageType::SizeType;
  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;
  using OutputIndexType = typename OutputImageType::IndexType;
  using OutputSizeType = typename OutputIndexType::SizeType;

  using Self = ForwardFFTImageFilter;
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

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
  ForwardFFTImageFilter() = default;
  ~ForwardFFTImageFilter() override = default;

  /** This class requires the entire input. */
  void
  GenerateInputRequestedRegion() override;

  /** This class produces the entire output. */
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkForwardFFTImageFilter.hxx"
#endif

#endif

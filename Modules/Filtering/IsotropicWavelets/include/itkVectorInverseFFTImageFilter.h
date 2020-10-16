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
#ifndef itkVectorInverseFFTImageFilter_h
#define itkVectorInverseFFTImageFilter_h

#include "itkVectorImage.h"
#include "itkInverseFFTImageFilter.h"

namespace itk
{
/** \class VectorInverseFFTImageFilter
 *
 * Applies InverseFFT to each index of a vector image.
 *
 * This class transforms a full complex image with Hermitian symmetry into
 * its real spatial domain representation.  If the input does not have
 * Hermitian symmetry, the imaginary component is discarded.
 *
 * The default output assumes input image is vector<complex<float|double>>
 *
 * \ingroup FourierTransform
 *
 * \sa ForwardFFTImageFilter, InverseFFTImageFilter
 * \ingroup ITKFFT
 * \ingroup IsotropicWavelets
 */
template <typename TInputImage,
          typename TOutputImage =
            VectorImage<typename TInputImage::PixelType::ComponentType::value_type, TInputImage::ImageDimension>>
class VectorInverseFFTImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VectorInverseFFTImageFilter);

  /** Standard class type alias. */
  using InputImageType = TInputImage;
  using InputPixelType = typename InputImageType::PixelType;
  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;

  using Self = VectorInverseFFTImageFilter;
  using Superclass = ImageToImageFilter<InputImageType, OutputImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(VectorInverseFFTImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = InputImageType::ImageDimension;

protected:
  VectorInverseFFTImageFilter() = default;
  ~VectorInverseFFTImageFilter() override = default;

  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVectorInverseFFTImageFilter.hxx"
#endif

#endif

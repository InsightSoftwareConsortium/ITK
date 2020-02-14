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
#ifndef itkFFTShiftImageFilter_h
#define itkFFTShiftImageFilter_h

#include "itkCyclicShiftImageFilter.h"

namespace itk
{
/**
 *\class FFTShiftImageFilter
 * \brief Shift the zero-frequency components of a Fourier transform
 * to the center of the image.
 *
 * The Fourier transform produces an image where the zero frequency
 * components are in the corner of the image, making it difficult to
 * understand. This filter shifts the component to the center of the
 * image.
 *
 * \note For images with an odd-sized dimension, applying this filter
 * twice will not produce the same image as the original one without
 * using SetInverse(true) on one (and only one) of the two filters.
 *
 * https://hdl.handle.net/1926/321
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa ForwardFFTImageFilter, InverseFFTImageFilter
 *
 * \ingroup FourierTransform
 * \ingroup ITKFFT
 */
template <typename TInputImage, typename TOutputImage>
class ITK_TEMPLATE_EXPORT FFTShiftImageFilter : public CyclicShiftImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(FFTShiftImageFilter);

  /** Standard class type aliases. */
  using Self = FFTShiftImageFilter;
  using Superclass = CyclicShiftImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using OutputImageType = TOutputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;
  using OutputImagePointer = typename OutputImageType::Pointer;
  using OutputImageConstPointer = typename OutputImageType::ConstPointer;
  using OutputImageRegionType = typename OutputImageType::RegionType;
  using OutputImagePixelType = typename OutputImageType::PixelType;
  using IndexType = typename OutputImageType::IndexType;
  using SizeType = typename OutputImageType::SizeType;

  /** ImageDimension constants */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(FFTShiftImageFilter, CyclicShiftImageFilter);

  /** Set/Get whether the filter must invert the transform or not.
   * This option has no effect if none of the size of the input image is even,
   * but is required to restore the original image if at least one
   * of the dimensions has an odd size. */
  itkSetMacro(Inverse, bool);
  itkGetConstReferenceMacro(Inverse, bool);
  itkBooleanMacro(Inverse);

protected:
  FFTShiftImageFilter();
  ~FFTShiftImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Override GenerateData method to set some parameters in the
   * superclass. */
  void
  GenerateData() override;

private:
  bool m_Inverse;

}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFFTShiftImageFilter.hxx"
#endif

#endif

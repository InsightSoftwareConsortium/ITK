/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
 * \brief Applies InverseFFT to each index of a vector image.
 *
 * This class transforms a full complex image with Hermitian symmetry into
 * its real spatial domain representation.  If the input does not have
 * Hermitian symmetry, the imaginary component is discarded.
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
// the default output assumes input image is vector<complex<float|double>>
class VectorInverseFFTImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef TInputImage                         InputImageType;
  typedef typename InputImageType::PixelType  InputPixelType;
  typedef TOutputImage                        OutputImageType;
  typedef typename OutputImageType::PixelType OutputPixelType;

  typedef VectorInverseFFTImageFilter                         Self;
  typedef ImageToImageFilter<InputImageType, OutputImageType> Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FrequencyExpandImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int, InputImageType::ImageDimension);

protected:
  VectorInverseFFTImageFilter() {}
  virtual ~VectorInverseFFTImageFilter() {}
  virtual void
  GenerateData() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(VectorInverseFFTImageFilter);
};
} // end namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVectorInverseFFTImageFilter.hxx"
#endif

#endif

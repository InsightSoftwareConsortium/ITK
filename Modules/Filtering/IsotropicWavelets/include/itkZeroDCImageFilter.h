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
#ifndef itkZeroDCImageFilter_h
#define itkZeroDCImageFilter_h

#include <itkImageToImageFilter.h>

namespace itk
{
/** \class ZeroDCImageFilter
 * \brief Set the DC component of the image to Zero.
 *
 * The DC component corresponds to the zero index after a forward Fourier transform (FFT) of the image.
 *
 * There are a few methods to set DC to zero, the one implemented here is to subtract the mean value of the input image
 * to all the pixels.
 *
 * Note that this filter works on the spatial domain, and has to be called before the FFT.
 *
 * \sa ForwardFFTImageFilter
 * \ingroup IsotropicWavelets
 */
template <typename TImageType>
class ZeroDCImageFilter : public ImageToImageFilter<TImageType, TImageType>
{
public:
  /** Standard class typedefs. */
  typedef ZeroDCImageFilter                          Self;
  typedef ImageToImageFilter<TImageType, TImageType> Superclass;
  typedef SmartPointer<Self>                         Pointer;
  typedef SmartPointer<const Self>                   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ZeroDCImageFilter, ImageToImageFilter);

  /** Typedef to describe the output image region type. */
  typedef typename TImageType::RegionType ImageRegionType;

  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int, TImageType::ImageDimension);

  /** Inherit some types from superclass. */
  typedef typename Superclass::InputImageType ImageType;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::Pointer         ImagePointer;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(ImageTypeHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TImageType::PixelType>));
  // End concept checking
#endif

protected:
  ZeroDCImageFilter();
  virtual ~ZeroDCImageFilter() {}

  void
  GenerateData() ITK_OVERRIDE;

private:
  ZeroDCImageFilter(const Self &) ITK_DELETE_FUNCTION;
  void
  operator=(const Self &) ITK_DELETE_FUNCTION;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkZeroDCImageFilter.hxx"
#endif

#endif

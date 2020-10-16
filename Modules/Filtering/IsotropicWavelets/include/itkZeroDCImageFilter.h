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
#ifndef itkZeroDCImageFilter_h
#define itkZeroDCImageFilter_h

#include <itkImageToImageFilter.h>
#include <itkSubtractImageFilter.h>
#include <itkStatisticsImageFilter.h>

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
  ITK_DISALLOW_COPY_AND_MOVE(ZeroDCImageFilter);

  /** Standard class type alias. */
  using Self = ZeroDCImageFilter;
  using Superclass = ImageToImageFilter<TImageType, TImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ZeroDCImageFilter, ImageToImageFilter);

  /** Typedef to describe the output image region type. */
  using ImageRegionType = typename TImageType::RegionType;

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = TImageType::ImageDimension;

  /** Inherit some types from superclass. */
  using ImageType = typename Superclass::InputImageType;
  using PixelType = typename ImageType::PixelType;
  using ImagePointer = typename ImageType::Pointer;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(ImageTypeHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TImageType::PixelType>));
  // End concept checking
#endif
  using StatisticsFilterType = itk::StatisticsImageFilter<TImageType>;
  using SubtractFilterType = itk::SubtractImageFilter<ImageType>;
  using RealType = typename StatisticsFilterType::RealType;
  RealType
  GetMean() const
  {
    return m_StatisticsFilter->GetMean();
  }

protected:
  ZeroDCImageFilter();
  ~ZeroDCImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

private:
  typename StatisticsFilterType::Pointer m_StatisticsFilter;
  typename SubtractFilterType::Pointer   m_SubtractFilter;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkZeroDCImageFilter.hxx"
#endif

#endif

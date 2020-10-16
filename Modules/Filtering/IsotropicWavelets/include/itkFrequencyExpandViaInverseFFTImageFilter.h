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
#ifndef itkFrequencyExpandViaInverseFFTImageFilter_h
#define itkFrequencyExpandViaInverseFFTImageFilter_h

#include <itkImageToImageFilter.h>
#include <itkInverseFFTImageFilter.h>
#include <itkForwardFFTImageFilter.h>
#include <itkExpandWithZerosImageFilter.h>
#include "itkChangeInformationImageFilter.h"

namespace itk
{
/** \class FrequencyExpandViaInverseFFTImageFilter
 * \brief Expand the size of an image in the frequency domain by an integer
 * factor in each dimension.
 *
 * The input in the frequency domain is converted to the spatial domain using an
 * inverseFFT, then a ExpandWithZerosImageFilter is used, and the output is converted back
 * to the frequency domain with a ForwardFFT.
 *
 * FrequencyExpandViaInverseFFTImageFilter increases the size of an image in the frequency domain
 * (for example, the output of a FFTImageFilter), by an integer
 * factor in each dimension.
 * The output image size in each dimension is given by:
 *
 * OutputSize[j] = InputSize[j] * ExpandFactors[j]
 *
 * This code was contributed in the Insight Journal paper:
 * https://hdl.handle.net....
 *
 * \ingroup IsotropicWavelets
 */
template <typename TImageType>
class FrequencyExpandViaInverseFFTImageFilter : public ImageToImageFilter<TImageType, TImageType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FrequencyExpandViaInverseFFTImageFilter);

  /** Standard class type alias. */
  using Self = FrequencyExpandViaInverseFFTImageFilter;
  using Superclass = ImageToImageFilter<TImageType, TImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FrequencyExpandViaInverseFFTImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration. */
  static constexpr unsigned int ImageDimension = TImageType::ImageDimension;

  /** Inherit some types from superclass. */
  using ImageType = typename Superclass::InputImageType;
  using PixelType = typename ImageType::PixelType;
  using ImagePointer = typename ImageType::Pointer;
  /** Typedef to describe the output image region type. */
  using ImageRegionType = typename TImageType::RegionType;
  /** Typedef of used filters */
  using InverseFFTFilterType = itk::InverseFFTImageFilter<ImageType>;
  using ExpandFilterType = itk::ExpandWithZerosImageFilter<typename InverseFFTFilterType::OutputImageType,
                                                           typename InverseFFTFilterType::OutputImageType>;
  using ForwardFFTFilterType = itk::ForwardFFTImageFilter<typename InverseFFTFilterType::OutputImageType, ImageType>;
  using ChangeInformationFilterType = itk::ChangeInformationImageFilter<ImageType>;

  /** The type of the expand factors representation */
  using ExpandFactorsType = FixedArray<unsigned int, ImageDimension>;

  /** Set the expand factors. Values are clamped to
   * a minimum value of 1. Default is 1 for all dimensions. */
  itkSetMacro(ExpandFactors, ExpandFactorsType);
  virtual void
  SetExpandFactors(const unsigned int factor);

  /** Get the expand factors. */
  itkGetConstReferenceMacro(ExpandFactors, ExpandFactorsType);

  /** FrequencyExpandViaInverseFFTImageFilter produces an image which is a different resolution and
   * with a different pixel spacing than its input image.  As such,
   * FrequencyExpandViaInverseFFTImageFilter needs to provide an implementation for
   * UpdateOutputInformation() in order to inform the pipeline execution model.
   * The original documentation of this method is below.
   * \sa ProcessObject::GenerateOutputInformaton() */
  void
  GenerateOutputInformation() override;

  /** FrequencyExpandViaInverseFFTImageFilter needs a smaller input requested region than the output
   * requested region.  As such, ShrinkImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform
   * the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(ImageTypeHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TImageType::PixelType>));
  // End concept checking
#endif

protected:
  FrequencyExpandViaInverseFFTImageFilter();
  ~FrequencyExpandViaInverseFFTImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;


  void
  GenerateData() override;

private:
  ExpandFactorsType                             m_ExpandFactors;
  typename InverseFFTFilterType::Pointer        m_InverseFFT;
  typename ForwardFFTFilterType::Pointer        m_ForwardFFT;
  typename ExpandFilterType::Pointer            m_Expander;
  typename ChangeInformationFilterType::Pointer m_ChangeInformation;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFrequencyExpandViaInverseFFTImageFilter.hxx"
#endif

#endif

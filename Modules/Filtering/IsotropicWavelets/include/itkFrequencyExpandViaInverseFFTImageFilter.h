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
#ifndef itkFrequencyExpandViaInverseFFTImageFilter_h
#define itkFrequencyExpandViaInverseFFTImageFilter_h

#include <itkImageToImageFilter.h>
#include <itkInverseFFTImageFilter.h>
#include <itkForwardFFTImageFilter.h>
#include <itkExpandWithZerosImageFilter.h>

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
  /** Standard class typedefs. */
  typedef FrequencyExpandViaInverseFFTImageFilter    Self;
  typedef ImageToImageFilter<TImageType, TImageType> Superclass;
  typedef SmartPointer<Self>                         Pointer;
  typedef SmartPointer<const Self>                   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FrequencyExpandViaInverseFFTImageFilter, ImageToImageFilter);

  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int, TImageType::ImageDimension);

  /** Inherit some types from superclass. */
  typedef typename Superclass::InputImageType ImageType;
  typedef typename ImageType::PixelType       PixelType;
  typedef typename ImageType::Pointer         ImagePointer;
  /** Typedef to describe the output image region type. */
  typedef typename TImageType::RegionType ImageRegionType;
  /** Typedef of used filters */
  typedef itk::InverseFFTImageFilter<ImageType> InverseFFTFilterType;
  typedef itk::ExpandWithZerosImageFilter<typename InverseFFTFilterType::OutputImageType,
                                          typename InverseFFTFilterType::OutputImageType>
                                                                                                ExpandFilterType;
  typedef itk::ForwardFFTImageFilter<typename InverseFFTFilterType::OutputImageType, ImageType> ForwardFFTFilterType;

  /** The type of the expand factors representation */
  typedef FixedArray<unsigned int, ImageDimension> ExpandFactorsType;

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
  virtual void
  GenerateOutputInformation() ITK_OVERRIDE;

  /** FrequencyExpandViaInverseFFTImageFilter needs a smaller input requested region than the output
   * requested region.  As such, ShrinkImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform
   * the pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void
  GenerateInputRequestedRegion() ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro(ImageTypeHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TImageType::PixelType>));
  // End concept checking
#endif

protected:
  FrequencyExpandViaInverseFFTImageFilter();
  ~FrequencyExpandViaInverseFFTImageFilter() {}
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void
  GenerateData() ITK_OVERRIDE;

private:
  FrequencyExpandViaInverseFFTImageFilter(const Self &) ITK_DELETE_FUNCTION;
  void
  operator=(const Self &) ITK_DELETE_FUNCTION;

  ExpandFactorsType                      m_ExpandFactors;
  typename InverseFFTFilterType::Pointer m_InverseFFT;
  typename ForwardFFTFilterType::Pointer m_ForwardFFT;
  typename ExpandFilterType::Pointer     m_Expander;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFrequencyExpandViaInverseFFTImageFilter.hxx"
#endif

#endif

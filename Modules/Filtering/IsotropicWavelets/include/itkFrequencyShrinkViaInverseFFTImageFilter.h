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
#ifndef itkFrequencyShrinkViaInverseFFTImageFilter_h
#define itkFrequencyShrinkViaInverseFFTImageFilter_h

#include <itkShrinkDecimateImageFilter.h>
#include <itkEnableIf.h>
#include <itkInverseFFTImageFilter.h>
#include <itkForwardFFTImageFilter.h>
#include <itkShrinkDecimateImageFilter.h>

namespace itk
{
/** \class FrequencyShrinkViaInverseFFTImageFilter
 * \brief Reduce the size of an image in the frequency domain by an integer
 * factor in each dimension.
 * This filter discard all the high frequency bins depending on the shrink factor
 * It performs an inverse FFT on input frequency image, and then a forward inverse.
 * The problem with other frequency shrinker is that it applies a low-pass rectangle filter (chop-off frequencies).
 * In the spatial domain this is equivalent to sinc interpolation (without any window), and it generates ringes
 * A way to improve this would be a frequency interpolator, avoiding ringing, not much frequency leakeage, and a sharp
 * cutoff to avoid losing much high frequency information.
 *
 * \ingroup IsotropicWavelets
 */
template <typename TImageType>
class FrequencyShrinkViaInverseFFTImageFilter : public ImageToImageFilter<TImageType, TImageType>
{
public:
  /** Standard class typedefs. */
  typedef FrequencyShrinkViaInverseFFTImageFilter    Self;
  typedef ImageToImageFilter<TImageType, TImageType> Superclass;
  typedef SmartPointer<Self>                         Pointer;
  typedef SmartPointer<const Self>                   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FrequencyShrinkViaInverseFFTImageFilter, ImageToImageFilter);

  /** Typedef to images */
  typedef TImageType                       ImageType;
  typedef typename ImageType::Pointer      ImagePointer;
  typedef typename ImageType::ConstPointer ImageConstPointer;
  typedef typename TImageType::IndexType   IndexType;
  typedef typename TImageType::PixelType   PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TImageType::RegionType ImageRegionType;
  /** Typedef of used filters */
  typedef itk::InverseFFTImageFilter<ImageType> InverseFFTFilterType;
  typedef itk::ShrinkDecimateImageFilter<typename InverseFFTFilterType::OutputImageType,
                                         typename InverseFFTFilterType::OutputImageType>
                                                                                                ShrinkFilterType;
  typedef itk::ForwardFFTImageFilter<typename InverseFFTFilterType::OutputImageType, ImageType> ForwardFFTFilterType;

  /** ImageDimension enumeration. */
  itkStaticConstMacro(ImageDimension, unsigned int, TImageType::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int, TImageType::ImageDimension);

  typedef FixedArray<unsigned int, ImageDimension> ShrinkFactorsType;

  /** Set the shrink factors. Values are clamped to
   * a minimum value of 1. Default is 1 for all dimensions. */
  itkSetMacro(ShrinkFactors, ShrinkFactorsType);
  void
  SetShrinkFactors(unsigned int factor);

  void
  SetShrinkFactor(unsigned int i, unsigned int factor);

  /** Get the shrink factors. */
  itkGetConstReferenceMacro(ShrinkFactors, ShrinkFactorsType);

  virtual void
  GenerateOutputInformation() ITK_OVERRIDE;

  /** FrequencyShrinkViaInverseFFTImageFilter needs a larger input requested region than the output
   * requested region.  As such, FrequencyShrinkViaInverseFFTImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to inform the
   * pipeline execution model.
   * \sa ProcessObject::GenerateInputRequestedRegion() */
  virtual void
  GenerateInputRequestedRegion() ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(ImageTypeHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TImageType::PixelType>));
  /** End concept checking */
#endif

protected:
  FrequencyShrinkViaInverseFFTImageFilter();
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void
  GenerateData() ITK_OVERRIDE;

private:
  FrequencyShrinkViaInverseFFTImageFilter(const Self &) ITK_DELETE_FUNCTION;
  void
  operator=(const Self &) ITK_DELETE_FUNCTION;

  ShrinkFactorsType                      m_ShrinkFactors;
  typename InverseFFTFilterType::Pointer m_InverseFFT;
  typename ForwardFFTFilterType::Pointer m_ForwardFFT;
  typename ShrinkFilterType::Pointer     m_Shrinker;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFrequencyShrinkViaInverseFFTImageFilter.hxx"
#endif

#endif

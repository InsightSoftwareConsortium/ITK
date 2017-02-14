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
#ifndef itkFrequencyBandPassImageFilter_h
#define itkFrequencyBandPassImageFilter_h

#include <itkImageToImageFilter.h>
#include <itkFrequencyImageRegionIteratorWithIndex.h>

namespace itk
{

/** \class FrequencyBandPassImageFilter
 * \brief Performs a pass filter based on the input frequency.
 *
 * \ingroup IsotropicWavelets
 */
template <typename TImageType>
class FrequencyBandPassImageFilter : public ImageToImageFilter<TImageType, TImageType>
{
public:
  /** Standard class typedefs. */
  typedef FrequencyBandPassImageFilter               Self;
  typedef ImageToImageFilter<TImageType, TImageType> Superclass;
  typedef SmartPointer<Self>                         Pointer;
  typedef SmartPointer<const Self>                   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FrequencyBandPassImageFilter, ImageToImageFilter);

  /** Typedef to images */
  typedef TImageType                       ImageType;
  typedef typename ImageType::Pointer      ImagePointer;
  typedef typename ImageType::ConstPointer ImageConstPointer;
  typedef typename TImageType::IndexType   IndexType;
  typedef typename TImageType::PixelType   PixelType;

  /** Typedef to describe the image region type. */
  typedef typename TImageType::RegionType ImageRegionType;

  itkStaticConstMacro(ImageDimension, unsigned int, TImageType::ImageDimension);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(ImageTypeHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TImageType::PixelType>));
  /** End concept checking */
#endif

  /** Frequency Iterator types */
  typedef FrequencyImageRegionIteratorWithIndex<TImageType>  FrequencyIteratorType;
  typedef typename FrequencyIteratorType::FrequencyValueType FrequencyValueType;

  /****** Frequency Threshold Getters/Setters *****/
  itkGetConstReferenceMacro(LowFrequencyThreshold, FrequencyValueType);
  itkGetConstReferenceMacro(HighFrequencyThreshold, FrequencyValueType);
  itkSetMacro(LowFrequencyThreshold, FrequencyValueType);
  void
  SetLowFrequencyThresholdInRadians(const FrequencyValueType & freq_low_in_radians);
  itkSetMacro(HighFrequencyThreshold, FrequencyValueType);
  void
  SetHighFrequencyThresholdInRadians(const FrequencyValueType & freq_high_in_radians);
  void
  SetFrequencyThresholds(const FrequencyValueType & freq_low, const FrequencyValueType & freq_high);
  void
  SetFrequencyThresholdsInRadians(const FrequencyValueType & freq_low_in_radians,
                                  const FrequencyValueType & freq_high_in_radians);

  itkSetMacro(PassBand, bool);
  itkGetConstReferenceMacro(PassBand, bool);
  void
  SetPassBand(const bool pass_low_threshold, const bool pass_high_threshold);
  void
  SetStopBand(const bool stop_low_threshold, const bool stop_high_threshold);
  itkSetMacro(PassLowFrequencyThreshold, bool);
  itkGetConstReferenceMacro(PassLowFrequencyThreshold, bool);
  itkSetMacro(PassHighFrequencyThreshold, bool);
  itkGetConstReferenceMacro(PassHighFrequencyThreshold, bool);

protected:
  FrequencyBandPassImageFilter();
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void
  BeforeThreadedGenerateData() ITK_OVERRIDE;
  virtual void
  ThreadedGenerateData(const ImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

private:
  FrequencyBandPassImageFilter(const Self &) ITK_DELETE_FUNCTION;
  void
  operator=(const Self &) ITK_DELETE_FUNCTION;

  /** Band range. Frequency thresholds in Hertz [0, 0.5] Hz by default. In rads: [0,pi]*/
  FrequencyValueType m_LowFrequencyThreshold;
  FrequencyValueType m_HighFrequencyThreshold;

  /// True: the band is a PassBand. False: StopBand
  bool m_PassBand;
  /** The pixel values that correspond to m_LowFrequencyThreshold are passed to the output image,
   * independent of m_PassBand */
  bool m_PassLowFrequencyThreshold;
  /** The pixel values that correspond to m_HighFrequencyThreshold are passed to the output image,
   * independent of m_PassBand */
  bool m_PassHighFrequencyThreshold;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFrequencyBandPassImageFilter.hxx"
#endif

#endif

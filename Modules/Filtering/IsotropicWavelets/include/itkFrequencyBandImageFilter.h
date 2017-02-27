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
#ifndef itkFrequencyBandImageFilter_h
#define itkFrequencyBandImageFilter_h

#include <itkImageToImageFilter.h>
#include <itkFrequencyImageRegionIteratorWithIndex.h>

namespace itk
{
/** \class FrequencyBandImageFilter
 * \brief Performs a frequency band filter in the range LowFrequencyThreshold and HighFrequencyThreshold.
 *
 * The default is a pass band between threshold frequencies [0,0.5] Hz or [0, pi] radians,
 * where both boundary values also pass (equivalent to SetPassBand(true,true)).
 * A pass band sets to zero any value outside the defined range, and let pass without modification the input image
 * inside the band.
 *
 * Instead, a stop band can be set between the threshold values.
 * In this case, the delimited band act as a stop band, setting values to zero in this range,
 * and don't modify input image values outside this range.
 * To set a stop band use SetPassBand(false), but more clarifying is to use
 * SetStopBand(bool, bool) that also control behaviour at band boundaries.
 *
 * Control of letting pass the boundaries of the band are controlled with
 * SetPassLow(High)FrequencyThreshold(bool). The default is to let pass low and high boundaries.
 * Also, SetPassBand(true, false), will let pass low boundary/threshold, and stop the high value.
 *
 * \ingroup IsotropicWavelets
 */
template <typename TImageType>
class FrequencyBandImageFilter : public ImageToImageFilter<TImageType, TImageType>
{
public:
  /** Standard class typedefs. */
  typedef FrequencyBandImageFilter                   Self;
  typedef ImageToImageFilter<TImageType, TImageType> Superclass;
  typedef SmartPointer<Self>                         Pointer;
  typedef SmartPointer<const Self>                   ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FrequencyBandImageFilter, ImageToImageFilter);

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
  /** Band range: Low threshold/boundary in Hertz */
  itkGetConstReferenceMacro(LowFrequencyThreshold, FrequencyValueType);
  itkSetMacro(LowFrequencyThreshold, FrequencyValueType);
  /**
   * Set low frequency threshold when input frequency is in radians.
   * @param freq_low_in_radians low freq in radians.
   */
  void
  SetLowFrequencyThresholdInRadians(const FrequencyValueType & freq_low_in_radians);

  /** Band range: High threshold/boundary in Hertz */
  itkGetConstReferenceMacro(HighFrequencyThreshold, FrequencyValueType);
  itkSetMacro(HighFrequencyThreshold, FrequencyValueType);
  void
  SetHighFrequencyThresholdInRadians(const FrequencyValueType & freq_high_in_radians);

  /**
   * Set LowFrequencyThreshold and HighFrequencyThreshold at the same time,
   * input frequencies in Hertz.
   *
   * @param freq_low in hertz.
   * @param freq_high in hertz.
   */
  void
  SetFrequencyThresholds(const FrequencyValueType & freq_low, const FrequencyValueType & freq_high);

  /**
   * Set LowFrequencyThreshold and HighFrequencyThreshold at the same time,
   * input frequencies in Radians. 1Hz = (1/2pi) rad
   *
   * @param freq_low in radians.
   * @param freq_high in radians.
   */
  void
  SetFrequencyThresholdsInRadians(const FrequencyValueType & freq_low_in_radians,
                                  const FrequencyValueType & freq_high_in_radians);

  /** The pixel values that correspond to m_LowFrequencyThreshold are passed to the output image,
   * independent of m_PassBand */
  itkSetMacro(PassLowFrequencyThreshold, bool);
  itkGetConstReferenceMacro(PassLowFrequencyThreshold, bool);
  itkBooleanMacro(PassLowFrequencyThreshold);

  /** The pixel values that correspond to m_HighFrequencyThreshold are passed to the output image,
   * independent of m_PassBand */
  itkSetMacro(PassHighFrequencyThreshold, bool);
  itkGetConstReferenceMacro(PassHighFrequencyThreshold, bool);
  itkBooleanMacro(PassHighFrequencyThreshold);

  /** True: the band is a PassBand. False: StopBand **/
  itkSetMacro(PassBand, bool);
  itkGetConstReferenceMacro(PassBand, bool);
  itkBooleanMacro(PassBand);

  /**
   * Utility method equivalent to:
   * SetPassBand(true)
   * SetPassLowFrequencyThreshold(pass_low_threshold)
   * SetPassHighFrequencyThreshold(pass_high_threshold)
   *
   * @param pass_low_threshold flag to let pass or not low boundary
   * @param pass_high_threshold flag to let pass or not high boundary
   */
  void
  SetPassBand(const bool pass_low_threshold, const bool pass_high_threshold);

  /**
   * Utility method equivalent to:
   * SetPassBand(false)
   * SetPassLowFrequencyThreshold(pass_low_threshold)
   * SetPassHighFrequencyThreshold(pass_high_threshold)
   *
   * @param pass_low_threshold flag to let pass or not low boundary
   * @param pass_high_threshold flag to let pass or not high boundary
   */
  void
  SetStopBand(const bool pass_low_threshold, const bool pass_high_threshold);

  /** If true the frequency cut-off uses the radius of the frequency vector. If false, it uses the max absolute value of
   * the frequency vector. */
  itkSetMacro(RadialBand, bool);
  itkGetConstReferenceMacro(RadialBand, bool);
  itkBooleanMacro(RadialBand);

protected:
  FrequencyBandImageFilter();
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void
  BeforeThreadedGenerateData() ITK_OVERRIDE;

  virtual void
  ThreadedGenerateData(const ImageRegionType & outputRegionForThread, ThreadIdType threadId) ITK_OVERRIDE;

private:
  FrequencyBandImageFilter(const Self &) ITK_DELETE_FUNCTION;
  void
  operator=(const Self &) ITK_DELETE_FUNCTION;

  FrequencyValueType m_LowFrequencyThreshold;
  FrequencyValueType m_HighFrequencyThreshold;

  bool m_PassBand;
  bool m_PassLowFrequencyThreshold;
  bool m_PassHighFrequencyThreshold;
  bool m_RadialBand;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFrequencyBandImageFilter.hxx"
#endif

#endif

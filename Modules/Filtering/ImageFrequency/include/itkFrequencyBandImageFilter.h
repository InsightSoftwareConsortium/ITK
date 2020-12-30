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
#ifndef itkFrequencyBandImageFilter_h
#define itkFrequencyBandImageFilter_h

#include <itkUnaryFrequencyDomainFilter.h>

namespace itk
{
/**
 *\class FrequencyBandImageFilter
 * \brief Performs a frequency band filtering on a frequency domain image
 *
 * The default filtering functor filters in the range LowFrequencyThreshold and
 * HighFrequencyThreshold - pass band between threshold frequencies [0,0.5] Hz or [0, pi] radians,
 * where both boundary values also pass (equivalent to SetPassBand(true,true)).
 * A pass band sets to zero any value outside the defined range, and let pass without modification the input image
 * inside the band.
 *
 * Instead, a stop band can be set between the threshold values.
 * In this case, the delimited band acts as a stop band, setting values to zero in this range,
 * and does not modify input image values outside this range.
 * Set a stop band using SetPassBand(false), but it is clearer to use
 * SetStopBand(bool, bool) that also control behaviour at band boundaries.
 *
 * The boundaries of the bands are controlled with
 * SetPassLow(High)FrequencyThreshold(bool). The default is to let pass low and high boundaries.
 * Also, SetPassBand(true, false), will let pass low boundary/threshold, and stop the high value.
 *
 * This filter derives from \ref UnaryFrequencyDomainFilter and implements its own custom functor.
 * If a different functor is set, the normal functionality of this filter will be lost.
 *
 * \sa UnaryFrequencyDomainFilter
 *
 * \ingroup ITKImageFrequency
 */
template <typename TImageType, typename TFrequencyIterator = FrequencyFFTLayoutImageRegionIteratorWithIndex<TImageType>>
class FrequencyBandImageFilter : public UnaryFrequencyDomainFilter<TImageType, TFrequencyIterator>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FrequencyBandImageFilter);

  /** Standard class type alias. */
  using Self = FrequencyBandImageFilter;
  using Superclass = UnaryFrequencyDomainFilter<TImageType, TFrequencyIterator>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FrequencyBandImageFilter, UnaryFrequencyDomainFilter);

  /** Typedef to images */
  using ImageType = TImageType;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using IndexType = typename TImageType::IndexType;
  using PixelType = typename TImageType::PixelType;

  /** Typedef to describe the image region type. */
  using ImageRegionType = typename TImageType::RegionType;

  static constexpr unsigned int ImageDimension = TImageType::ImageDimension;

  /** Frequency Iterator types */
  using FrequencyIteratorType = TFrequencyIterator;
  using FrequencyValueType = typename FrequencyIteratorType::FrequencyValueType;

  /****** Frequency Threshold Getters/Setters *****/
  /** Band range: Low threshold/boundary in Hertz */
  itkGetConstReferenceMacro(LowFrequencyThreshold, FrequencyValueType);
  itkSetMacro(LowFrequencyThreshold, FrequencyValueType);
  /**
   * Set low frequency threshold when input frequency is in radians.
   * @param freqLowInRadians low freq in radians.
   */
  void
  SetLowFrequencyThresholdInRadians(const FrequencyValueType & freqLowInRadians);

  /** Band range: High threshold/boundary in Hertz */
  itkGetConstReferenceMacro(HighFrequencyThreshold, FrequencyValueType);
  itkSetMacro(HighFrequencyThreshold, FrequencyValueType);
  void
  SetHighFrequencyThresholdInRadians(const FrequencyValueType & freqHighInRadians);

  /**
   * Set LowFrequencyThreshold and HighFrequencyThreshold at the same time,
   * input frequencies in Hertz.
   *
   * @param freqLow in hertz.
   * @param freqHigh in hertz.
   */
  void
  SetFrequencyThresholds(const FrequencyValueType & freqLow, const FrequencyValueType & freqHigh);

  /**
   * Set LowFrequencyThreshold and HighFrequencyThreshold at the same time,
   * input frequencies in Radians. 1Hz = (1/2pi) rad
   *
   * @param freqLowInRadians low freq in radians.
   * @param freqHighInRadians high freq in radians.
   */
  void
  SetFrequencyThresholdsInRadians(const FrequencyValueType & freqLowInRadians,
                                  const FrequencyValueType & freqHighInRadians);

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
  SetPassBand(const bool passLowThreshold, const bool passHighThreshold);

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
  SetStopBand(const bool passLowThreshold, const bool passHighThreshold);

  /** If true the frequency cut-off uses the radius of the frequency vector. If false, it uses the max absolute value of
   * the frequency vector. */
  itkSetMacro(RadialBand, bool);
  itkGetConstReferenceMacro(RadialBand, bool);
  itkBooleanMacro(RadialBand);

  /** Control if negative frequencies with absolute value equal to low frequency threshold are passing.
   * Only effective when RadialBand is false **/
  itkSetMacro(PassNegativeLowFrequencyThreshold, bool);
  itkGetConstReferenceMacro(PassNegativeLowFrequencyThreshold, bool);
  itkBooleanMacro(PassNegativeLowFrequencyThreshold);

  /** Control if negative frequencies with absolute value equal to high frequency threshold are passing.
   * Only effective when RadialBand is false **/
  itkSetMacro(PassNegativeHighFrequencyThreshold, bool);
  itkGetConstReferenceMacro(PassNegativeHighFrequencyThreshold, bool);
  itkBooleanMacro(PassNegativeHighFrequencyThreshold);


protected:
  FrequencyBandImageFilter();
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /* Checks the logic of FrequencyThresholds. */
  void
  VerifyPreconditions() ITKv5_CONST override;

  /* This is the box functor, which implements the filter's behavior. */
  void
  BandPass(FrequencyIteratorType & freqIt);

private:
  std::function<void(const ImageRegionType &)> m_DynamicThreadedGenerateDataFunction;

  FrequencyValueType m_LowFrequencyThreshold;
  FrequencyValueType m_HighFrequencyThreshold;

  bool m_PassBand{ true };
  bool m_PassLowFrequencyThreshold{ true };
  bool m_PassHighFrequencyThreshold{ true };
  bool m_RadialBand{ true };
  bool m_PassNegativeLowFrequencyThreshold{ true };
  bool m_PassNegativeHighFrequencyThreshold{ true };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFrequencyBandImageFilter.hxx"
#endif

#endif

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
#ifndef itkFrequencyBandImageFilter_hxx
#define itkFrequencyBandImageFilter_hxx

#include <itkFrequencyBandImageFilter.h>
#include <itkMath.h>
#include <itkImageAlgorithm.h>

namespace itk
{
template <class TImageType>
FrequencyBandImageFilter<TImageType>::FrequencyBandImageFilter()
  : m_LowFrequencyThreshold(0)
  , m_HighFrequencyThreshold(0.5)
  , // Nyquist in hertz
  m_PassBand(true)
  , m_PassLowFrequencyThreshold(true)
  , m_PassHighFrequencyThreshold(true)
  , m_RadialBand(true)
  , m_PassNegativeLowFrequencyThreshold(true)
  , m_PassNegativeHighFrequencyThreshold(true)
{}

template <class TImageType>
void
FrequencyBandImageFilter<TImageType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "FrequencyThresholds: Low: " << this->m_LowFrequencyThreshold
     << ", High: " << this->m_HighFrequencyThreshold << std::endl;
  os << indent << (this->m_PassBand ? "PassBand " : "StopBand ") << std::endl;
  os << indent << "   PassLowFrequencyThreshold? " << (this->m_PassLowFrequencyThreshold ? "Yes" : "No ") << std::endl;
  os << indent << "   PassHighFrequencyThreshold? " << (this->m_PassHighFrequencyThreshold ? "Yes" : "No ")
     << std::endl;
  os << indent << "   RadialBand? " << (this->m_RadialBand ? "Yes" : "No ") << std::endl;
}

template <class TImageType>
void
FrequencyBandImageFilter<TImageType>::SetPassBand(const bool pass_low_threshold, const bool pass_high_threshold)
{
  this->m_PassBand = true;
  this->m_PassLowFrequencyThreshold = pass_low_threshold;
  this->m_PassHighFrequencyThreshold = pass_high_threshold;
  this->Modified();
}

template <class TImageType>
void
FrequencyBandImageFilter<TImageType>::SetStopBand(const bool pass_low_threshold, const bool pass_high_threshold)
{
  this->m_PassBand = false;
  this->m_PassLowFrequencyThreshold = pass_low_threshold;
  this->m_PassHighFrequencyThreshold = pass_high_threshold;
  this->Modified();
}

template <class TImageType>
void
FrequencyBandImageFilter<TImageType>::SetFrequencyThresholds(const FrequencyValueType & freq_low,
                                                             const FrequencyValueType & freq_high)
{
  itkAssertInDebugAndIgnoreInReleaseMacro(freq_low <= freq_high);
  this->m_LowFrequencyThreshold = freq_low;
  this->m_HighFrequencyThreshold = freq_high;
  this->Modified();
}

template <class TImageType>
void
FrequencyBandImageFilter<TImageType>::SetLowFrequencyThresholdInRadians(const FrequencyValueType & freq_in_radians_low)
{
  this->SetLowFrequencyThreshold(freq_in_radians_low * 0.5 * itk::Math::one_over_pi);
}

template <class TImageType>
void
FrequencyBandImageFilter<TImageType>::SetHighFrequencyThresholdInRadians(
  const FrequencyValueType & freq_in_radians_high)
{
  this->SetHighFrequencyThreshold(freq_in_radians_high * 0.5 * itk::Math::one_over_pi);
}

template <class TImageType>
void
FrequencyBandImageFilter<TImageType>::SetFrequencyThresholdsInRadians(const FrequencyValueType & freq_in_radians_low,
                                                                      const FrequencyValueType & freq_in_radians_high)
{
  this->SetFrequencyThresholds(freq_in_radians_low * 0.5 * itk::Math::one_over_pi,
                               freq_in_radians_high * 0.5 * itk::Math::one_over_pi);
}

template <class TImageType>
void
FrequencyBandImageFilter<TImageType>::BeforeThreadedGenerateData()
{
  if (this->m_LowFrequencyThreshold > this->m_HighFrequencyThreshold)
    itkExceptionMacro("FrequencyThresholds wrong: " << this->m_LowFrequencyThreshold << " , "
                                                    << this->m_HighFrequencyThreshold);

  ImageAlgorithm::Copy(this->GetInput(),
                       this->GetOutput(),
                       this->GetInput()->GetLargestPossibleRegion(),
                       this->GetOutput()->GetLargestPossibleRegion());
}

template <class TImageType>
void
FrequencyBandImageFilter<TImageType>::ThreadedGenerateData(const ImageRegionType & outputRegionForThread, ThreadIdType)
{
  // outputPtr is a copy of input image from BeforeThreadedGenerateData
  ImagePointer outputPtr = this->GetOutput();

  FrequencyIteratorType freqIt(outputPtr, outputRegionForThread);
  freqIt.GoToBegin();
  FrequencyValueType                            f;
  typename FrequencyIteratorType::FrequencyType w;
  FrequencyValueType                            wMax;
  FrequencyValueType                            wMin;
  bool                                          freqIsNegative = false;

  while (!freqIt.IsAtEnd())
  {
    if (this->m_RadialBand)
    {
      f = sqrt(freqIt.GetFrequencyModuloSquare());
    }
    else // Cut-off box taking into account max absolute frequency.
    {
      w = freqIt.GetFrequency();
      wMax = std::abs(*std::max_element(w.Begin(), w.End()));
      wMin = std::abs(*std::min_element(w.Begin(), w.End()));
      f = std::max(wMax, wMin);
      if (wMin < wMax)
      {
        freqIsNegative = true;
      }
    }

    if (this->m_PassBand)
    {
      if (f < this->m_LowFrequencyThreshold || f > this->m_HighFrequencyThreshold)
      {
        freqIt.Set(NumericTraits<PixelType>::ZeroValue());
      }
    }
    else // Stop Band
    {
      if (f > this->m_LowFrequencyThreshold && f < this->m_HighFrequencyThreshold)
      {
        freqIt.Set(NumericTraits<PixelType>::ZeroValue());
      }
    }

    // Boundaries: Do not pass threshold frequencies if requested.
    if (!this->m_PassLowFrequencyThreshold)
    {
      if (f == this->m_LowFrequencyThreshold)
      {
        // Different boundaries when negative frequencies in the non-radial case.
        if (!this->m_RadialBand && this->m_PassNegativeLowFrequencyThreshold && freqIsNegative)
        {
          continue;
        }
        else
        {
          freqIt.Set(NumericTraits<PixelType>::ZeroValue());
        }
      }
    }

    if (!this->m_PassHighFrequencyThreshold)
    {
      if (f == this->m_HighFrequencyThreshold)
      {
        // Different boundaries when negative frequencies in the non-radial case.
        if (!this->m_RadialBand && this->m_PassNegativeHighFrequencyThreshold && freqIsNegative)
        {
          continue;
        }
        else
        {
          freqIt.Set(NumericTraits<PixelType>::ZeroValue());
        }
      }
    }

    ++freqIt;
  }
}
} // end namespace itk

#endif

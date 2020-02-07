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
#ifndef itkFrequencyBandImageFilter_hxx
#define itkFrequencyBandImageFilter_hxx

#include <itkFrequencyBandImageFilter.h>

#include <itkMath.h>
#include <itkImageAlgorithm.h>
#include <algorithm>
#include <cmath>

namespace itk
{
template <typename TImageType, typename TFrequencyIterator>
FrequencyBandImageFilter<TImageType, TFrequencyIterator>::FrequencyBandImageFilter()
  : m_LowFrequencyThreshold(0)
  , m_HighFrequencyThreshold(0.5)

{
  this->InPlaceOff();
  this->DynamicMultiThreadingOn();
  this->SetFunctor([this](FrequencyIteratorType & f) { this->BandPass(f); });
}

template <typename TImageType, typename TFrequencyIterator>
void
FrequencyBandImageFilter<TImageType, TFrequencyIterator>::PrintSelf(std::ostream & os, Indent indent) const
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

template <typename TImageType, typename TFrequencyIterator>
void
FrequencyBandImageFilter<TImageType, TFrequencyIterator>::SetPassBand(const bool passLowThreshold,
                                                                      const bool passHighThreshold)
{
  this->m_PassBand = true;
  this->m_PassLowFrequencyThreshold = passLowThreshold;
  this->m_PassHighFrequencyThreshold = passHighThreshold;
  this->Modified();
}

template <typename TImageType, typename TFrequencyIterator>
void
FrequencyBandImageFilter<TImageType, TFrequencyIterator>::SetStopBand(const bool passLowThreshold,
                                                                      const bool passHighThreshold)
{
  this->m_PassBand = false;
  this->m_PassLowFrequencyThreshold = passLowThreshold;
  this->m_PassHighFrequencyThreshold = passHighThreshold;
  this->Modified();
}

template <typename TImageType, typename TFrequencyIterator>
void
FrequencyBandImageFilter<TImageType, TFrequencyIterator>::SetFrequencyThresholds(const FrequencyValueType & freqLow,
                                                                                 const FrequencyValueType & freqHigh)
{
  itkAssertInDebugAndIgnoreInReleaseMacro(freqLow <= freqHigh);
  this->m_LowFrequencyThreshold = freqLow;
  this->m_HighFrequencyThreshold = freqHigh;
  this->Modified();
}

template <typename TImageType, typename TFrequencyIterator>
void
FrequencyBandImageFilter<TImageType, TFrequencyIterator>::SetLowFrequencyThresholdInRadians(
  const FrequencyValueType & freqLowInRadians)
{
  this->SetLowFrequencyThreshold(freqLowInRadians * 0.5 * Math::one_over_pi);
}

template <typename TImageType, typename TFrequencyIterator>
void
FrequencyBandImageFilter<TImageType, TFrequencyIterator>::SetHighFrequencyThresholdInRadians(
  const FrequencyValueType & freqHighInRadians)
{
  this->SetHighFrequencyThreshold(freqHighInRadians * 0.5 * Math::one_over_pi);
}

template <typename TImageType, typename TFrequencyIterator>
void
FrequencyBandImageFilter<TImageType, TFrequencyIterator>::SetFrequencyThresholdsInRadians(
  const FrequencyValueType & freqLowInRadians,
  const FrequencyValueType & freqHighInRadians)
{
  this->SetFrequencyThresholds(freqLowInRadians * 0.5 * Math::one_over_pi, freqHighInRadians * 0.5 * Math::one_over_pi);
}

template <typename TImageType, typename TFrequencyIterator>
void
FrequencyBandImageFilter<TImageType, TFrequencyIterator>::VerifyPreconditions() ITKv5_CONST
{
  this->Superclass::VerifyPreconditions();

  if (this->m_LowFrequencyThreshold > this->m_HighFrequencyThreshold)
  {
    itkExceptionMacro("FrequencyThresholds are illogical; Low > High: " << this->m_LowFrequencyThreshold << " > "
                                                                        << this->m_HighFrequencyThreshold);
  }
}

template <typename TImageType, typename TFrequencyIterator>
void
FrequencyBandImageFilter<TImageType, TFrequencyIterator>::BandPass(FrequencyIteratorType & freqIt)
{
  FrequencyValueType                            scalarFrequency;
  typename FrequencyIteratorType::FrequencyType vectorFrequency;
  FrequencyValueType                            maxFrequency;
  FrequencyValueType                            minFrequency;
  bool                                          freqIsNegative = false;

  if (this->m_RadialBand)
  {
    scalarFrequency = sqrt(freqIt.GetFrequencyModuloSquare());
  }
  else // Cut-off box taking into account max absolute scalar frequency.
  {
    vectorFrequency = freqIt.GetFrequency();
    maxFrequency = std::abs(*std::max_element(vectorFrequency.cbegin(), vectorFrequency.cend()));
    minFrequency = std::abs(*std::min_element(vectorFrequency.cbegin(), vectorFrequency.cend()));
    scalarFrequency = std::max(maxFrequency, minFrequency);
    if (minFrequency < maxFrequency)
    {
      freqIsNegative = true;
    }
  }

  if (this->m_PassBand)
  {
    if (scalarFrequency < this->m_LowFrequencyThreshold || scalarFrequency > this->m_HighFrequencyThreshold)
    {
      freqIt.Set(NumericTraits<PixelType>::ZeroValue());
    }
  }
  else // Stop Band
  {
    if (scalarFrequency > this->m_LowFrequencyThreshold && scalarFrequency < this->m_HighFrequencyThreshold)
    {
      freqIt.Set(NumericTraits<PixelType>::ZeroValue());
    }
  }

  // Boundaries: Do not pass threshold frequencies if requested.
  if (!this->m_PassLowFrequencyThreshold)
  {
    if (Math::FloatAlmostEqual(scalarFrequency, this->m_LowFrequencyThreshold))
    {
      // Different boundaries when negative frequencies in the non-radial case.
      if (!this->m_RadialBand && this->m_PassNegativeLowFrequencyThreshold && freqIsNegative)
      {
        return;
      }
      else
      {
        freqIt.Set(NumericTraits<PixelType>::ZeroValue());
      }
    }
  }

  if (!this->m_PassHighFrequencyThreshold)
  {
    if (Math::FloatAlmostEqual(scalarFrequency, this->m_HighFrequencyThreshold))
    {
      // Different boundaries when negative frequencies in the non-radial case.
      if (!this->m_RadialBand && this->m_PassNegativeHighFrequencyThreshold && freqIsNegative)
      {
        return;
      }
      else
      {
        freqIt.Set(NumericTraits<PixelType>::ZeroValue());
      }
    }
  }
}

} // end namespace itk

#endif

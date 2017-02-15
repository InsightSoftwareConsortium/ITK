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
#ifndef itkFrequencyBandPassImageFilter_hxx
#define itkFrequencyBandPassImageFilter_hxx

#include <itkFrequencyBandPassImageFilter.h>
#include <itkMath.h>
#include <itkImageAlgorithm.h>

namespace itk
{
template <class TImageType>
FrequencyBandPassImageFilter<TImageType>::FrequencyBandPassImageFilter()
  : m_LowFrequencyThreshold(0)
  , m_HighFrequencyThreshold(0.5)
  , // Nyquist in hertz
  m_PassBand(true)
  , m_PassLowFrequencyThreshold(true)
  , m_PassHighFrequencyThreshold(true)
{}

template <class TImageType>
void
FrequencyBandPassImageFilter<TImageType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "FrequencyThresholds: Low: " << this->m_LowFrequencyThreshold
     << ", High: " << this->m_HighFrequencyThreshold << std::endl;
  os << indent << (this->m_PassBand ? "PassBand " : "StopBand ") << std::endl;
  os << indent << "   PassLowFrequencyThreshold? " << (this->m_PassLowFrequencyThreshold ? "Yes" : "No ") << std::endl;
  os << indent << "   PassHighFrequencyThreshold? " << (this->m_PassHighFrequencyThreshold ? "Yes" : "No ")
     << std::endl;
}

template <class TImageType>
void
FrequencyBandPassImageFilter<TImageType>::SetPassBand(const bool pass_low_threshold, const bool pass_high_threshold)
{
  this->m_PassBand = true;
  this->m_PassLowFrequencyThreshold = pass_low_threshold;
  this->m_PassHighFrequencyThreshold = pass_high_threshold;
  this->Modified();
}

template <class TImageType>
void
FrequencyBandPassImageFilter<TImageType>::SetStopBand(const bool pass_low_threshold, const bool pass_high_threshold)
{
  this->m_PassBand = false;
  this->m_PassLowFrequencyThreshold = pass_low_threshold;
  this->m_PassHighFrequencyThreshold = pass_high_threshold;
  this->Modified();
}

template <class TImageType>
void
FrequencyBandPassImageFilter<TImageType>::SetFrequencyThresholds(const FrequencyValueType & freq_low,
                                                                 const FrequencyValueType & freq_high)
{
  itkAssertInDebugAndIgnoreInReleaseMacro(freq_low <= freq_high);
  this->m_LowFrequencyThreshold = freq_low;
  this->m_HighFrequencyThreshold = freq_high;
  this->Modified();
}

template <class TImageType>
void
FrequencyBandPassImageFilter<TImageType>::SetLowFrequencyThresholdInRadians(
  const FrequencyValueType & freq_in_radians_low)
{
  this->SetLowFrequencyThreshold(freq_in_radians_low * 0.5 * itk::Math::one_over_pi);
}

template <class TImageType>
void
FrequencyBandPassImageFilter<TImageType>::SetHighFrequencyThresholdInRadians(
  const FrequencyValueType & freq_in_radians_high)
{
  this->SetHighFrequencyThreshold(freq_in_radians_high * 0.5 * itk::Math::one_over_pi);
}

template <class TImageType>
void
FrequencyBandPassImageFilter<TImageType>::SetFrequencyThresholdsInRadians(
  const FrequencyValueType & freq_in_radians_low,
  const FrequencyValueType & freq_in_radians_high)
{
  this->SetFrequencyThresholds(freq_in_radians_low * 0.5 * itk::Math::one_over_pi,
                               freq_in_radians_high * 0.5 * itk::Math::one_over_pi);
}

template <class TImageType>
void
FrequencyBandPassImageFilter<TImageType>::BeforeThreadedGenerateData()
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
FrequencyBandPassImageFilter<TImageType>::ThreadedGenerateData(const ImageRegionType & outputRegionForThread,
                                                               ThreadIdType)
{
  // outputPtr is a copy of input image from BeforeThreadedGenerateData
  ImagePointer outputPtr = this->GetOutput();

  FrequencyIteratorType freqIt(outputPtr, outputRegionForThread);
  freqIt.GoToBegin();
  FrequencyValueType f;

  while (!freqIt.IsAtEnd())
  {
    f = sqrt(freqIt.GetFrequencyModuloSquare());
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
      if (f == this->m_LowFrequencyThreshold)
      {
        freqIt.Set(NumericTraits<PixelType>::ZeroValue());
      }
    if (!this->m_PassHighFrequencyThreshold)
      if (f == this->m_HighFrequencyThreshold)
      {
        freqIt.Set(NumericTraits<PixelType>::ZeroValue());
      }

    ++freqIt;
  }
}
} // end namespace itk

#endif

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
#ifndef itkPhaseFrequencyCorrelationOptimizer_hxx
#define itkPhaseFrequencyCorrelationOptimizer_hxx

#include "itkPhaseFrequencyCorrelationOptimizer.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkCompensatedSummation.h"

#include <cmath>
#include <type_traits>

namespace itk
{

template <typename TRegistrationMethod>
PhaseFrequencyCorrelationOptimizer<TRegistrationMethod>::PhaseFrequencyCorrelationOptimizer()
{
  this->m_PadFilter->SetSizeGreatestPrimeFactor( this->m_FFTFilter->GetSizeGreatestPrimeFactor() );
  this->m_CyclicShiftFilter->SetInput( this->m_PadFilter->GetOutput() );
  this->m_FFTFilter->SetInput( this->m_CyclicShiftFilter->GetOutput() );

  this->m_MaxPhaseOptimizer->SetPeakInterpolationMethod(PeakInterpolationMethodEnum::Parabolic);
}


template <typename TRegistrationMethod>
void
PhaseFrequencyCorrelationOptimizer<TRegistrationMethod>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


template <typename TRegistrationMethod>
bool
PhaseFrequencyCorrelationOptimizer<TRegistrationMethod>::SupportsPeakInterpolationMethod(
  PeakInterpolationMethodEnum method) const
{
  switch (method)
  {
  case (PeakInterpolationMethodEnum::None):
  case (PeakInterpolationMethodEnum::WeightedMeanPhase):
  case (PeakInterpolationMethodEnum::PhaseFrequencySlope):
    return true;
  default:
    return false;
  }
}


template <typename TRegistrationMethod>
void
PhaseFrequencyCorrelationOptimizer<TRegistrationMethod>::ComputeOffset()
{
  ImageConstPointer input = static_cast<ImageType *>(this->GetInput(0));
  ImageConstPointer fixed = static_cast<ImageType *>(this->GetInput(1));
  ImageConstPointer moving = static_cast<ImageType *>(this->GetInput(2));

  if (!input)
  {
    return;
  }

  const typename ImageType::SpacingType spacing = fixed->GetSpacing();
  const typename ImageType::PointType   fixedOrigin = fixed->GetOrigin();
  const typename ImageType::PointType   movingOrigin = moving->GetOrigin();

  this->m_MaxPhaseOptimizer->SetInput(input);
  this->m_MaxPhaseOptimizer->SetFixedImage(fixed);
  this->m_MaxPhaseOptimizer->SetMovingImage(moving);
  this->m_MaxPhaseOptimizer->SetOffsetCount(this->GetOffsetCount());

  this->m_MaxPhaseOptimizer->Modified();
  this->m_MaxPhaseOptimizer->Update();

  this->SetOffsetCount(this->m_MaxPhaseOptimizer->GetOffsetCount());
  this->m_Offsets = this->m_MaxPhaseOptimizer->GetOffsets();
  this->m_Confidences = this->m_MaxPhaseOptimizer->GetConfidences();

  const typename ImageType::RegionType wholeImage = input->GetLargestPossibleRegion();
  const typename ImageType::IndexType  oIndex = wholeImage.GetIndex();
  const typename ImageType::SizeType   size = wholeImage.GetSize();

  const auto maxIndices = this->m_MaxPhaseOptimizer->GetMaxIndices();

  typename ImageType::IndexType adjustedSize;
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    adjustedSize[d] = size[d] + oIndex[d];
  }

  if (this->m_PeakInterpolationMethod != PeakInterpolationMethodEnum::None) // interpolate the peak
  {
    for (unsigned int peak = 0; peak < this->m_PhaseInterpolated && peak < this->m_Offsets.size(); ++peak)
    {
      this->m_PadFilter->SetInput( this->m_MaxPhaseOptimizer->GetAdjustedInput() );
      typename CyclicShiftFilterType::OffsetType offset;
      for (unsigned int dim = 0; dim < ImageDimension; ++dim)
      {
        offset[dim] = -maxIndices[peak][dim];
      }
      this->m_CyclicShiftFilter->SetShift( offset );
      this->m_FFTFilter->Update();
      const typename FFTFilterType::OutputImageType * correlationFFT = this->m_FFTFilter->GetOutput();

      using ContinuousIndexType = ContinuousIndex<OffsetScalarType, ImageDimension>;
      ContinuousIndexType           maxIndex = maxIndices[peak];
      if (this->m_PeakInterpolationMethod == PeakInterpolationMethodEnum::WeightedMeanPhase) {
        using SumType = CompensatedSummation< double >;
        SumType powerSum;
        SumType weightedPhase;
        typename FFTFilterType::OutputImageType::IndexType index;
        for (unsigned int dim = 0; dim < ImageDimension; ++dim)
        {
          powerSum.ResetToZero();
          weightedPhase.ResetToZero();
          index.Fill(0);
          const SizeValueType maxFreqIndex = correlationFFT->GetLargestPossibleRegion().GetSize()[dim] / 2;
          for(SizeValueType freqIndex = 1; freqIndex < maxFreqIndex; ++freqIndex) {
            index[dim] = freqIndex;
            const typename FFTFilterType::OutputPixelType correlation = correlationFFT->GetPixel(index);
            const double phase = std::arg(correlation);
            const double power = correlation.imag() * correlation.imag() + correlation.real() * correlation.real();
            weightedPhase += phase / Math::pi * power;
            powerSum += power;
          }
          const double deltaToF = -1 * weightedPhase.GetSum() / powerSum.GetSum();
          maxIndex[dim] += deltaToF;
        }
      } else if(this->m_PeakInterpolationMethod == PeakInterpolationMethodEnum::PhaseFrequencySlope) {
        // todo: compute the linear regression of the phase, use
        // slope, add to maxIndex
      }

      for (unsigned i = 0; i < ImageDimension; ++i)
      {
        const OffsetScalarType directOffset =
          (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] * (maxIndex[i] - oIndex[i]);
        const OffsetScalarType mirrorOffset =
          (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] * (maxIndex[i] - adjustedSize[i]);
        if (std::abs(directOffset) <= std::abs(mirrorOffset))
        {
          this->m_Offsets[peak][i] = directOffset;
        }
        else
        {
          this->m_Offsets[peak][i] = mirrorOffset;
        }
      }
      std::cout << "GENERATED: " << this->m_Offsets[peak] << std::endl;
    }
  }
}

} // end namespace itk

#endif

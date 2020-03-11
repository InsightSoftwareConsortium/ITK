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

#include <cmath>
#include <type_traits>

namespace itk
{

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
  if (method == PeakInterpolationMethodEnum::None)
  {
    return true;
  }
  return false;
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

  this->m_SamplePeakOptimizer->SetInput(input);
  this->m_SamplePeakOptimizer->SetFixedImage(fixed);
  this->m_SamplePeakOptimizer->SetMovingImage(moving);
  this->m_SamplePeakOptimizer->SetOffsetCount(this->GetOffsetCount());

  this->m_SamplePeakOptimizer->Modified();
  this->m_SamplePeakOptimizer->Update();
  this->SetOffsetCount(this->m_SamplePeakOptimizer->GetOffsetCount());
  this->m_Offsets = this->m_SamplePeakOptimizer->GetOffsets();
  this->m_Confidences = this->m_SamplePeakOptimizer->GetConfidences();

  const typename ImageType::RegionType wholeImage = input->GetLargestPossibleRegion();
  const typename ImageType::IndexType  oIndex = wholeImage.GetIndex();
  const typename ImageType::SizeType   size = wholeImage.GetSize();

  const auto maxIndices = this->m_SamplePeakOptimizer->GetMaxIndices();

  typename ImageType::IndexType adjustedSize;
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    adjustedSize[d] = size[d] + oIndex[d];
  }

  if (this->m_PeakInterpolationMethod != PeakInterpolationMethodEnum::None) // interpolate the peak
  {
    // for (size_t offsetIndex = 0; offsetIndex < this->m_Offsets.size(); ++offsetIndex)
    //{
    // using ContinuousIndexType = ContinuousIndex<OffsetScalarType, ImageDimension>;
    // ContinuousIndexType maxIndex = maxIndices[offsetIndex];
    // typename ImageType::IndexType tempIndex = maxIndices[offsetIndex];
    // typename ImageType::PixelType y0;
    // typename ImageType::PixelType y1 = input->GetPixel(tempIndex);
    // typename ImageType::PixelType y2;

    // for (unsigned i = 0; i < ImageDimension; i++)
    //{
    // tempIndex[i] = maxIndex[i] - 1;
    // if (!wholeImage.IsInside(tempIndex))
    //{
    // tempIndex[i] = maxIndex[i];
    // continue;
    //}
    // y0 = input->GetPixel(tempIndex);
    // tempIndex[i] = maxIndex[i] + 1;
    // if (!wholeImage.IsInside(tempIndex))
    //{
    // tempIndex[i] = maxIndex[i];
    // continue;
    //}
    // y2 = input->GetPixel(tempIndex);
    // tempIndex[i] = maxIndex[i];

    // OffsetScalarType omega, theta, ratio;
    // switch (m_PeakInterpolationMethod)
    //{
    // case PeakInterpolationMethod::Parabolic:
    // maxIndex[i] += (y0 - y2) / (2 * (y0 - 2 * y1 + y2));
    // break;
    // case PeakInterpolationMethod::Cosine:
    // ratio = (y0 + y2) / (2 * y1);
    // if (offsetIndex > 0) // clip to -0.999... to 0.999... range
    //{
    // ratio = std::min(ratio, 1.0 - std::numeric_limits<OffsetScalarType>::epsilon());
    // ratio = std::max(ratio, -1.0 + std::numeric_limits<OffsetScalarType>::epsilon());
    //}
    // omega = std::acos(ratio);
    // theta = std::atan((y0 - y2) / (2 * y1 * std::sin(omega)));
    // maxIndex[i] -= ::itk::Math::one_over_pi * theta / omega;
    // break;
    // default:
    // itkAssertInDebugAndIgnoreInReleaseMacro("Unknown interpolation method");
    // break;
    //} // switch PeakInterpolationMethod

    // const OffsetScalarType directOffset = (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] * (maxIndex[i] -
    // oIndex[i]); const OffsetScalarType mirrorOffset = (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] *
    // (maxIndex[i] - adjustedSize[i]); if (std::abs(directOffset) <= std::abs(mirrorOffset))
    //{
    // this->m_Offsets[offsetIndex][i] = directOffset;
    //}
    // else
    //{
    // this->m_Offsets[offsetIndex][i] = mirrorOffset;
    //}
    //} // for ImageDimension
    //} // for offsetIndex
  }
}

} // end namespace itk

#endif

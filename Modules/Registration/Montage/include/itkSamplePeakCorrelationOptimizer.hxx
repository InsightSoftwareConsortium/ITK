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
#ifndef itkSamplePeakCorrelationOptimizer_hxx
#define itkSamplePeakCorrelationOptimizer_hxx

#include "itkSamplePeakCorrelationOptimizer.h"

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"

#include <cmath>
#include <type_traits>

/*
 * \author Jakub Bican, jakub.bican@matfyz.cz, Department of Image Processing,
 *         Institute of Information Theory and Automation,
 *         Academy of Sciences of the Czech Republic.
 *
 */

namespace itk
{

template <typename TRegistrationMethod>
SamplePeakCorrelationOptimizer<TRegistrationMethod>::SamplePeakCorrelationOptimizer()
{
  this->m_AdjustedInput = ImageType::New();
}


template <typename TRegistrationMethod>
void
SamplePeakCorrelationOptimizer<TRegistrationMethod>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "MaxCalculator: " << m_MaxCalculator << std::endl;
  os << indent << "MergePeaks: " << m_MergePeaks << std::endl;
  os << indent << "ZeroSuppression: " << m_ZeroSuppression << std::endl;
  os << indent << "PixelDistanceTolerance: " << m_PixelDistanceTolerance << std::endl;
}


template <typename TRegistrationMethod>
bool
SamplePeakCorrelationOptimizer<TRegistrationMethod>::SupportsPeakInterpolationMethod(
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
SamplePeakCorrelationOptimizer<TRegistrationMethod>::ComputeOffset()
{
  ImageConstPointer input = static_cast<ImageType *>(this->GetInput(0));
  ImageConstPointer fixed = static_cast<ImageType *>(this->GetInput(1));
  ImageConstPointer moving = static_cast<ImageType *>(this->GetInput(2));

  OffsetType offset;
  offset.Fill(0);

  if (!input)
  {
    return;
  }

  const typename ImageType::RegionType wholeImage = input->GetLargestPossibleRegion();
  const typename ImageType::SizeType   size = wholeImage.GetSize();
  const typename ImageType::IndexType  oIndex = wholeImage.GetIndex();

  const typename ImageType::SpacingType spacing = fixed->GetSpacing();
  const typename ImageType::PointType   fixedOrigin = fixed->GetOrigin();
  const typename ImageType::PointType   movingOrigin = moving->GetOrigin();

  // create the image which will be biased towards the expected solution
  // other pixels get their value reduced by multiplication with
  // e^(-f*(d/s)^2), where f is distancePenaltyFactor,
  // d is pixel's distance, and s is approximate image size
  m_AdjustedInput->CopyInformation(input);
  m_AdjustedInput->SetRegions(input->GetBufferedRegion());
  m_AdjustedInput->Allocate(false);

  typename ImageType::IndexType adjustedSize;
  typename ImageType::IndexType directExpectedIndex;
  typename ImageType::IndexType mirrorExpectedIndex;
  double                        imageSize2 = 0.0; // image size, squared
  for (unsigned d = 0; d < ImageDimension; d++)
  {
    adjustedSize[d] = size[d] + oIndex[d];
    imageSize2 += adjustedSize[d] * adjustedSize[d];
    directExpectedIndex[d] = (movingOrigin[d] - fixedOrigin[d]) / spacing[d] + oIndex[d];
    mirrorExpectedIndex[d] = (movingOrigin[d] - fixedOrigin[d]) / spacing[d] + adjustedSize[d];
  }

  double distancePenaltyFactor = 0.0;
  if (m_PixelDistanceTolerance == 0) // up to about half image size
  {
    distancePenaltyFactor = -10.0 / imageSize2;
  }
  else // up to about five times the provided tolerance
  {
    distancePenaltyFactor = std::log(0.9) / (m_PixelDistanceTolerance * m_PixelDistanceTolerance);
  }

  MultiThreaderBase * mt = this->GetMultiThreader();
  mt->ParallelizeImageRegion<ImageDimension>(
    wholeImage,
    [&](const typename ImageType::RegionType & region) {
      ImageRegionConstIterator<ImageType>     iIt(input, region);
      ImageRegionIteratorWithIndex<ImageType> oIt(m_AdjustedInput, region);
      IndexValueType                          zeroDist2 =
        100 * m_PixelDistanceTolerance * m_PixelDistanceTolerance; // round down to zero further from this
      for (; !oIt.IsAtEnd(); ++iIt, ++oIt)
      {
        typename ImageType::IndexType ind = oIt.GetIndex();
        IndexValueType                dist = 0;
        for (unsigned d = 0; d < ImageDimension; d++)
        {
          IndexValueType distDirect = (directExpectedIndex[d] - ind[d]) * (directExpectedIndex[d] - ind[d]);
          IndexValueType distMirror = (mirrorExpectedIndex[d] - ind[d]) * (mirrorExpectedIndex[d] - ind[d]);
          if (distDirect <= distMirror)
          {
            dist += distDirect;
          }
          else
          {
            dist += distMirror;
          }
        }

        typename ImageType::PixelType pixel;
        if (m_PixelDistanceTolerance > 0 && dist > zeroDist2)
        {
          pixel = 0;
        }
        else // evaluate the expensive exponential function
        {
          pixel = iIt.Get() * std::exp(distancePenaltyFactor * dist);
#ifndef NDEBUG
          pixel *= 1000; // make the intensities in this image more humane (close to 1.0)
                         // it is really hard to count zeroes after decimal point when comparing pixel intensities
                         // since this images is used to find maxima, absolute values are irrelevant
#endif
        }
        oIt.Set(pixel);
      }
    },
    nullptr);

  WriteDebug(m_AdjustedInput.GetPointer(), "m_AdjustedInput.nrrd");

  if (m_ZeroSuppression > 0.0) // suppress trivial zero solution
  {
    constexpr IndexValueType znSize = 4; // zero neighborhood size, in city-block distance
    mt->ParallelizeImageRegion<ImageDimension>(
      wholeImage,
      [&](const typename ImageType::RegionType & region) {
        ImageRegionIteratorWithIndex<ImageType> oIt(m_AdjustedInput, region);
        for (; !oIt.IsAtEnd(); ++oIt)
        {
          bool                          pixelValid = false;
          typename ImageType::PixelType pixel;
          typename ImageType::IndexType ind = oIt.GetIndex();
          IndexValueType                dist = 0;
          for (unsigned d = 0; d < ImageDimension; d++)
          {
            IndexValueType distD = ind[d] - oIndex[d];
            if (distD > IndexValueType(size[d] / 2)) // wrap around
            {
              distD = size[d] - distD;
            }
            dist += distD;
          }

          if (dist < znSize) // neighborhood of [0,0,...,0] - in case zero peak is blurred
          {
            pixelValid = true;
          }
          else
          {
            for (unsigned d = 0; d < ImageDimension; d++) // lines/sheets of zero m_MaxIndices
            {
              if (ind[d] == oIndex[d]) // one of the m_MaxIndices is "zero"
              {
                pixelValid = true;
              }
            }
          }

          if (pixelValid) // either neighborhood or lines/sheets says update the pixel
          {
            pixel = oIt.Get();
            // avoid the initial steep rise of function x/(1+x) by shifting it by 10
            pixel *= (dist + 10) / (m_ZeroSuppression + dist + 10);
            oIt.Set(pixel);
          }
        }
      },
      nullptr);

    WriteDebug(m_AdjustedInput.GetPointer(), "m_AdjustedInputZS.nrrd");
  }

  m_MaxCalculator->SetImage(m_AdjustedInput);
  if (m_MergePeaks)
  {
    m_MaxCalculator->SetN(std::ceil(this->m_Offsets.size() / 2) *
                          (static_cast<unsigned>(std::pow(3, ImageDimension)) - 1));
  }
  else
  {
    m_MaxCalculator->SetN(this->m_Offsets.size());
  }

  try
  {
    m_MaxCalculator->ComputeMaxima();
  }
  catch (ExceptionObject & err)
  {
    itkDebugMacro("exception caught during execution of max calculator - passing ");
    throw err;
  }

  this->m_Confidences = m_MaxCalculator->GetMaxima();
  this->m_MaxIndices = m_MaxCalculator->GetIndicesOfMaxima();
  itkAssertOrThrowMacro(this->m_Confidences.size() == m_MaxIndices.size(),
                        "Maxima and their m_MaxIndices must have the same number of elements");
  std::greater<PixelType> compGreater;
  const auto zeroBound = std::upper_bound(this->m_Confidences.begin(), this->m_Confidences.end(), 0.0, compGreater);
  if (zeroBound != this->m_Confidences.end()) // there are some non-positive values in here
  {
    unsigned i = zeroBound - this->m_Confidences.begin();
    this->m_Confidences.resize(i);
    m_MaxIndices.resize(i);
  }

  if (m_MergePeaks > 0) // eliminate m_MaxIndices belonging to the same blurry peak
  {
    unsigned i = 1;
    while (i < m_MaxIndices.size())
    {
      unsigned k = 0;
      while (k < i)
      {
        // calculate maximum distance along any dimension
        SizeValueType dist = 0;
        for (unsigned d = 0; d < ImageDimension; d++)
        {
          SizeValueType d1 = std::abs(m_MaxIndices[i][d] - m_MaxIndices[k][d]);
          if (d1 > size[d] / 2) // wrap around
          {
            d1 = size[d] - d1;
          }
          dist = std::max(dist, d1);
        }
        if (dist <= m_MergePeaks)
        {
          break;
        }
        ++k;
      }

      if (k < i) // k is nearby
      {
        this->m_Confidences[k] += this->m_Confidences[i]; // join amplitudes
        this->m_Confidences.erase(this->m_Confidences.begin() + i);
        m_MaxIndices.erase(m_MaxIndices.begin() + i);
      }
      else // examine next index
      {
        ++i;
      }
    }

    // now we need to re-sort the values
    std::vector<unsigned> sIndices;
    sIndices.reserve(this->m_Confidences.size());
    for (i = 0; i < this->m_Confidences.size(); i++)
    {
      sIndices.push_back(i);
    }
    std::sort(sIndices.begin(), sIndices.end(), [this](unsigned a, unsigned b) {
      return this->m_Confidences[a] > this->m_Confidences[b];
    });

    // now apply sorted order
    typename MaxCalculatorType::ValueVector tMaxs(this->m_Confidences.size());
    typename MaxCalculatorType::IndexVector tIndices(this->m_Confidences.size());
    for (i = 0; i < this->m_Confidences.size(); i++)
    {
      tMaxs[i] = this->m_Confidences[sIndices[i]];
      tIndices[i] = m_MaxIndices[sIndices[i]];
    }
    this->m_Confidences.swap(tMaxs);
    m_MaxIndices.swap(tIndices);
  }

  if (this->m_Offsets.size() > this->m_Confidences.size())
  {
    this->SetOffsetCount(this->m_Confidences.size());
  }
  else
  {
    this->m_Confidences.resize(this->m_Offsets.size());
    m_MaxIndices.resize(this->m_Offsets.size());
  }

  // double confidenceFactor = 1.0 / this->m_Confidences[0];

  for (unsigned m = 0; m < this->m_Confidences.size(); m++)
  {
    using ContinuousIndexType = ContinuousIndex<OffsetScalarType, ImageDimension>;
    ContinuousIndexType maxIndex = m_MaxIndices[m];

    for (unsigned i = 0; i < ImageDimension; ++i)
    {
      const OffsetScalarType directOffset =
        (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] * (maxIndex[i] - oIndex[i]);
      const OffsetScalarType mirrorOffset =
        (movingOrigin[i] - fixedOrigin[i]) - 1 * spacing[i] * (maxIndex[i] - adjustedSize[i]);
      if (std::abs(directOffset) <= std::abs(mirrorOffset))
      {
        offset[i] = directOffset;
      }
      else
      {
        offset[i] = mirrorOffset;
      }
    }

    // this->m_Confidences[m] *= confidenceFactor; // normalize - highest confidence will be 1.0
#ifdef NDEBUG
    this->m_Confidences[m] *= 1000.0; // make the intensities more humane (close to 1.0)
#endif

    this->m_Offsets[m] = offset;
  }
}

} // end namespace itk

#endif

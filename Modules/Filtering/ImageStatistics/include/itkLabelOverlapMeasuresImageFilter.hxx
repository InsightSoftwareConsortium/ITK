/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkLabelOverlapMeasuresImageFilter_hxx
#define itkLabelOverlapMeasuresImageFilter_hxx


#include "itkImageRegionConstIterator.h"
#include "itkTotalProgressReporter.h"

namespace itk
{

template <typename TLabelImage>
LabelOverlapMeasuresImageFilter<TLabelImage>::LabelOverlapMeasuresImageFilter()
{
  Self::SetPrimaryInputName("SourceImage");
  Self::AddRequiredInputName("TargetImage", 1);

  // This filter requires two input images
  this->SetNumberOfRequiredInputs(2);
}

template <typename TLabelImage>
void
LabelOverlapMeasuresImageFilter<TLabelImage>::BeforeStreamedGenerateData()
{
  Superclass::BeforeStreamedGenerateData();

  // Initialize the final map
  this->m_LabelSetMeasures.clear();
}

template <typename TLabelImage>
void
LabelOverlapMeasuresImageFilter<TLabelImage>::MergeMap(MapType & m1, MapType & m2) const
{
  for (auto m2_value : m2)
  {
    // Does this label exist in the cumulative structure yet?
    auto m1It = m1.find(m2_value.first);
    if (m1It == m1.end())
    {
      // move m2 entry into m1, this reuses the histogram if needed.
      m1.emplace(m2_value.first, std::move(m2_value.second));
    }
    else
    {
      typename MapType::mapped_type & labelStats = m1It->second;

      // Accumulate the information into m1
      labelStats.m_Source += m2_value.second.m_Source;             // segmentation which will be compared (TP+FP)
      labelStats.m_Target += m2_value.second.m_Target;             // Ground Truth segmentation (TP+FN)
      labelStats.m_Union += m2_value.second.m_Union;               // (TP+FN+FP)
      labelStats.m_Intersection += m2_value.second.m_Intersection; //(TP)
      labelStats.m_SourceComplement += m2_value.second.m_SourceComplement; //(FP)
      labelStats.m_TargetComplement += m2_value.second.m_TargetComplement; //(FN)
    }
  }
}

template <typename TLabelImage>
void
LabelOverlapMeasuresImageFilter<TLabelImage>::ThreadedStreamedGenerateData(const RegionType & outputRegionForThread)
{

  MapType localStatistics;

  ImageRegionConstIterator<LabelImageType> itS(this->GetSourceImage(), outputRegionForThread);
  ImageRegionConstIterator<LabelImageType> itT(this->GetTargetImage(), outputRegionForThread);

  // Support progress methods/callbacks
  TotalProgressReporter progress(this, this->GetSourceImage()->GetLargestPossibleRegion().GetNumberOfPixels());

  for (itS.GoToBegin(), itT.GoToBegin(); !itS.IsAtEnd(); ++itS, ++itT)
  {
    LabelType sourceLabel = itS.Get();
    LabelType targetLabel = itT.Get();

    // Initialized to empty if key does not already exist
    auto & sValue = localStatistics[sourceLabel];
    auto & tValue = localStatistics[targetLabel];

    ++sValue.m_Source;
    ++tValue.m_Target;

    if (sourceLabel == targetLabel)
    {
      ++sValue.m_Intersection;
      ++sValue.m_Union;
    }
    else
    {
      ++sValue.m_Union;
      ++tValue.m_Union;

      ++sValue.m_SourceComplement;
      ++tValue.m_TargetComplement;
    }

    progress.CompletedPixel();
  }


  // Merge localStatistics and m_LabelSetMeasures concurrently safe in a
  // local copy, this thread may do multiple merges.
  while (true)
  {

    {
      std::unique_lock<std::mutex> lock(m_Mutex);

      if (m_LabelSetMeasures.empty())
      {
        swap(m_LabelSetMeasures, localStatistics);
        break;
      }
      else
      {
        // copy the output map to thread local storage
        MapType tomerge;
        swap(m_LabelSetMeasures, tomerge);

        // allow other threads to merge data
        lock.unlock();

        // Merge tomerge into localStatistics, locally
        MergeMap(localStatistics, tomerge);
      }
    } // release lock
  }
}

template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetTotalOverlap() const -> RealType
{
  RealType numerator = 0.0;
  RealType denominator = 0.0;
  for (auto mapIt = this->m_LabelSetMeasures.begin(); mapIt != this->m_LabelSetMeasures.end(); ++mapIt)
  {
    // Do not include the background in the final value.
    if (mapIt->first == NumericTraits<LabelType>::ZeroValue())
    {
      continue;
    }
    numerator += static_cast<RealType>(mapIt->second.m_Intersection);
    denominator += static_cast<RealType>(mapIt->second.m_Target);
  }

  if (Math::ExactlyEquals(denominator, 0.0))
  {
    return NumericTraits<RealType>::max();
  }
  else
  {
    return (numerator / denominator);
  }
}

template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetTargetOverlap(LabelType label) const -> RealType
{
  auto mapIt = this->m_LabelSetMeasures.find(label);
  if (mapIt == this->m_LabelSetMeasures.end())
  {
    itkWarningMacro("Label " << label << " not found.");
    return 0.0;
  }

  RealType value;

  if (mapIt->second.m_Target == 0)
  {
    value = NumericTraits<RealType>::max();
  }
  else
  {
    value = static_cast<RealType>(mapIt->second.m_Intersection) / static_cast<RealType>(mapIt->second.m_Target);
  }
  return value;
}

template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetUnionOverlap() const -> RealType
{
  RealType numerator = 0.0;
  RealType denominator = 0.0;
  for (auto mapIt = this->m_LabelSetMeasures.begin(); mapIt != this->m_LabelSetMeasures.end(); ++mapIt)
  {
    // Do not include the background in the final value.
    if (mapIt->first == NumericTraits<LabelType>::ZeroValue())
    {
      continue;
    }
    numerator += static_cast<RealType>(mapIt->second.m_Intersection);
    denominator += static_cast<RealType>(mapIt->second.m_Union);
  }

  if (Math::ExactlyEquals(denominator, 0.0))
  {
    return NumericTraits<RealType>::max();
  }
  else
  {
    return (numerator / denominator);
  }
}

template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetUnionOverlap(LabelType label) const -> RealType
{
  auto mapIt = this->m_LabelSetMeasures.find(label);
  if (mapIt == this->m_LabelSetMeasures.end())
  {
    itkWarningMacro("Label " << label << " not found.");
    return 0.0;
  }

  RealType value;
  if (Math::ExactlyEquals(mapIt->second.m_Union, 0.0))
  {
    value = NumericTraits<RealType>::max();
  }
  else
  {
    value = static_cast<RealType>(mapIt->second.m_Intersection) / static_cast<RealType>(mapIt->second.m_Union);
  }

  return value;
}

template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetMeanOverlap() const -> RealType
{
  RealType uo = this->GetUnionOverlap();
  return (2.0 * uo / (1.0 + uo));
}

template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetMeanOverlap(LabelType label) const -> RealType
{
  RealType uo = this->GetUnionOverlap(label);
  return (2.0 * uo / (1.0 + uo));
}

template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetVolumeSimilarity() const -> RealType
{
  RealType numerator = 0.0;
  RealType denominator = 0.0;
  for (auto mapIt = this->m_LabelSetMeasures.begin(); mapIt != this->m_LabelSetMeasures.end(); ++mapIt)
  {
    // Do not include the background in the final value.
    if (mapIt->first == NumericTraits<LabelType>::ZeroValue())
    {
      continue;
    }
    numerator += ((static_cast<RealType>(mapIt->second.m_Source) - static_cast<RealType>(mapIt->second.m_Target)));
    denominator += ((static_cast<RealType>(mapIt->second.m_Source) + static_cast<RealType>(mapIt->second.m_Target)));
  }

  if (Math::ExactlyEquals(denominator, 0.0))
  {
    return NumericTraits<RealType>::max();
  }
  else
  {
    return (2.0 * numerator / denominator);
  }
}

template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetVolumeSimilarity(LabelType label) const -> RealType
{
  auto mapIt = this->m_LabelSetMeasures.find(label);
  if (mapIt == this->m_LabelSetMeasures.end())
  {
    itkWarningMacro("Label " << label << " not found.");
    return 0.0;
  }
  RealType value = 2.0 *
                   (static_cast<RealType>(mapIt->second.m_Source) - static_cast<RealType>(mapIt->second.m_Target)) /
                   (static_cast<RealType>(mapIt->second.m_Source) + static_cast<RealType>(mapIt->second.m_Target));
  return value;
}

template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetFalseNegativeError() const -> RealType
{
  RealType numerator = 0.0;
  RealType denominator = 0.0;
  for (auto mapIt = this->m_LabelSetMeasures.begin(); mapIt != this->m_LabelSetMeasures.end(); ++mapIt)
  {
    // Do not include the background in the final value.
    if (mapIt->first == NumericTraits<LabelType>::ZeroValue())
    {
      continue;
    }
    numerator += static_cast<RealType>(mapIt->second.m_TargetComplement);
    denominator += static_cast<RealType>(mapIt->second.m_Target);
  }

  if (Math::ExactlyEquals(denominator, 0.0))
  {
    return NumericTraits<RealType>::max();
  }
  else
  {
    return (numerator / denominator);
  }
}

template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetFalseNegativeError(LabelType label) const -> RealType
{
  auto mapIt = this->m_LabelSetMeasures.find(label);
  if (mapIt == this->m_LabelSetMeasures.end())
  {
    itkWarningMacro("Label " << label << " not found.");
    return 0.0;
  }

  RealType value;
  if (Math::ExactlyEquals(mapIt->second.m_Target, 0.0))
  {
    value = NumericTraits<RealType>::max();
  }
  else
  {
    value = static_cast<RealType>(mapIt->second.m_TargetComplement) / static_cast<RealType>(mapIt->second.m_Target);
  }

  return value;
}

template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetFalsePositiveError() const -> RealType
{
  RealType numerator = 0.0;
  RealType denominator = 0.0;

  auto nVox = this->GetInput(0)->GetLargestPossibleRegion().GetNumberOfPixels(); // TP+FP+FN+TN

  for (auto mapIt = this->m_LabelSetMeasures.begin(); mapIt != this->m_LabelSetMeasures.end(); ++mapIt)
  {
    // Do not include the background in the final value.
    if (mapIt->first == NumericTraits<LabelType>::ZeroValue())
    {
      continue;
    }
    auto nComplementIntersection = nVox - mapIt->second.m_Union;                                      // TN
    numerator += static_cast<RealType>(mapIt->second.m_SourceComplement);                             // FP
    denominator += static_cast<RealType>(mapIt->second.m_SourceComplement + nComplementIntersection); // FP+TN
  }

  if (Math::ExactlyEquals(denominator, 0.0))
  {
    return NumericTraits<RealType>::max();
  }
  else
  {
    return (numerator / denominator);
  }
}

template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetFalsePositiveError(LabelType label) const -> RealType
{
  auto nVox = this->GetInput(0)->GetLargestPossibleRegion().GetNumberOfPixels(); // TP+FP+FN+TN
  auto mapIt = this->m_LabelSetMeasures.find(label);
  if (mapIt == this->m_LabelSetMeasures.end())
  {
    itkWarningMacro("Label " << label << " not found.");
    return 0.0;
  }

  RealType value;
  if (Math::ExactlyEquals(mapIt->second.m_Source, 0.0))
  {
    value = NumericTraits<RealType>::max();
  }
  else
  {
    auto nComplementIntersection = nVox - mapIt->second.m_Union; // TN

    value = static_cast<RealType>(mapIt->second.m_SourceComplement) /
            static_cast<RealType>(mapIt->second.m_SourceComplement + nComplementIntersection);
  }

  return value;
}


template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetFalseDiscoveryRate() const -> RealType
{
  RealType numerator = 0.0;
  RealType denominator = 0.0;
  for (auto mapIt = this->m_LabelSetMeasures.begin(); mapIt != this->m_LabelSetMeasures.end(); ++mapIt)
  {
    // Do not include the background in the final value.
    if (mapIt->first == NumericTraits<LabelType>::ZeroValue())
    {
      continue;
    }
    numerator += static_cast<RealType>(mapIt->second.m_SourceComplement); // FP
    denominator += static_cast<RealType>(mapIt->second.m_Source);         // FP+TP
  }

  if (Math::ExactlyEquals(denominator, 0.0))
  {
    return NumericTraits<RealType>::max();
  }
  else
  {
    return (numerator / denominator);
  }
}

template <typename TLabelImage>
auto
LabelOverlapMeasuresImageFilter<TLabelImage>::GetFalseDiscoveryRate(LabelType label) const -> RealType
{
  auto mapIt = this->m_LabelSetMeasures.find(label);
  if (mapIt == this->m_LabelSetMeasures.end())
  {
    itkWarningMacro("Label " << label << " not found.");
    return 0.0;
  }

  RealType value;
  if (Math::ExactlyEquals(mapIt->second.m_Source, 0.0))
  {
    value = NumericTraits<RealType>::max();
  }
  else
  {
    value = static_cast<RealType>(mapIt->second.m_SourceComplement) / static_cast<RealType>(mapIt->second.m_Source);
  }
  return value;
}

template <typename TLabelImage>
void
LabelOverlapMeasuresImageFilter<TLabelImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  // todo!!!
  Superclass::PrintSelf(os, indent);
}


} // end namespace itk
#endif

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
#ifndef itkUniformRandomSpatialNeighborSubsampler_hxx
#define itkUniformRandomSpatialNeighborSubsampler_hxx
#include "itkUniformRandomSpatialNeighborSubsampler.h"
#include <set>

namespace itk {
namespace Statistics {

template <typename TSample, typename TRegion>
UniformRandomSpatialNeighborSubsampler<TSample, TRegion>
::UniformRandomSpatialNeighborSubsampler()
{
  m_RandomNumberGenerator = RandomGeneratorType::New();
  m_UseClockForSeed = false;
  m_RandomNumberGenerator->SetSeed(this->m_Seed);
  m_NumberOfResultsRequested = 0;
}

template <typename TSample, typename TRegion>
typename LightObject::Pointer
UniformRandomSpatialNeighborSubsampler<TSample, TRegion>
::InternalClone() const
{
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval =
    dynamic_cast<Self *>(loPtr.GetPointer());
  if(rval.IsNull())
    {
    itkExceptionMacro(<< "downcast to type "
                      << this->GetNameOfClass()
                      << " failed.");
    }

  rval->m_NumberOfResultsRequested = this->m_NumberOfResultsRequested;
  rval->SetSeed(this->m_Seed);
  rval->SetUseClockForSeed(this->m_UseClockForSeed);
  return loPtr;
}

template <typename TSample, typename TRegion>
void
UniformRandomSpatialNeighborSubsampler<TSample, TRegion>
::Search(const InstanceIdentifier& query,
         SubsamplePointer& results)
{
  if (!this->m_RadiusInitialized)
    {
    itkExceptionMacro(<< "Radius not set.");
    }
  if (!this->m_SampleRegionInitialized)
    {
    itkExceptionMacro(<< "Sample region not set.");
    }
  if (!this->GetRegionConstraintInitialized())
    {
    this->SetRegionConstraint(this->m_SampleRegion);
    }

  results->Clear();
  results->SetSample(this->m_Sample);

  RegionType searchRegion;
  IndexType searchStartIndex;
  IndexType searchEndIndex;

  IndexType constraintIndex = this->m_RegionConstraint.GetIndex();
  SizeType constraintSize = this->m_RegionConstraint.GetSize();

  IndexType queryIndex;
  typename RegionType::OffsetTableType offsetTable;
  this->m_SampleRegion.ComputeOffsetTable(offsetTable);
  ImageHelperType::ComputeIndex(this->m_SampleRegion.GetIndex(),
                                query,
                                offsetTable,
                                queryIndex);

  unsigned int numberOfPoints = 1;

  for (unsigned int dim = 0; dim < RegionType::ImageDimension; ++dim)
    {
    if (queryIndex[dim] < static_cast<IndexValueType>(this->m_Radius[dim]))
      {
      searchStartIndex[dim] = std::max(NumericTraits<IndexValueType>::ZeroValue(), constraintIndex[dim]);
      }
    else
      {
      searchStartIndex[dim] = std::max(static_cast<IndexValueType>(queryIndex[dim] - this->m_Radius[dim]),
                                           constraintIndex[dim]);
      }

    if (queryIndex[dim] + this->m_Radius[dim] < constraintIndex[dim] + constraintSize[dim])
      {
      searchEndIndex[dim] = queryIndex[dim] + this->m_Radius[dim];
      }
    else
      {
      searchEndIndex[dim] = (constraintIndex[dim] + constraintSize[dim]) - 1;
      }
    numberOfPoints = numberOfPoints * (searchEndIndex[dim] - searchStartIndex[dim] + 1);
    }

  if (!this->m_RegionConstraint.IsInside(queryIndex))
    {
    itkWarningMacro( "query point (" << query << ") corresponding to index ("
                     << queryIndex << ") is not inside the given image region ("
                     << this->m_RegionConstraint
                     << ").  No matching points found.");
    return;
    }

  if (!this->m_RequestMaximumNumberOfResults &&
      (m_NumberOfResultsRequested < numberOfPoints))
    {
    numberOfPoints = m_NumberOfResultsRequested;
    }

  unsigned int pointsFound = 0;

  ::std::set<InstanceIdentifier> usedIds;
  typename RegionType::OffsetValueType offset;

  // The trouble with decoupling the region from the sample is that
  // there is an implicit assumption that the instance identifiers in the sample
  // are ordered as if someone was iterating forward through the region
  // TODO Is this a safe assumption to make?
  if (this->m_CanSelectQuery)
    {
    // This case will be faster since it doesn't need to check
    // whether the selected index matches the query index
    while (pointsFound < numberOfPoints)
      {
      // randomly select an index
      IndexType index;
      for (unsigned int dim = 0; dim < RegionType::ImageDimension; ++dim)
        {
        index[dim] = this->GetIntegerVariate(searchStartIndex[dim],
                                             searchEndIndex[dim],
                                             queryIndex[dim]);
        }
      offset = 0;
      ImageHelperType::ComputeOffset(this->m_SampleRegion.GetIndex(),
                                     index,
                                     offsetTable,
                                     offset);

      results->AddInstance(static_cast<InstanceIdentifier>(offset));
      ++pointsFound;
      }
    }
  else
    {
    while (pointsFound < numberOfPoints)
      {
      // randomly select an index
      IndexType index;
      for (unsigned int dim = 0; dim < RegionType::ImageDimension; ++dim)
        {
        index[dim] = this->GetIntegerVariate(searchStartIndex[dim],
                                             searchEndIndex[dim],
                                             queryIndex[dim]);
        }
      // only include this index if it is not the query point
      if (index != queryIndex)
        {
        offset = 0;
        ImageHelperType::ComputeOffset(this->m_SampleRegion.GetIndex(),
                                       index,
                                       offsetTable,
                                       offset);
        results->AddInstance(static_cast<InstanceIdentifier>(offset));
        ++pointsFound;
        }
      }
    }
} // end Search method

template <typename TSample, typename TRegion>
typename UniformRandomSpatialNeighborSubsampler<TSample, TRegion>::RandomIntType
UniformRandomSpatialNeighborSubsampler<TSample, TRegion>
::GetIntegerVariate(RandomIntType lowerBound,
                    RandomIntType upperBound,
                    RandomIntType itkNotUsed(mean))
{
  RandomIntType sizeRange = upperBound - lowerBound;
  // mean ignored since we are uniformly sampling
  return lowerBound +
    m_RandomNumberGenerator->GetIntegerVariate(sizeRange);
}

template <typename TSample, typename TRegion>
void
UniformRandomSpatialNeighborSubsampler<TSample, TRegion>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Number of results requested: " << m_NumberOfResultsRequested << std::endl;
  os << indent << "Use clock for seed: " << m_UseClockForSeed << std::endl;
  os << std::endl;
}

}// end namespace Statistics
}// end namespace itk

#endif

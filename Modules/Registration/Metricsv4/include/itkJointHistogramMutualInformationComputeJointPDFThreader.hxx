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
#ifndef itkJointHistogramMutualInformationComputeJointPDFThreader_hxx
#define itkJointHistogramMutualInformationComputeJointPDFThreader_hxx

#include "itkIndexRange.h"
#if !defined(ITK_LEGACY_REMOVE) && !defined(ITK_FUTURE_LEGACY_REMOVE)
#  include "itkImageRegionConstIteratorWithIndex.h"
#endif

namespace itk
{

template <typename TJointHistogramMetric>
void
JointHistogramMutualInformationComputeJointPDFThreader<
  ThreadedImageRegionPartitioner<TJointHistogramMetric::VirtualImageDimension>,
  TJointHistogramMetric>::ThreadedExecution(const DomainType & imageSubRegion, const ThreadIdType threadId)
{
  VirtualPointType virtualPoint;
  // The virtual image is never allocated, so iterate its indices rather than its pixels.
  for (const VirtualIndexType & virtualIndex :
       ImageRegionIndexRange<TJointHistogramMetric::VirtualImageDimension>(imageSubRegion))
  {
    this->m_Associate->TransformVirtualIndexToPhysicalPoint(virtualIndex, virtualPoint);
    this->ProcessPoint(virtualIndex, virtualPoint, threadId);
  }
}

template <typename TJointHistogramMetric>
void
JointHistogramMutualInformationComputeJointPDFThreader<ThreadedIndexedContainerPartitioner, TJointHistogramMetric>::
  ThreadedExecution(const DomainType & indexSubRange, const ThreadIdType threadId)
{
  VirtualPointType virtualPoint;
  VirtualIndexType virtualIndex;
  using ElementIdentifierType = typename VirtualPointSetType::MeshTraits::PointIdentifier;
  const ElementIdentifierType begin = indexSubRange[0];
  const ElementIdentifierType end = indexSubRange[1];
  for (ElementIdentifierType i = begin; i <= end; ++i)
  {
    virtualPoint = this->m_Associate->m_VirtualSampledPointSet->GetPoint(i);
    this->m_Associate->TransformPhysicalPointToVirtualIndex(virtualPoint, virtualIndex);
    this->ProcessPoint(virtualIndex, virtualPoint, threadId);
  }
}

} // end namespace itk
#endif

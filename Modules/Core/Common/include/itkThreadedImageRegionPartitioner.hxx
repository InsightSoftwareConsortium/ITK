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
#ifndef itkThreadedImageRegionPartitioner_hxx
#define itkThreadedImageRegionPartitioner_hxx

#include "itkThreadedImageRegionPartitioner.h"

namespace itk
{

template <unsigned int VDimension>
ThreadedImageRegionPartitioner<VDimension>
::ThreadedImageRegionPartitioner()
{
  this->m_ImageRegionSplitter = ImageRegionSplitterType::New();
}

template <unsigned int VDimension>
ThreadedImageRegionPartitioner<VDimension>
::~ThreadedImageRegionPartitioner()
{
}

template <unsigned int VDimension>
ThreadIdType
ThreadedImageRegionPartitioner<VDimension>
::PartitionDomain( const ThreadIdType threadId,
                        const ThreadIdType requestedTotal,
                        const DomainType &completeRegion,
                        DomainType& subRegion) const
{
  subRegion = completeRegion;
  const unsigned int maxNumberOfSplits = m_ImageRegionSplitter->GetSplit( threadId, requestedTotal, subRegion );

  return static_cast<ThreadIdType>( maxNumberOfSplits );
}

} // end namespace itk

#endif

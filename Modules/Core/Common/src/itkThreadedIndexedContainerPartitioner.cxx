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
#include "itkThreadedIndexedContainerPartitioner.h"
#include "itkMath.h"

namespace itk
{

ThreadedIndexedContainerPartitioner
::ThreadedIndexedContainerPartitioner()
{
}

ThreadedIndexedContainerPartitioner
::~ThreadedIndexedContainerPartitioner()
{
}

ThreadIdType
ThreadedIndexedContainerPartitioner
::PartitionDomain( const ThreadIdType threadId,
                        const ThreadIdType requestedTotal,
                        const DomainType& completeIndexRange,
                        DomainType& subIndexRange) const
{
  // completeIndexRange and subIndexRange are inclusive

  // determine the actual number of pieces that will be generated
  const double count = static_cast<double>( completeIndexRange[1] - completeIndexRange[0] + 1 );
  ThreadIdType valuesPerThread =
    Math::Ceil<ThreadIdType>( count/static_cast<double>(requestedTotal) );
  ThreadIdType maxThreadIdUsed =
    Math::Ceil<ThreadIdType>( count/static_cast<double>(valuesPerThread) ) - 1;

  // Split the index range
  if (threadId < maxThreadIdUsed)
    {
    subIndexRange[0] = completeIndexRange[0] + threadId * valuesPerThread;
    subIndexRange[1] = subIndexRange[0] + valuesPerThread - 1;
    }
  if (threadId == maxThreadIdUsed)
    {
    subIndexRange[0] = completeIndexRange[0] + threadId * valuesPerThread;
    // last thread needs to process the "rest" of the range
    subIndexRange[1] = completeIndexRange[1];
    }

  itkDebugMacro("ThreadedIndexedContainerPartitioner:  Split : " << subIndexRange );

  return maxThreadIdUsed + 1;
}

} // end namespace itk

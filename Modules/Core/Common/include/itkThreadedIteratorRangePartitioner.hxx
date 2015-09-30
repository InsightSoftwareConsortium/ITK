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
#ifndef itkThreadedIteratorRangePartitioner_hxx
#define itkThreadedIteratorRangePartitioner_hxx

#include "itkNumericTraits.h"
#include "itkThreadedIteratorRangePartitioner.h"
#include "itkMath.h"

#include <iterator>

namespace itk
{

template< typename TIterator >
ThreadedIteratorRangePartitioner< TIterator >
::ThreadedIteratorRangePartitioner()
{
}

template< typename TIterator >
ThreadedIteratorRangePartitioner< TIterator >
::~ThreadedIteratorRangePartitioner()
{
}

template< typename TIterator >
ThreadIdType
ThreadedIteratorRangePartitioner< TIterator >
::PartitionDomain( const ThreadIdType threadId,
                   const ThreadIdType requestedTotal,
                   const DomainType& completeDomain,
                   DomainType& subDomain ) const
{
  // overallIndexRange is expected to be inclusive

  // determine the actual number of pieces that will be generated
  ThreadIdType count = std::distance( completeDomain.Begin(), completeDomain.End() );

  ThreadIdType valuesPerThread =
    Math::Ceil<ThreadIdType>( static_cast< double >( count ) / static_cast< double >( requestedTotal ));
  ThreadIdType maxThreadIdUsed =
    Math::Ceil<ThreadIdType>( static_cast< double >( count ) / static_cast< double >( valuesPerThread )) - 1;

  if ( threadId > maxThreadIdUsed )
    {
    // return before advancing Begin iterator, to prevent advancing
    // past end
    return maxThreadIdUsed + 1;
    }

  const ThreadIdType startIndexCount = threadId * valuesPerThread;
  subDomain.m_Begin = completeDomain.Begin();
  std::advance( subDomain.m_Begin, startIndexCount );

  if (threadId < maxThreadIdUsed)
    {
    subDomain.m_End = subDomain.m_Begin;
    std::advance( subDomain.m_End, valuesPerThread );
    }
  if (threadId == maxThreadIdUsed)
    {
    // last thread needs to process the "rest" of the range
    subDomain.m_End = completeDomain.End();
    }

  return maxThreadIdUsed + 1;
}

} // end namespace itk

#endif

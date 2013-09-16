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
#ifndef __itkThreadedIteratorRangePartitioner_hxx
#define __itkThreadedIteratorRangePartitioner_hxx

#include "itkNumericTraits.h"
#include "itkThreadedIteratorRangePartitioner.h"
#include "itkMath.h"

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
  typename DomainType::IteratorType it;
  ThreadIdType count = NumericTraits< ThreadIdType >::Zero;
  for( it = completeDomain.Begin(); it != completeDomain.End(); ++it )
    {
    ++count;
    }
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

  // Split the domain range
  it = completeDomain.Begin();
  const ThreadIdType startIndexCount = threadId * valuesPerThread;
  for( ThreadIdType ii = 0; ii < startIndexCount; ++ii )
    {
    ++it;
    }
  subDomain.m_Begin = it;
  if (threadId < maxThreadIdUsed)
    {
    const ThreadIdType endIndexCount = valuesPerThread;
    for( ThreadIdType ii = 0; ii < endIndexCount; ++ii )
      {
      ++it;
      }
    subDomain.m_End = it;
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

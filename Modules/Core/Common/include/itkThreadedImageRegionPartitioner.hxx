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
#ifndef __itkThreadedImageRegionPartitioner_hxx
#define __itkThreadedImageRegionPartitioner_hxx

#include "itkThreadedImageRegionPartitioner.h"

namespace itk
{

template <unsigned int VDimension>
ThreadedImageRegionPartitioner<VDimension>
::ThreadedImageRegionPartitioner()
{
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
  const SizeType      requestedRegionSize = completeRegion.GetSize();
  const ThreadIdType  singleThread = 1;

  // Initialize the subRegion to the output requested region
  subRegion            = completeRegion;
  IndexType splitIndex = subRegion.GetIndex();
  SizeType  splitSize  = subRegion.GetSize();

  // Protect against division by 0 below. Seems this would be a bug
  // in MultiThreader if it passed 0 for requestedTotal.
  if( requestedTotal == 0 )
    {
    return singleThread;
    }

  // split on the outermost dimension available
  int splitAxis = this->ImageDimension - 1;
  while( requestedRegionSize[splitAxis] == 1 )
    {
    --splitAxis;
    if( splitAxis < 0 )
      {
      // cannot split
      itkDebugMacro( " Cannot Split Region" );
      return singleThread;
      }
    }

  // Make sure we don't have a 0-valued dimension size to avoid
  // division by 0 below.
  // This would be a bug in the passed completeRegion.
  if( requestedRegionSize[splitAxis] == 0 )
    {
    itkExceptionMacro( "requestedRegionSize[splitAxis] == 0. "
                      << "Error in input 'completeRegion'" );
    }

  // determine the actual number of pieces that will be generated
  const SizeValueType range = requestedRegionSize[splitAxis];

  ThreadIdType valuesPerThread =
    Math::Ceil<ThreadIdType>( range / static_cast<double>(requestedTotal) );

  ThreadIdType maxThreadIdUsed =
    Math::Ceil<ThreadIdType>( range / static_cast<double>(valuesPerThread) ) - 1;

  // Split the region
  if( threadId < maxThreadIdUsed )
    {
    splitIndex[splitAxis] += threadId * valuesPerThread;
    splitSize[splitAxis] = valuesPerThread;
    }

  if( threadId == maxThreadIdUsed )
    {
    splitIndex[splitAxis] += threadId * valuesPerThread;
    // last thread needs to process the "rest" dimension being split
    splitSize[splitAxis] = splitSize[splitAxis] - threadId * valuesPerThread;
    }

  // set the split region ivars
  subRegion.SetIndex( splitIndex );
  subRegion.SetSize( splitSize );

  itkDebugMacro("  Split Piece: " << subRegion );

  return maxThreadIdUsed + 1;
}

} // end namespace itk

#endif

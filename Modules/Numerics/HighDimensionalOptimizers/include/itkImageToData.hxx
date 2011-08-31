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
#ifndef __itkImageToData_hxx
#define __itkImageToData_hxx

#include "itkImageToData.h"
#include "vnl/vnl_math.h"

namespace itk
{

/**
 * Constructor
 */
template <unsigned int VDimension, class TDataHolder, typename TInputObject>
ImageToData<VDimension, TDataHolder, TInputObject>
::ImageToData()
{
}

/**
 * Destructor
 */
template <unsigned int VDimension, class TDataHolder, typename TInputObject>
ImageToData<VDimension, TDataHolder, TInputObject>
::~ImageToData()
{}

/**
 *
 */
template <unsigned int VDimension, class TDataHolder, typename TInputObject>
void
ImageToData<VDimension, TDataHolder, TInputObject>
::SetOverallRegion(  const ImageRegionType& region )
{
  this->SetOverallObject( region );
}

/**
 *
 */
template <unsigned int VDimension, class TDataHolder, typename TInputObject>
ThreadIdType
ImageToData<VDimension, TDataHolder, TInputObject>
::SplitRequestedObject( const ThreadIdType threadID,
                        const ThreadIdType requestedTotal,
                        const InputObjectType &overallRegion,
                        InputObjectType& splitRegion) const
{
  const SizeType      requestedRegionSize = overallRegion.GetSize();
  const ThreadIdType  singleThread = 1;

  // Initialize the splitRegion to the output requested region
  splitRegion = overallRegion;
  IndexType splitIndex = splitRegion.GetIndex();
  SizeType splitSize = splitRegion.GetSize();

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
  // This would be a bug in the passed overallRegion.
  if( requestedRegionSize[splitAxis] == 0 )
    {
    itkExceptionMacro( "requestedRegionSize[splitAxis] == 0. "
                      << "Error in input 'overallRegion'" );
    }

  // determine the actual number of pieces that will be generated
  const SizeValueType range = requestedRegionSize[splitAxis];
  ThreadIdType valuesPerThread =
    Math::Ceil<ThreadIdType>( range / static_cast<double>(requestedTotal) );
  ThreadIdType maxThreadIdUsed =
    Math::Ceil<ThreadIdType>( range / static_cast<double>(valuesPerThread) ) - 1;

  // Split the region
  if( threadID < maxThreadIdUsed )
    {
    splitIndex[splitAxis] += threadID * valuesPerThread;
    splitSize[splitAxis] = valuesPerThread;
    }

  if( threadID == maxThreadIdUsed )
    {
    splitIndex[splitAxis] += threadID * valuesPerThread;
    // last thread needs to process the "rest" dimension being split
    splitSize[splitAxis] = splitSize[splitAxis] - threadID * valuesPerThread;
    }

  // set the split region ivars
  splitRegion.SetIndex( splitIndex );
  splitRegion.SetSize( splitSize );

  itkDebugMacro("  Split Piece: " << splitRegion );

  return maxThreadIdUsed + 1;
}

} // end namespace itk

#endif

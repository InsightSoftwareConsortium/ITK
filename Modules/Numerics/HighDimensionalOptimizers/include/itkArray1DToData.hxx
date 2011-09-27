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
#ifndef __itkArray1DToData_hxx
#define __itkArray1DToData_hxx

#include "itkArray1DToData.h"
#include "vnl/vnl_math.h"

namespace itk
{

/**
 * Default constructor
 */
template<class TDataHolder>
Array1DToData<TDataHolder>::Array1DToData()
{
  this->m_OverallObject.Fill(0);
}

/**
 * Destructor
 */
template<class TDataHolder>
Array1DToData<TDataHolder>::~Array1DToData()
{}

/**
 * Set the overall range over which to thread.
 */
template<class TDataHolder>
void
Array1DToData<TDataHolder>
::SetOverallIndexRange(  const IndexRangeType & range )
{
  if( range[0] > range[1] )
    {
    itkExceptionMacro("Error in range.  Begin is less than End: "
                      << range << ".");
    }
  this->SetOverallObject( range );
}

/**
 * Split the requested range into a subrange.
 */
template<class TDataHolder>
ThreadIdType
Array1DToData<TDataHolder>
::SplitRequestedObject( const ThreadIdType threadID,
                        const ThreadIdType requestedTotal,
                        const InputObjectType& overallIndexRange,
                        InputObjectType& splitIndexRange) const
{
  // overallIndexRange is expected to be inclusive

  // determine the actual number of pieces that will be generated
  IndexRangeType::IndexValueType count =
    overallIndexRange[1] - overallIndexRange[0] + 1;
  ThreadIdType valuesPerThread =
    Math::Ceil<ThreadIdType>( count/static_cast<double>(requestedTotal) );
  ThreadIdType maxThreadIdUsed =
    Math::Ceil<ThreadIdType>( count/static_cast<double>(valuesPerThread) ) - 1;

  // Split the index range
  if (threadID < maxThreadIdUsed)
    {
    splitIndexRange[0] = overallIndexRange[0] + threadID * valuesPerThread;
    splitIndexRange[1] = splitIndexRange[0] + valuesPerThread - 1;
    }
  if (threadID == maxThreadIdUsed)
    {
    splitIndexRange[0] = overallIndexRange[0] + threadID * valuesPerThread;
    // last thread needs to process the "rest" of the range
    splitIndexRange[1] = overallIndexRange[1];
    }

  itkDebugMacro("Array1DToData:  Split : " << splitIndexRange );

  return maxThreadIdUsed + 1;
}

} // end namespace itk

#endif

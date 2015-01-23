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
#ifndef itkConnectedComponentAlgorithm_h
#define itkConnectedComponentAlgorithm_h

#include "itkImage.h"
#include "itkShapedNeighborhoodIterator.h"

namespace itk
{
template< typename TIterator >
TIterator *
setConnectivity(TIterator *it, bool fullyConnected = false)
{
  typename TIterator::OffsetType offset;
  it->ClearActiveList();
  if ( !fullyConnected )
    {
    // only activate the neighbors that are face connected
    // to the current pixel. do not include the center pixel
    offset.Fill(0);
    for ( unsigned int d = 0; d < TIterator::Dimension; ++d )
      {
      offset[d] = -1;
      it->ActivateOffset(offset);
      offset[d] = 1;
      it->ActivateOffset(offset);
      offset[d] = 0;
      }
    }
  else
    {
    // activate all neighbors that are face+edge+vertex
    // connected to the current pixel. do not include the center pixel
    unsigned int centerIndex = it->GetCenterNeighborhoodIndex();
    for ( unsigned int d = 0; d < centerIndex * 2 + 1; d++ )
      {
      offset = it->GetOffset(d);
      it->ActivateOffset(offset);
      }
    offset.Fill(0);
    it->DeactivateOffset(offset);
    }
  return it;
}

template< typename TIterator >
TIterator *
setConnectivityPrevious(TIterator *it, bool fullyConnected = false)
{
  // activate the "previous" neighbours
  typename TIterator::OffsetType offset;
  it->ClearActiveList();
  if ( !fullyConnected )
    {
    // only activate the neighbors that are face connected
    // to the current pixel. do not include the center pixel
    offset.Fill(0);
    for ( unsigned int d = 0; d < TIterator::Dimension; ++d )
      {
      offset[d] = -1;
      it->ActivateOffset(offset);
      offset[d] = 0;
      }
    }
  else
    {
    // activate all neighbors that are face+edge+vertex
    // connected to the current pixel. do not include the center pixel
    unsigned int centerIndex = it->GetCenterNeighborhoodIndex();
    for ( unsigned int d = 0; d < centerIndex; d++ )
      {
      offset = it->GetOffset(d);
      it->ActivateOffset(offset);
      }
    offset.Fill(0);
    it->DeactivateOffset(offset);
    }
  return it;
}

template< typename TIterator >
TIterator *
setConnectivityLater(TIterator *it, bool fullyConnected = false)
{
  // activate the "later" neighbours
  typename TIterator::OffsetType offset;
  it->ClearActiveList();
  if ( !fullyConnected )
    {
    // only activate the neighbors that are face connected
    // to the current pixel. do not include the center pixel
    offset.Fill(0);
    for ( unsigned int d = 0; d < TIterator::Dimension; ++d )
      {
      offset[d] = 1;
      it->ActivateOffset(offset);
      offset[d] = 0;
      }
    }
  else
    {
    // activate all neighbors that are face+edge+vertex
    // connected to the current pixel. do not include the center pixel
    unsigned int centerIndex = it->GetCenterNeighborhoodIndex();
    for ( unsigned int d = centerIndex + 1; d < 2 * centerIndex + 1; d++ )
      {
      offset = it->GetOffset(d);
      it->ActivateOffset(offset);
      }
    offset.Fill(0);
    it->DeactivateOffset(offset);
    }
  return it;
}
}

#endif

#ifndef __itkConnectedComponentAlgorithm_h
#define __itkConnectedComponentAlgorithm_h

#include "itkImage.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkShapedNeighborhoodIterator.h"

namespace itk
{
template< class TIterator >
TIterator*
setConnectivity( TIterator* it, bool fullyConnected=false )
{
  typename TIterator::OffsetType offset;
  it->ClearActiveList();
  if( !fullyConnected) 
    {
    // only activate the neighbors that are face connected
    // to the current pixel. do not include the center pixel
    offset.Fill( 0 );
    for( unsigned int d=0; d < TIterator::Dimension; ++d )
      {
      offset[d] = -1;
      it->ActivateOffset( offset );
      offset[d] = 1;
      it->ActivateOffset( offset );
      offset[d] = 0;
      }
    }
  else
    {
    // activate all neighbors that are face+edge+vertex
    // connected to the current pixel. do not include the center pixel
    unsigned int centerIndex = it->GetCenterNeighborhoodIndex();
    for( unsigned int d=0; d < centerIndex*2 + 1; d++ )
      {
      offset = it->GetOffset( d );
      it->ActivateOffset( offset );
      }
    offset.Fill(0);
    it->DeactivateOffset( offset );
    }
  return it;
}

template< class TIterator >
TIterator*
setConnectivityPrevious( TIterator* it, bool fullyConnected=false )
{
  // activate the "previous" neighbours
  typename TIterator::OffsetType offset;
  it->ClearActiveList();
  if( !fullyConnected) 
    {
    // only activate the neighbors that are face connected
    // to the current pixel. do not include the center pixel
    offset.Fill( 0 );
    for( unsigned int d=0; d < TIterator::Dimension; ++d )
      {
      offset[d] = -1;
      it->ActivateOffset( offset );
//       offset[d] = 1;
//       it->ActivateOffset( offset );
      offset[d] = 0;
      }
    }
  else
    {
    // activate all neighbors that are face+edge+vertex
    // connected to the current pixel. do not include the center pixel
    unsigned int centerIndex = it->GetCenterNeighborhoodIndex();
    for( unsigned int d=0; d < centerIndex; d++ )
      {
      offset = it->GetOffset( d );
      it->ActivateOffset( offset );
      }
    offset.Fill(0);
    it->DeactivateOffset( offset );
    }
  return it;
}

template< class TIterator >
TIterator*
setConnectivityLater( TIterator* it, bool fullyConnected=false )
{
  // activate the "later" neighbours
  typename TIterator::OffsetType offset;
  it->ClearActiveList();
  if( !fullyConnected) 
    {
    // only activate the neighbors that are face connected
    // to the current pixel. do not include the center pixel
    offset.Fill( 0 );
    for( unsigned int d=0; d < TIterator::Dimension; ++d )
      {
       offset[d] = 1;
       it->ActivateOffset( offset );
       offset[d] = 0;
      }
    }
  else
    {
    // activate all neighbors that are face+edge+vertex
    // connected to the current pixel. do not include the center pixel
    unsigned int centerIndex = it->GetCenterNeighborhoodIndex();
    for( unsigned int d=centerIndex+1; d < 2*centerIndex+1; d++ )
      {
      offset = it->GetOffset( d );
      it->ActivateOffset( offset );
      }
    offset.Fill(0);
    it->DeactivateOffset( offset );
    }
  return it;
}

}


#endif

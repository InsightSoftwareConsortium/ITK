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
#ifndef itkConstNeighborhoodIterator_hxx
#define itkConstNeighborhoodIterator_hxx
#include "itkConstNeighborhoodIterator.h"
namespace itk
{
template< typename TImage, typename TBoundaryCondition >
bool
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::InBounds() const
{
  if ( m_IsInBoundsValid )
    {
    return m_IsInBounds;
    }

  bool ans = true;
  for ( DimensionValueType i = 0; i < Dimension; i++ )
    {
    if ( m_Loop[i] < m_InnerBoundsLow[i] || m_Loop[i] >= m_InnerBoundsHigh[i] )
      {
      m_InBounds[i] = ans = false;
      }
    else
      {
      m_InBounds[i] = true;
      }
    }
  m_IsInBounds = ans;
  m_IsInBoundsValid = true;
  return ans;
}

template< typename TImage, typename TBoundaryCondition >
bool
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::IndexInBounds(const NeighborIndexType n, OffsetType & internalIndex, OffsetType & offset ) const
{
  if ( !m_NeedToUseBoundaryCondition )
    {
    return true;
    }
  else if ( this->InBounds() ) // Is this whole neighborhood in bounds?
    {
    return true;
    }
  else
    {
    bool flag = true;
    internalIndex = this->ComputeInternalIndex(n);

    // Is this pixel in bounds?
    for ( DimensionValueType i = 0; i < Dimension; ++i )
      {
      if ( m_InBounds[i] )
        {
        offset[i] = 0; // this dimension in bounds
        }
      else  // part of this dimension spills out of bounds
        {
        // Calculate overlap for this dimension
        const OffsetValueType   OverlapLow ( m_InnerBoundsLow[i] - m_Loop[i] );
        if ( internalIndex[i] < OverlapLow )
          {
          flag = false;
          offset[i] = OverlapLow - internalIndex[i];
          }
        else
          {
          const OffsetValueType OverlapHigh ( static_cast< OffsetValueType >( this->GetSize(i) - ( ( m_Loop[i] + 2 ) - m_InnerBoundsHigh[i] ) ) );
          if ( OverlapHigh < internalIndex[i] )
            {
            flag = false;
            offset[i] =  OverlapHigh - internalIndex[i];
            }
          else
            {
            offset[i] = 0;
            }
          }
        }
      }
    return flag;
    }
}

template< class TImage, class TBoundaryCondition >
bool
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::IndexInBounds(const NeighborIndexType n ) const
{
  if ( !m_NeedToUseBoundaryCondition )
    {
    return true;
    }
  else if ( this->InBounds() ) // Is this whole neighborhood in bounds?
    {
    return true;
    }
  else
    {
    bool flag = true;
    const OffsetType & internalIndex = this->ComputeInternalIndex(n);
    // Is this pixel in bounds?
    for ( DimensionValueType i = 0; i < Dimension; ++i )
      {
      if ( ! m_InBounds[i] )
        {
        // Calculate overlap for this dimension
        const OffsetValueType OverlapLow ( m_InnerBoundsLow[i] - m_Loop[i] );
        if ( internalIndex[i] < OverlapLow )
          {
          flag = false;
          }
        else
          {
          const OffsetValueType OverlapHigh ( static_cast< OffsetValueType >( this->GetSize(i) - ( ( m_Loop[i] + 2 ) - m_InnerBoundsHigh[i] ) ) );
          if ( OverlapHigh < internalIndex[i] )
            {
            flag = false;
            }
          }
        }
      }
    return flag;
    }
}

template< typename TImage, typename TBoundaryCondition >
typename ConstNeighborhoodIterator< TImage, TBoundaryCondition >::PixelType
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::GetPixel(NeighborIndexType n, bool & IsInBounds) const
{
  // If the region the iterator is walking (padded by the neighborhood size)
  // never bumps up against the bounds of the buffered region, then don't
  // bother checking any boundary conditions
  if ( !m_NeedToUseBoundaryCondition )
    {
    IsInBounds = true;
    return ( m_NeighborhoodAccessorFunctor.Get( this->operator[](n) ) );
    }

  // Is this whole neighborhood in bounds?
  if ( this->InBounds() )
    {
    IsInBounds = true;
    return ( m_NeighborhoodAccessorFunctor.Get( this->operator[](n) ) );
    }
  else
    {
    bool         flag;
    OffsetType   offset, internalIndex;

    flag = this->IndexInBounds( n, internalIndex, offset );
    if ( flag )
      {
      IsInBounds = true;
      return ( m_NeighborhoodAccessorFunctor.Get( this->operator[](n) ) );
      }
    else
      {
      IsInBounds = false;
      return ( m_NeighborhoodAccessorFunctor.BoundaryCondition( internalIndex, offset, this, this->m_BoundaryCondition ) );
      }
    }
}

template< typename TImage, typename TBoundaryCondition >
typename ConstNeighborhoodIterator< TImage, TBoundaryCondition >::OffsetType
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::ComputeInternalIndex(const NeighborIndexType n) const
{
  OffsetType ans;
  const long D = (long)Dimension;
  unsigned long r = (unsigned long)n;
  for ( long i = D - 1; i >= 0; --i )
    {
    ans[i] = static_cast< OffsetValueType >( r / this->GetStride(i) );
    r = r % this->GetStride(i);
    }
  return ans;
}

template< typename TImage, typename TBoundaryCondition >
typename ConstNeighborhoodIterator< TImage, TBoundaryCondition >::RegionType
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::GetBoundingBoxAsImageRegion() const
{
  const IndexValueType zero = NumericTraits< IndexValueType >::ZeroValue();
  RegionType ans;

  ans.SetIndex( this->GetIndex(zero) );
  ans.SetSize( this->GetSize() );

  return ans;
}

template< typename TImage, typename TBoundaryCondition >
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::ConstNeighborhoodIterator() :
  m_IsInBounds(false),
  m_IsInBoundsValid(false),
  m_NeedToUseBoundaryCondition(false)
{
  IndexType zeroIndex; zeroIndex.Fill(0);
  SizeType zeroSize; zeroSize.Fill(0);

  m_Bound.Fill(0);
  m_Begin = ITK_NULLPTR;
  m_BeginIndex.Fill(0);

  m_End   = ITK_NULLPTR;
  m_EndIndex.Fill(0);
  m_Loop.Fill(0);
  m_Region.SetIndex(zeroIndex);
  m_Region.SetSize(zeroSize);

  m_WrapOffset.Fill(0);

  for ( DimensionValueType i = 0; i < Dimension; i++ )
    {
    m_InBounds[i] = false;
    }

  this->ResetBoundaryCondition();

  m_BoundaryCondition = &m_InternalBoundaryCondition;
}

template< typename TImage, typename TBoundaryCondition >
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::ConstNeighborhoodIterator(const Self & orig):
  Neighborhood< InternalPixelType *, Dimension >(orig)
{
  m_Bound      = orig.m_Bound;
  m_Begin      = orig.m_Begin;
  m_BeginIndex = orig.m_BeginIndex;
  m_ConstImage = orig.m_ConstImage;
  m_End        = orig.m_End;
  m_EndIndex   = orig.m_EndIndex;
  m_Loop       = orig.m_Loop;
  m_Region     = orig.m_Region;
  m_WrapOffset = orig.m_WrapOffset;

  m_InternalBoundaryCondition = orig.m_InternalBoundaryCondition;
  m_NeedToUseBoundaryCondition = orig.m_NeedToUseBoundaryCondition;
  for ( DimensionValueType i = 0; i < Dimension; ++i )
    {
    m_InBounds[i] = orig.m_InBounds[i];
    }
  m_IsInBoundsValid = orig.m_IsInBoundsValid;
  m_IsInBounds = orig.m_IsInBounds;

  m_InnerBoundsLow  = orig.m_InnerBoundsLow;
  m_InnerBoundsHigh = orig.m_InnerBoundsHigh;

  // Check to see if the default boundary
  // conditions have been overridden.
  if ( orig.m_BoundaryCondition ==
       static_cast< ImageBoundaryConditionConstPointerType >(
         &orig.m_InternalBoundaryCondition ) )
    {
    this->ResetBoundaryCondition();
    }
  else
    {
    m_BoundaryCondition = orig.m_BoundaryCondition;
    }
  m_NeighborhoodAccessorFunctor = orig.m_NeighborhoodAccessorFunctor;
}

template< typename TImage, typename TBoundaryCondition >
void
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::SetEndIndex()
{
  if ( m_Region.GetNumberOfPixels() > 0 )
    {
    m_EndIndex = m_Region.GetIndex();
    m_EndIndex[Dimension - 1] = m_Region.GetIndex()[Dimension - 1]
                                + static_cast< OffsetValueType >( m_Region.GetSize()[Dimension - 1] );
    }
  else
    {
    // Region has no pixels, so set the end index to be the begin index
    m_EndIndex = m_Region.GetIndex();
    }
}

template< typename TImage, typename TBoundaryCondition >
typename ConstNeighborhoodIterator< TImage, TBoundaryCondition >::NeighborhoodType
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::GetNeighborhood() const
{
  OffsetType            OverlapLow, OverlapHigh, temp, offset;

  const ConstIterator _end = this->End();
  NeighborhoodType    ans;

  typename NeighborhoodType::Iterator ansIt;
  ConstIterator thisIt;

  ans.SetRadius( this->GetRadius() );

  if ( m_NeedToUseBoundaryCondition == false )
    {
    for ( ansIt = ans.Begin(), thisIt = this->Begin();
          thisIt < _end; ++ansIt, ++thisIt )
      {
      *ansIt = m_NeighborhoodAccessorFunctor.Get(*thisIt);
      }
    }
  else if ( InBounds() )
    {
    for ( ansIt = ans.Begin(), thisIt = this->Begin();
          thisIt < _end; ++ansIt, ++thisIt )
      {
      *ansIt = m_NeighborhoodAccessorFunctor.Get(*thisIt);
      }
    }
  else
    {
    // Calculate overlap & initialize index
    for ( DimensionValueType i = 0; i < Dimension; i++ )
      {
      OverlapLow[i] = m_InnerBoundsLow[i] - m_Loop[i];
      OverlapHigh[i] =
        static_cast< OffsetValueType >( this->GetSize(i) ) - ( ( m_Loop[i] + 2 )
                                                               - m_InnerBoundsHigh[i] );
      temp[i] = 0;
      }

    // Iterate through neighborhood
    for ( ansIt = ans.Begin(), thisIt = this->Begin();
          thisIt < _end; ++ansIt, ++thisIt )
      {
      bool flag = true;

      // Is this pixel in bounds?
      for ( DimensionValueType i = 0; i < Dimension; ++i )
        {
        if ( m_InBounds[i] )
          {
          offset[i] = 0;                  // this dimension in bounds
          }
        else  // part of this dimension spills out of bounds
          {
          if ( temp[i] < OverlapLow[i] )
            {
            flag = false;
            offset[i] = OverlapLow[i] - temp[i];
            }
          else if ( OverlapHigh[i] < temp[i] )
            {
            flag = false;
            offset[i] =  OverlapHigh[i] - temp[i];
            }
          else { offset[i] = 0; }
          }
        }

      if ( flag )
        {
        *ansIt = m_NeighborhoodAccessorFunctor.Get(*thisIt);
        }
      else
        {
        *ansIt = m_NeighborhoodAccessorFunctor.BoundaryCondition(
          temp, offset, this, this->m_BoundaryCondition);
        }

      m_BoundaryCondition->operator()(temp, offset, this);

      for ( DimensionValueType i = 0; i < Dimension; ++i )  // Update index
        {
        temp[i]++;
        if ( temp[i] == static_cast< OffsetValueType >( this->GetSize(i) ) )
          {
          temp[i] = 0;
          }
        else
          {
          break;
          }
        }
      }
    }
  return ans;
}

template< typename TImage, typename TBoundaryCondition >
void
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::GoToBegin()
{
  this->SetLocation(m_BeginIndex);
}

template< typename TImage, typename TBoundaryCondition >
void
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::GoToEnd()
{
  this->SetLocation(m_EndIndex);
}

template< typename TImage, typename TBoundaryCondition >
void
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::SetRegion(const RegionType & region)
{
  m_Region = region;

  const IndexType regionIndex = region.GetIndex();

  this->SetBeginIndex( region.GetIndex() );
  this->SetLocation( region.GetIndex() );
  this->SetBound( region.GetSize() );
  this->SetEndIndex();

  m_Begin = m_ConstImage->GetBufferPointer() + m_ConstImage->ComputeOffset(regionIndex);

  m_End = m_ConstImage->GetBufferPointer() + m_ConstImage->ComputeOffset(m_EndIndex);

  // now determine whether boundary conditions are going to be needed
  const IndexType bStart = m_ConstImage->GetBufferedRegion().GetIndex();
  const SizeType  bSize  = m_ConstImage->GetBufferedRegion().GetSize();
  const IndexType rStart = region.GetIndex();
  const SizeType  rSize  = region.GetSize();

  m_NeedToUseBoundaryCondition = false;
  for ( DimensionValueType i = 0; i < Dimension; ++i )
    {
    const OffsetValueType overlapLow = static_cast< OffsetValueType >( ( rStart[i]
                                       - static_cast<OffsetValueType>( this->GetRadius(i) ) ) - bStart[i] );
    const OffsetValueType overlapHigh = static_cast< OffsetValueType >( ( bStart[i] + bSize[i] )
                                       - ( rStart[i] + rSize[i] + static_cast<OffsetValueType>( this->GetRadius(i) ) ) );

    if ( overlapLow < 0 ) // out of bounds condition, define a region of
      {
      m_NeedToUseBoundaryCondition = true;
      break;
      }

    if ( overlapHigh < 0 )
      {
      m_NeedToUseBoundaryCondition = true;
      break;
      }
    }
}

template< typename TImage, typename TBoundaryCondition >
void ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::Initialize(const SizeType & radius, const ImageType *ptr,
             const RegionType & region)
{
  m_ConstImage = ptr;

  this->SetRadius(radius);

  SetRegion(region);

  m_IsInBoundsValid = false;
  m_IsInBounds = false;
}

template< typename TImage, typename TBoundaryCondition >
ConstNeighborhoodIterator< TImage, TBoundaryCondition > &
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::operator=(const Self & orig)
{
  if(this != &orig)
    {
    Superclass::operator=(orig);

    m_Bound        = orig.m_Bound;
    m_Begin        = orig.m_Begin;
    m_ConstImage   = orig.m_ConstImage;
    m_End          = orig.m_End;
    m_EndIndex     = orig.m_EndIndex;
    m_Loop         = orig.m_Loop;
    m_Region       = orig.m_Region;
    m_BeginIndex = orig.m_BeginIndex;
    m_WrapOffset = orig.m_WrapOffset;

    m_InternalBoundaryCondition = orig.m_InternalBoundaryCondition;
    m_NeedToUseBoundaryCondition = orig.m_NeedToUseBoundaryCondition;

    m_InnerBoundsLow  = orig.m_InnerBoundsLow;
    m_InnerBoundsHigh = orig.m_InnerBoundsHigh;

    for ( DimensionValueType i = 0; i < Dimension; ++i )
      {
      m_InBounds[i] = orig.m_InBounds[i];
      }
    m_IsInBoundsValid = orig.m_IsInBoundsValid;
    m_IsInBounds = orig.m_IsInBounds;

    // Check to see if the default boundary conditions
    // have been overridden.
    if ( orig.m_BoundaryCondition ==
         static_cast< ImageBoundaryConditionConstPointerType >(
           &orig.m_InternalBoundaryCondition ) )
      {
      this->ResetBoundaryCondition();
      }
    else { m_BoundaryCondition = orig.m_BoundaryCondition; }
    m_NeighborhoodAccessorFunctor = orig.m_NeighborhoodAccessorFunctor;
    }
  return *this;
}

template< typename TImage, typename TBoundaryCondition >
ConstNeighborhoodIterator< TImage, TBoundaryCondition > &
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::operator++()
{
  Iterator       it;
  const Iterator _end = Superclass::End();

  // Repositioning neighborhood, previous bounds check on neighborhood
  // location is invalid.
  m_IsInBoundsValid = false;

  // Increment pointers.
  for ( it = Superclass::Begin(); it < _end; ++it )
    {
    ( *it )++;
    }

  // Check loop bounds, wrap & add pointer offsets if needed.
  for ( DimensionValueType i = 0; i < Dimension; ++i )
    {
    m_Loop[i]++;
    if ( m_Loop[i] == m_Bound[i] )
      {
      m_Loop[i] = m_BeginIndex[i];
      for ( it = Superclass::Begin(); it < _end; ++it )
        {
        ( *it ) += m_WrapOffset[i];
        }
      }
    else { break; }
    }
  return *this;
}

template< typename TImage, typename TBoundaryCondition >
ConstNeighborhoodIterator< TImage, TBoundaryCondition > &
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::operator--()
{
  Iterator       it;
  const Iterator _end = Superclass::End();

  // Repositioning neighborhood, previous bounds check on neighborhood
  // location is invalid.
  m_IsInBoundsValid = false;

  // Decrement pointers.
  for ( it = Superclass::Begin(); it < _end; ++it )
    {
    ( *it )--;
    }

  // Check loop bounds, wrap & add pointer offsets if needed.
  for ( DimensionValueType i = 0; i < Dimension; ++i )
    {
    if ( m_Loop[i] == m_BeginIndex[i] )
      {
      m_Loop[i] = m_Bound[i] - 1;
      for ( it = Superclass::Begin(); it < _end; ++it )
        {
        ( *it ) -= m_WrapOffset[i];
        }
      }
    else
      {
      m_Loop[i]--;
      break;
      }
    }
  return *this;
}

template< typename TImage, typename TBoundaryCondition >
void
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::PrintSelf(std::ostream & os, Indent indent) const
{
  DimensionValueType i;

  os << indent;
  os << "ConstNeighborhoodIterator {this= " << this;
  os << ", m_Region = { Start = {";
  for ( i = 0; i < Dimension; ++i )
    {
    os << m_Region.GetIndex()[i] << " ";
    }
  os << "}, Size = { ";
  for ( i = 0; i < Dimension; ++i )
    {
    os << m_Region.GetSize()[i] << " ";
    }
  os << "} }";
  os << ", m_BeginIndex = { ";
  for ( i = 0; i < Dimension; ++i )
    {
    os << m_BeginIndex[i] << " ";
    }
  os << "} , m_EndIndex = { ";
  for ( i = 0; i < Dimension; ++i )
    {
    os << m_EndIndex[i] << " ";
    }
  os << "} , m_Loop = { ";
  for ( i = 0; i < Dimension; ++i )
    {
    os << m_Loop[i] << " ";
    }
  os << "}, m_Bound = { ";
  for ( i = 0; i < Dimension; ++i )
    {
    os << m_Bound[i] << " ";
    }
  os << "}, m_IsInBounds = {" << m_IsInBounds;
  os << "}, m_IsInBoundsValid = {" << m_IsInBoundsValid;
  os << "}, m_WrapOffset = { ";
  for ( i = 0; i < Dimension; ++i )
    {
    os << m_WrapOffset[i] << " ";
    }
  os << ", m_Begin = " << m_Begin;
  os << ", m_End = " << m_End;
  os << "}"  << std::endl;

  os << indent << ",  m_InnerBoundsLow = { ";
  for ( i = 0; i < Dimension; i++ )
    {
    os << m_InnerBoundsLow[i] << " ";
    }
  os << "}, m_InnerBoundsHigh = { ";
  for ( i = 0; i < Dimension; i++ )
    {
    os << m_InnerBoundsHigh[i] << " ";
    }
  os << "} }" << std::endl;
  Superclass::PrintSelf( os, indent.GetNextIndent() );
}

template< typename TImage, typename TBoundaryCondition >
void ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::SetBound(const SizeType & size)
{
  SizeType               radius  = this->GetRadius();
  const OffsetValueType *offset   = m_ConstImage->GetOffsetTable();
  const IndexType        imageBRStart  = m_ConstImage->GetBufferedRegion().GetIndex();
  SizeType               imageBRSize = m_ConstImage->GetBufferedRegion().GetSize();

  // Set the bounds and the wrapping offsets. Inner bounds are the loop
  // indices where the iterator will begin to overlap the edge of the image
  // buffered region.
  for ( DimensionValueType i = 0; i < Dimension; ++i )
    {
    m_Bound[i] = m_BeginIndex[i] + static_cast< OffsetValueType >( size[i] );
    m_InnerBoundsHigh[i] = static_cast< IndexValueType >( imageBRStart[i]
                                                          + static_cast< OffsetValueType >( imageBRSize[i] )
                                                          - static_cast< OffsetValueType >( radius[i] ) );
    m_InnerBoundsLow[i] = static_cast< IndexValueType >( imageBRStart[i]
                                                         + static_cast< OffsetValueType >( radius[i] ) );
    m_WrapOffset[i]     = ( static_cast< OffsetValueType >( imageBRSize[i] )
                            - ( m_Bound[i] - m_BeginIndex[i] ) ) * offset[i];
    }
  m_WrapOffset[Dimension - 1] = 0; // last offset is zero because there are no
                                   // higher dimensions
}

template< typename TImage, typename TBoundaryCondition >
void ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::SetPixelPointers(const IndexType & pos)
{
  const Iterator         _end = Superclass::End();
  InternalPixelType *    Iit;
  ImageType *            ptr = const_cast< ImageType * >( m_ConstImage.GetPointer() );
  const SizeType         size = this->GetSize();
  const OffsetValueType *OffsetTable = m_ConstImage->GetOffsetTable();
  const SizeType         radius = this->GetRadius();

  DimensionValueType  i;
  Iterator            Nit;
  SizeType            loop;

  for ( i = 0; i < Dimension; ++i )
    {
    loop[i] = 0;
    }

  // Find first "upper-left-corner"  pixel address of neighborhood
  Iit = ptr->GetBufferPointer() + ptr->ComputeOffset(pos);

  for ( i = 0; i < Dimension; ++i )
    {
    Iit -= radius[i] * OffsetTable[i];
    }

  // Compute the rest of the pixel addresses
  for ( Nit = Superclass::Begin(); Nit != _end; ++Nit )
    {
    *Nit = Iit;
    ++Iit;
    for ( i = 0; i < Dimension; ++i )
      {
      loop[i]++;
      if ( loop[i] == size[i] )
        {
        if ( i == Dimension - 1 ) { break; }
        Iit += OffsetTable[i + 1] - OffsetTable[i] * static_cast< OffsetValueType >( size[i] );
        loop[i] = 0;
        }
      else { break; }
      }
    }
}

template< typename TImage, typename TBoundaryCondition >
ConstNeighborhoodIterator< TImage, TBoundaryCondition > &
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::operator+=(const OffsetType & idx)
{
  Iterator               it;
  const Iterator         _end = this->End();
  OffsetValueType        accumulator = 0;
  const OffsetValueType *stride = this->GetImagePointer()->GetOffsetTable();

  // Repositioning neighborhood, previous bounds check on neighborhood
  // location is invalid.
  m_IsInBoundsValid = false;

  // Offset from the increment in the lowest dimension
  accumulator += idx[0];

  // Offsets from the stride lengths in each dimension.
  //
  // Because the image offset table is based on its buffer size and not its
  // requested region size, we don't have to worry about adding in the wrapping
  // offsets.
  for ( DimensionValueType i = 1; i < Dimension; ++i )
    {
    accumulator += idx[i] * stride[i];
    }

  // Increment pointers.
  for ( it = this->Begin(); it < _end; ++it )
    {
    ( *it ) += accumulator;
    }

  // Update loop counter values
  m_Loop += idx;

  return *this;
}

template< typename TImage, typename TBoundaryCondition >
ConstNeighborhoodIterator< TImage, TBoundaryCondition > &
ConstNeighborhoodIterator< TImage, TBoundaryCondition >
::operator-=(const OffsetType & idx)
{
  Iterator               it;
  const Iterator         _end = this->End();
  OffsetValueType        accumulator = 0;
  const OffsetValueType *stride = this->GetImagePointer()->GetOffsetTable();

  // Repositioning neighborhood, previous bounds check on neighborhood
  // location is invalid.
  m_IsInBoundsValid = false;

  // Offset from the increment in the lowest dimension
  accumulator += idx[0];

  // Offsets from the stride lengths in each dimension.
  //
  // Because the image offset table is based on its buffer size and not its
  // requested region size, we don't have to worry about adding in the wrapping
  // offsets.
  for ( DimensionValueType i = 1; i < Dimension; ++i )
    {
    accumulator += idx[i] * stride[i];
    }

  // Increment pointers.
  for ( it = this->Begin(); it < _end; ++it )
    {
    ( *it ) -= accumulator;
    }

  // Update loop counter values
  m_Loop -= idx;

  return *this;
}
} // namespace itk

#endif

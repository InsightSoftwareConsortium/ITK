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
#ifndef itkConstNeighborhoodIteratorWithOnlyIndex_hxx
#define itkConstNeighborhoodIteratorWithOnlyIndex_hxx

#include "itkConstNeighborhoodIteratorWithOnlyIndex.h"

namespace itk
{
template< typename TImage >
bool
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
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

template< typename TImage >
bool
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
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
    bool              flag = true;
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
        const OffsetValueType   OverlapLow = m_InnerBoundsLow[i] - m_Loop[i];
        if ( internalIndex[i] < OverlapLow )
          {
          flag = false;
          offset[i] = OverlapLow - internalIndex[i];
          }
        else
          {
          const OffsetValueType OverlapHigh = static_cast< OffsetValueType >( this->GetSize(i) - ( ( m_Loop[i] + 2 ) - m_InnerBoundsHigh[i] ) );
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

template< typename TImage >
typename ConstNeighborhoodIteratorWithOnlyIndex< TImage >::OffsetType
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::ComputeInternalIndex(NeighborIndexType n) const
{
  OffsetType    ans;
  const long D = (long)Dimension;
  unsigned long r = (unsigned long)n;
  for ( long i = D - 1; i >= 0; --i )
    {
    ans[i] = static_cast< OffsetValueType >( r / this->GetStride(i) );
    r = r % this->GetStride(i);
    }
  return ans;
}

template< typename TImage >
typename ConstNeighborhoodIteratorWithOnlyIndex< TImage >::RegionType
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::GetBoundingBoxAsImageRegion() const
{
  const IndexValueType zero = NumericTraits< IndexValueType >::ZeroValue();
  RegionType ans;
  ans.SetIndex( this->GetIndex(zero) );
  ans.SetSize( this->GetSize() );

  return ans;
}

template< typename TImage >
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::ConstNeighborhoodIteratorWithOnlyIndex()
{
  IndexType zeroIndex;
  zeroIndex.Fill(0);

  SizeType zeroSize;
  zeroSize.Fill(0);

  m_Bound.Fill(0);
  m_BeginIndex.Fill(0);
  m_EndIndex.Fill(0);
  m_Loop.Fill(0);
  m_Region.SetIndex(zeroIndex);
  m_Region.SetSize(zeroSize);

  for ( DimensionValueType i = 0; i < Dimension; i++ )
    {
    m_InBounds[i] = false;
    }

  m_IsInBounds = false;
  m_IsInBoundsValid = false;
  m_NeedToUseBoundaryCondition = false;
}

template< typename TImage >
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::ConstNeighborhoodIteratorWithOnlyIndex(const Self & orig):
  Neighborhood< DummyNeighborhoodPixelType, Dimension >(orig)
{
  m_Bound      = orig.m_Bound;
  m_BeginIndex = orig.m_BeginIndex;
  m_ConstImage = orig.m_ConstImage;
  m_EndIndex   = orig.m_EndIndex;
  m_Loop       = orig.m_Loop;
  m_Region     = orig.m_Region;

  m_NeedToUseBoundaryCondition = orig.m_NeedToUseBoundaryCondition;
  for ( DimensionValueType i = 0; i < Dimension; ++i )
    {
    m_InBounds[i] = orig.m_InBounds[i];
    }
  m_IsInBoundsValid = orig.m_IsInBoundsValid;
  m_IsInBounds = orig.m_IsInBounds;

  m_InnerBoundsLow  = orig.m_InnerBoundsLow;
  m_InnerBoundsHigh = orig.m_InnerBoundsHigh;
}

template< typename TImage >
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::ConstNeighborhoodIteratorWithOnlyIndex(const SizeType & radius, const ImageType *ptr, const RegionType & region)
{
  this->Initialize(radius, ptr, region);
  for ( unsigned int i = 0; i < Dimension; i++ )
    {
    m_InBounds[i] = false;
    }
}

template< typename TImage >
void
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::SetEndIndex()
{
  if ( m_Region.GetNumberOfPixels() > 0 )
    {
    m_EndIndex = m_Region.GetIndex();
    m_EndIndex[Dimension - 1] = m_Region.GetIndex()[Dimension - 1] + static_cast< OffsetValueType >( m_Region.GetSize()[Dimension - 1] );
    }
  else
    {
    // Region has no pixels, so set the end index to be the begin index
    m_EndIndex = m_Region.GetIndex();
    }
}

template< typename TImage >
void
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::GoToBegin()
{
  this->SetLocation(m_BeginIndex);
}

template< typename TImage >
void
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::GoToEnd()
{
  this->SetLocation(m_EndIndex);
}

template< typename TImage >
void ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::Initialize(const SizeType & radius, const ImageType *ptr, const RegionType & region)
{
  m_ConstImage = ptr;
  m_Region = region;

  this->SetRadius(radius);
  this->SetBeginIndex( region.GetIndex() );
  this->SetLocation( region.GetIndex() );
  this->SetBound( region.GetSize() );
  this->SetEndIndex();

  // now determine whether boundary conditions are going to be needed
  const IndexType bStart = ptr->GetBufferedRegion().GetIndex();
  const SizeType  bSize  = ptr->GetBufferedRegion().GetSize();
  const IndexType rStart = region.GetIndex();
  const SizeType  rSize  = region.GetSize();

  m_NeedToUseBoundaryCondition = false;
  for ( DimensionValueType i = 0; i < Dimension; ++i )
    {
    const OffsetValueType overlapLow = static_cast< OffsetValueType >( ( rStart[i] - static_cast<OffsetValueType>( radius[i] ) ) - bStart[i] );
    const OffsetValueType overlapHigh = static_cast< OffsetValueType >( ( bStart[i] + bSize[i] ) - ( rStart[i] + rSize[i] + static_cast<OffsetValueType>( radius[i] ) ) );

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

  m_IsInBoundsValid = false;
  m_IsInBounds = false;
}

template< typename TImage >
ConstNeighborhoodIteratorWithOnlyIndex< TImage > &
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::operator=(const Self & orig)
{
  if(this != &orig)
    {
    Superclass::operator=(orig);

    m_Bound        = orig.m_Bound;
    m_ConstImage   = orig.m_ConstImage;
    m_EndIndex     = orig.m_EndIndex;
    m_Loop         = orig.m_Loop;
    m_Region       = orig.m_Region;
    m_BeginIndex = orig.m_BeginIndex;

    m_NeedToUseBoundaryCondition = orig.m_NeedToUseBoundaryCondition;

    m_InnerBoundsLow  = orig.m_InnerBoundsLow;
    m_InnerBoundsHigh = orig.m_InnerBoundsHigh;

    for ( DimensionValueType i = 0; i < Dimension; ++i )
      {
      m_InBounds[i] = orig.m_InBounds[i];
      }
    m_IsInBoundsValid = orig.m_IsInBoundsValid;
    m_IsInBounds = orig.m_IsInBounds;
    }
  return *this;
}

template< typename TImage >
bool ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::operator<(const Self & it) const
{
 for( DimensionValueType i = 1; i <= Dimension; i++ )
  {
  if( this->GetIndex()[Dimension - i] < it.GetIndex()[Dimension - i] )
    {
    return true;
    }
  if( this->GetIndex()[Dimension - i] > it.GetIndex()[Dimension - i] )
    {
    return false;
    }
  }
  return false;
}

template< typename TImage >
bool ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::operator<=(const Self & it) const
{
  if( this->operator==( it ) )
    {
    return true;
    }
  return this->operator<( it );
}

template< typename TImage >
bool ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::operator>(const Self & it) const
{
 for( DimensionValueType i = 1; i <= Dimension; i++ )
  {
  if( this->GetIndex()[Dimension - i] > it.GetIndex()[Dimension - i] )
    {
    return true;
    }
  if( this->GetIndex()[Dimension - i] < it.GetIndex()[Dimension - i] )
    {
    return false;
    }
  }
  return false;
}

template< typename TImage >
bool ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::operator>=(const Self & it) const
{
  if( this->operator==( it ) )
    {
    return true;
    }
  return this->operator>( it );
}

template< typename TImage >
bool ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::IsAtEnd() const
{
  // m_EndIndex is at end of iteration when its greatest dimension
  // is at the bound for that dimension.
  if ( this->GetIndex()[Dimension - 1] > this->m_EndIndex[Dimension - 1] )
    {
    ExceptionObject    e(__FILE__, __LINE__);
    std::ostringstream msg;
    msg << "In method IsAtEnd, GetIndex()[Dimension - 1] = " << GetIndex()[Dimension - 1]
        << " is greater than m_EndIndex[Dimension - 1] = " << this->m_EndIndex[Dimension - 1]
        << std::endl
        << "  " << *this;
    e.SetDescription( msg.str().c_str() );
    throw e;
    }
  return ( this->GetIndex()[Dimension - 1] == this->m_EndIndex[Dimension - 1] );
}

template< typename TImage >
ConstNeighborhoodIteratorWithOnlyIndex< TImage > &
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::operator++()
{
  // Repositioning neighborhood, previous bounds check on neighborhood
  // location is invalid.
  m_IsInBoundsValid = false;

  // Check loop bounds, wrap.
  for ( DimensionValueType i = 0; i < Dimension; ++i )
    {
    m_Loop[i]++;
    if ( m_Loop[i] == m_Bound[i] )
      {
      // Let the last dimension stay at m_Bound[i],
      // signifying we are at the end.
      if( i < ( Dimension - 1 ) )
        {
        m_Loop[i] = m_BeginIndex[i];
        }
      }
    else
      {
      break;
      }
    }
  return *this;
}

template< typename TImage >
ConstNeighborhoodIteratorWithOnlyIndex< TImage > &
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::operator--()
{
  // Repositioning neighborhood, previous bounds check on neighborhood
  // location is invalid.
  m_IsInBoundsValid = false;

  // Check loop bounds, wrap.
  for ( DimensionValueType i = 0; i < Dimension; ++i )
    {
    if ( m_Loop[i] == m_BeginIndex[i] )
      {
      m_Loop[i] = m_Bound[i] - 1;
      }
    else
      {
      m_Loop[i]--;
      break;
      }
    }
  return *this;
}

template< typename TImage >
void
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  DimensionValueType i;

  os << indent;
  os << "ConstNeighborhoodIteratorWithOnlyIndex {this= " << this;
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

template< typename TImage >
void ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::SetBound(const SizeType & size)
{
  SizeType               radius  = this->GetRadius();
  const IndexType        imageBRStart  = m_ConstImage->GetBufferedRegion().GetIndex();
  SizeType               imageBRSize = m_ConstImage->GetBufferedRegion().GetSize();

  // Set the bounds and the wrapping offsets. Inner bounds are the loop
  // indices where the iterator will begin to overlap the edge of the image
  // buffered region.
  for ( DimensionValueType i = 0; i < Dimension; ++i )
    {
    m_Bound[i] = m_BeginIndex[i] + static_cast< OffsetValueType >( size[i] );
    m_InnerBoundsHigh[i] = static_cast< IndexValueType >( imageBRStart[i] + static_cast< OffsetValueType >( imageBRSize[i] ) - static_cast< OffsetValueType >( radius[i] ) );
    m_InnerBoundsLow[i] = static_cast< IndexValueType >( imageBRStart[i] + static_cast< OffsetValueType >( radius[i] ) );
    }
}

template< typename TImage >
ConstNeighborhoodIteratorWithOnlyIndex< TImage > &
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::operator+=(const OffsetType & idx)
{
  // Repositioning neighborhood, previous bounds check on neighborhood
  // location is invalid.
  m_IsInBoundsValid = false;

  // Update loop counter values
  m_Loop += idx;

  return *this;
}

template< typename TImage >
ConstNeighborhoodIteratorWithOnlyIndex< TImage > &
ConstNeighborhoodIteratorWithOnlyIndex< TImage >
::operator-=(const OffsetType & idx)
{
  // Repositioning neighborhood, previous bounds check on neighborhood
  // location is invalid.
  m_IsInBoundsValid = false;

  // Update loop counter values
  m_Loop -= idx;
  return *this;
}
} // namespace itk

#endif

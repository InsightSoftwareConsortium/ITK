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
#ifndef __itkNeighborhood_txx
#define __itkNeighborhood_txx

#include "itkNeighborhood.h"
#include <cstring>

namespace itk
{
template< class TPixel, unsigned int VDimension, class TContainer >
void
Neighborhood< TPixel, VDimension, TContainer >
::ComputeNeighborhoodStrideTable()
{
  unsigned stride, accum;

  for ( unsigned int dim = 0; dim < VDimension; ++dim )
    {
    stride = 0;
    accum = 1;

    for ( unsigned int i = 0; i < VDimension; ++i )
      {
      if ( i == dim ) { stride = accum; }
      accum *= m_Size[i];
      }

    m_StrideTable[dim] = stride;
    }
}

template< class TPixel, unsigned int VDimension, class TContainer >
void Neighborhood< TPixel, VDimension, TContainer >
::ComputeNeighborhoodOffsetTable()
{
  m_OffsetTable.clear();
  m_OffsetTable.reserve( this->Size() );
  OffsetType   o;
  unsigned int i, j;
  for ( j = 0; j < VDimension; j++ )
    {
    o[j] = -( static_cast< OffsetValueType >( this->GetRadius(j) ) );
    }

  for ( i = 0; i < this->Size(); ++i )
    {
    m_OffsetTable.push_back(o);
    for ( j = 0; j < VDimension; j++ )
      {
      o[j] = o[j] + 1;
      if ( o[j] > static_cast< OffsetValueType >( this->GetRadius(j) ) )
        {
        o[j] = -( static_cast< OffsetValueType >( this->GetRadius(j) ) );
        }
      else { break; }
      }
    }
}

template< class TPixel, unsigned int VDimension, class TContainer >
void
Neighborhood< TPixel, VDimension, TContainer >
::SetRadius(const SizeValueType s)
{
  SizeType k;

  for ( unsigned int i = 0; i < VDimension; i++ )
    {
    k[i] = s;
    }
  this->SetRadius(k);
}

template< class TPixel, unsigned int VDimension, class TContainer >
void
Neighborhood< TPixel, VDimension, TContainer >
::SetRadius(const SizeType & r)
{
  memcpy(m_Radius.m_Size, r.m_Size, sizeof( const SizeValueType ) * VDimension);
  this->SetSize();

  unsigned int cumul = 1;
  for ( unsigned int i = 0; i < VDimension; i++ )
    {
    cumul *= m_Size[i];
    }

  this->Allocate(cumul);
  this->ComputeNeighborhoodStrideTable();
  this->ComputeNeighborhoodOffsetTable();
}

template< class TPixel, unsigned int VDimension, class TContainer >
Neighborhood< TPixel, VDimension, TContainer >
::Neighborhood(const Self & other)
{
  m_Radius     = other.m_Radius;
  m_Size       = other.m_Size;
  m_DataBuffer = other.m_DataBuffer;
  ::memcpy(m_StrideTable, other.m_StrideTable, sizeof( OffsetValueType ) * VDimension);
  m_OffsetTable = other.m_OffsetTable;
}

template< class TPixel, unsigned int VDimension, class TContainer >
Neighborhood< TPixel, VDimension, TContainer > &
Neighborhood< TPixel, VDimension, TContainer >
::operator=(const Self & other)
{
  m_Radius     = other.m_Radius;
  m_Size       = other.m_Size;
  m_DataBuffer = other.m_DataBuffer;
  ::memcpy(m_StrideTable, other.m_StrideTable, sizeof( OffsetValueType ) * VDimension);
  m_OffsetTable = other.m_OffsetTable;
  return *this;
}

template< class TPixel, unsigned int VDimension, class TContainer >
std::slice Neighborhood< TPixel, VDimension, TContainer >
::GetSlice(unsigned int d) const
{
  unsigned int n = this->Size() / 2;

  n = n - ( static_cast< unsigned >( this->GetStride(d) )
            * ( static_cast< unsigned >( this->GetSize()[d] / 2 ) ) );

  return std::slice( static_cast< size_t >( n ),
                     static_cast< size_t >( this->GetSize()[d] ),
                     static_cast< size_t >( this->GetStride(d) ) );
}

template< class TPixel, unsigned int VDimension, class TContainer >
typename Neighborhood< TPixel, VDimension, TContainer >::NeighborIndexType
Neighborhood< TPixel, VDimension, TContainer >
::GetNeighborhoodIndex(const OffsetType & o) const
{
  unsigned int idx = ( this->Size() / 2 );

  for ( unsigned i = 0; i < VDimension; ++i )
    {
    idx += o[i] * static_cast< OffsetValueType >( m_StrideTable[i] );
    }
  return idx;
}

template< class TPixel, unsigned int VDimension, class TContainer >
void Neighborhood< TPixel, VDimension, TContainer >
::PrintSelf(std::ostream & os, Indent indent) const
{
  unsigned int i;

  os << indent << "m_Size: [ ";
  for ( i = 0; i < VDimension; ++i )
    {
    os << m_Size[i] << " ";
    }
  os << "]" << std::endl;

  os << indent << "m_Radius: [ ";
  for ( i = 0; i < VDimension; ++i )
    {
    os << m_Radius[i] << " ";
    }
  os << "]" << std::endl;

  os << indent << "m_StrideTable: [ ";
  for ( i = 0; i < VDimension; ++i )
    {
    os << m_StrideTable[i] << " ";
    }
  os << "]" << std::endl;

  os << indent << "m_OffsetTable: [ ";
  for ( i = 0; i < m_OffsetTable.size(); ++i )
    {
    os << m_OffsetTable[i] << " ";
    }
  os << "]" << std::endl;
}
}  // namespace itk

#endif

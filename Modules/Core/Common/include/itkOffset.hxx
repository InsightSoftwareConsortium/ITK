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
#ifndef __itkOffset_hxx
#define __itkOffset_hxx

#include "itkOffset.h"

namespace itk
{

template <unsigned int VOffsetDimension>
const typename Offset<VOffsetDimension>::Self
Offset<VOffsetDimension>
::operator+(const Self & offset) const
{
  Self result;

  for( unsigned int i = 0; i < VOffsetDimension; ++i )
    {
    result[i] = m_Offset[i] + offset[i];
    }
  return result;
}

template <unsigned int VOffsetDimension>
const typename Offset<VOffsetDimension>::Self
Offset<VOffsetDimension>
::operator+(const Size<VOffsetDimension> & size) const
{
  Self result;

  for( unsigned int i = 0; i < VOffsetDimension; ++i )
    {
    result[i] = m_Offset[i] + size[i];
    }
  return result;
}

template <unsigned int VOffsetDimension>
const typename Offset<VOffsetDimension>::Self &
Offset<VOffsetDimension>
::operator+=(const Size<VOffsetDimension> & size)
{
  for( unsigned int i = 0; i < VOffsetDimension; ++i )
    {
    m_Offset[i] += size[i];
    }
  return *this;
}

template <unsigned int VOffsetDimension>
const typename Offset<VOffsetDimension>::Self &
Offset<VOffsetDimension>
::operator-=(const Size<VOffsetDimension> & size)
{
  for( unsigned int i = 0; i < VOffsetDimension; ++i )
    {
    m_Offset[i] -= size[i];
    }
  return *this;
}

template <unsigned int VOffsetDimension>
const typename Offset<VOffsetDimension>::Self
Offset<VOffsetDimension>
::operator-(const Self & vec)
{
  Self result;

  for( unsigned int i = 0; i < VOffsetDimension; ++i )
    {
    result[i] = m_Offset[i] - vec.m_Offset[i];
    }
  return result;
}

template <unsigned int VOffsetDimension>
const typename Offset<VOffsetDimension>::Self &
Offset<VOffsetDimension>
::operator+=(const Self & vec)
{
  for( unsigned int i = 0; i < VOffsetDimension; ++i )
    {
    m_Offset[i] += vec.m_Offset[i];
    }
  return *this;
}

template <unsigned int VOffsetDimension>
const typename Offset<VOffsetDimension>::Self &
Offset<VOffsetDimension>
::operator-=(const Self & vec)
{
  for( unsigned int i = 0; i < VOffsetDimension; ++i )
    {
    m_Offset[i] -= vec.m_Offset[i];
    }
  return *this;
}

template <unsigned int VOffsetDimension>
bool
Offset<VOffsetDimension>
::operator==(const Self & vec) const
{
  bool same = 1;

  for( unsigned int i = 0; i < VOffsetDimension && same; ++i )
    {
    same = ( m_Offset[i] == vec.m_Offset[i] );
    }
  return same;
}

template <unsigned int VOffsetDimension>
bool
Offset<VOffsetDimension>
::operator!=(const Self & vec) const
{
  // return !this->operator==(vec);
  bool same = 1;

  for( unsigned int i = 0; i < VOffsetDimension && same; ++i )
    {
    same = ( m_Offset[i] == vec.m_Offset[i] );
    }
  return !same;
}

template <unsigned int VOffsetDimension>
void
Offset<VOffsetDimension>
::SetOffset(const OffsetValueType val[VOffsetDimension])
{
  memcpy(m_Offset, val, sizeof( OffsetValueType ) * VOffsetDimension);
}

template <unsigned int VOffsetDimension>
void
Offset<VOffsetDimension>
::Fill(OffsetValueType value)
{
  for( unsigned int i = 0; i < VOffsetDimension; ++i )
    {
    m_Offset[i] = value;
    }
}

template <unsigned int VOffsetDimension>
Offset<VOffsetDimension>
Offset<VOffsetDimension>
::GetBasisOffset(unsigned int dim)
{
  Self ind;
  memset(ind.m_Offset, 0, sizeof( OffsetValueType ) * VOffsetDimension);
  ind.m_Offset[dim] = 1;
  return ind;
}

namespace Functor
{
template <unsigned int VOffsetDimension>
bool
OffsetLexicographicCompare<VOffsetDimension>
::operator()(Offset<VOffsetDimension> const & l, Offset<VOffsetDimension> const & r) const
{
  for( unsigned int i = 0; i < VOffsetDimension; ++i )
    {
    if( l.m_Offset[i] < r.m_Offset[i] )
      {
      return true;
      }
    else if( l.m_Offset[i] > r.m_Offset[i] )
      {
      return false;
      }
    }
  return false;
}

}     // End namepace Functor

} // End namespace itk
#endif // __itkOffset_hxx

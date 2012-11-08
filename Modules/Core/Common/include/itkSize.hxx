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
#ifndef __itkSize_hxx
#define __itkSize_hxx

#include "itkSize.h"

namespace itk
{
template <unsigned int VDimension>
const typename Size<VDimension>::Self
Size<VDimension>
::operator+(const typename Size<VDimension>::Self & vec) const
  {
  Self result;
  for( unsigned int i = 0; i < VDimension; ++i )
    {
    result[i] = m_Size[i] + vec.m_Size[i];
    }
  return result;
  }

template <unsigned int VDimension>
const typename Size<VDimension>::Self
& Size<VDimension>
::operator+=(const typename Size<VDimension>::Self & vec)
  {
  for( unsigned int i = 0; i < VDimension; ++i )
    {
    m_Size[i] += vec.m_Size[i];
    }
  return *this;
  }

template <unsigned int VDimension>
const typename Size<VDimension>::Self
Size<VDimension>
::operator-(const typename Size<VDimension>::Self & vec) const
  {
  Self result;
  for( unsigned int i = 0; i < VDimension; ++i )
    {
    result[i] = m_Size[i] - vec.m_Size[i];
    }
  return result;
  }

template <unsigned int VDimension>
const typename Size<VDimension>::Self
& Size<VDimension>
::operator-=(const typename Size<VDimension>::Self & vec)
  {
  for( unsigned int i = 0; i < VDimension; ++i )
    {
    m_Size[i] -= vec.m_Size[i];
    }
  return *this;
  }

template <unsigned int VDimension>
const typename Size<VDimension>::Self
Size<VDimension>
::operator*(const typename Size<VDimension>::Self & vec) const
  {
  Self result;
  for( unsigned int i = 0; i < VDimension; ++i )
    {
    result[i] = m_Size[i] * vec.m_Size[i];
    }
  return result;
  }

template <unsigned int VDimension>
const typename Size<VDimension>::Self
& Size<VDimension>
::operator*=(const typename Size<VDimension>::Self & vec)
  {
  for( unsigned int i = 0; i < VDimension; ++i )
    {
    m_Size[i] *= vec.m_Size[i];
    }
  return *this;
  }

template <unsigned int VDimension>
bool
Size<VDimension>
::operator==(const typename Size<VDimension>::Self & vec) const
  {
  bool same = 1;
  for( unsigned int i = 0; i < VDimension && same; ++i )
    {
    same = ( m_Size[i] == vec.m_Size[i] );
    }
  return same;
  }

template <unsigned int VDimension>
bool
Size<VDimension>
::operator!=(const typename Size<VDimension>::Self & vec) const
  {
  bool same = 1;
  for( unsigned int i = 0; i < VDimension && same; ++i )
    {
    same = ( m_Size[i] == vec.m_Size[i] );
    }
  return !same;
  }

template <unsigned int VDimension>
void
Size<VDimension>
::Fill(SizeValueType value)
{
  for( unsigned int i = 0; i < VDimension; ++i )
    {
    m_Size[i] = value;
    }
}

} // End namespace itk
#endif // __itkSize_hxx

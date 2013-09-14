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
#ifndef __itkScalarToRGBPixelFunctor_hxx
#define __itkScalarToRGBPixelFunctor_hxx

#include "itkScalarToRGBPixelFunctor.h"

namespace itk
{
namespace Functor
{
template< typename TScalar >
ScalarToRGBPixelFunctor< TScalar >
::ScalarToRGBPixelFunctor()
{
  m_ColorIndex[0] = 0;
  m_ColorIndex[1] = 0;
  m_ColorIndex[2] = 0;
  const unsigned int scalarSize = static_cast< unsigned int >( sizeof( ScalarType ) );
  for ( unsigned int i = 0; i < scalarSize && i < 3; ++i )
    {
    m_ColorIndex[i] = i;
    }

#ifdef ITK_WORDS_BIGENDIAN
  m_IsBigEndian = true;
#else
  m_IsBigEndian = false;
#endif
}

template< typename TScalar >
typename ScalarToRGBPixelFunctor< TScalar >::RGBPixelType
ScalarToRGBPixelFunctor< TScalar >
::operator()(const TScalar & v) const
{
  unsigned int i;
  int          j;

  TScalar        buf = v;
  unsigned char *bytes = (unsigned char *)( &buf );

  RGBPixelType ans;

  if ( this->m_IsBigEndian == true )
    {   // swap bytes
    TScalar tmp;
    for ( j = sizeof( TScalar ) - 1, i = 0; j >= 0; j--, i++ )
      {
      ( (unsigned char *)( &tmp ) )[i] = bytes[j];
      }
    buf = tmp;
    }

  ans[0] = static_cast< RGBComponentType >( bytes[m_ColorIndex[0]] * 3 );
  ans[1] = static_cast< RGBComponentType >( ( bytes[m_ColorIndex[0]] + bytes[m_ColorIndex[1]] ) * 5 );
  ans[2] = static_cast< RGBComponentType >( ( bytes[m_ColorIndex[0]] + bytes[m_ColorIndex[2]] ) );

  return ans;
}
} // end namespace Functor
} // end namespace itk

#endif

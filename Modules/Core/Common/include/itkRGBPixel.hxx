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
#ifndef itkRGBPixel_hxx
#define itkRGBPixel_hxx
#include "itkRGBPixel.h"
#include "itkNumericTraits.h"
#include "itkMath.h"
#include <algorithm>

namespace itk
{
template< typename T >
RGBPixel< T > &
RGBPixel< T >
::operator=(const ComponentType r[3])
{
  BaseArray::operator=(r);
  return *this;
}

template< typename T >
RGBPixel< T >
RGBPixel< T >
::operator+(const Self & r) const
{
  Self result;

  for ( unsigned int i = 0; i < 3; i++ )
    {
    result[i] = ( *this )[i] + r[i];
    }
  return result;
}

template< typename T >
RGBPixel< T >
RGBPixel< T >
::operator-(const Self & r) const
{
  Self result;

  for ( unsigned int i = 0; i < 3; i++ )
    {
    result[i] = ( *this )[i] - r[i];
    }
  return result;
}

template< typename T >
const RGBPixel< T > &
RGBPixel< T >
::operator+=(const Self & r)
{
  for ( unsigned int i = 0; i < 3; i++ )
    {
    ( *this )[i] += r[i];
    }
  return *this;
}

template< typename T >
const RGBPixel< T > &
RGBPixel< T >
::operator-=(const Self & r)
{
  for ( unsigned int i = 0; i < 3; i++ )
    {
    ( *this )[i] -= r[i];
    }
  return *this;
}

template< typename T >
const RGBPixel< T > &
RGBPixel< T >
::operator*=(const ComponentType & r)
{
  for ( unsigned int i = 0; i < 3; i++ )
    {
    ( *this )[i] *= r;
    }
  return *this;
}


template< typename T >
const RGBPixel< T > &
RGBPixel< T >
::operator/=(const ComponentType & r)
{
  for ( unsigned int i = 0; i < 3; i++ )
    {
    ( *this )[i] /= r;
    }
  return *this;
}

template< typename T >
RGBPixel< T >
RGBPixel< T >
::operator*(const ComponentType & r) const
{
  Self result;

  for ( unsigned int i = 0; i < 3; i++ )
    {
    result[i] = ( *this )[i] * r;
    }
  return result;
}

template< typename T >
RGBPixel< T >
RGBPixel< T >
::operator/(const ComponentType & r) const
{
  Self result;

  for ( unsigned int i = 0; i < 3; i++ )
    {
    result[i] = ( *this )[i] / r;
    }
  return result;
}

template< typename T >
bool
RGBPixel< T >
::operator==(const Self & r) const
{
  for ( unsigned int i = 0; i < 3; i++ )
    {
    if ( Math::NotExactlyEquals(( *this )[i], r[i]) )
      {
      return false;
      }
    }
  return true;
}

template< typename T >
bool
RGBPixel< T >
::operator<(const Self & r) const
{
  return std::lexicographical_compare( this->cbegin(), this->cend(), r.cbegin(), r.cend() );
}

template< typename T >
typename RGBPixel< T >::LuminanceType
RGBPixel< T >
::GetLuminance() const
{
  const LuminanceType luminance =
    0.30  * static_cast< LuminanceType >( this->GetRed() )
    + 0.59  * static_cast< LuminanceType >( this->GetGreen() )
    + 0.11  * static_cast< LuminanceType >( this->GetBlue() );

  return luminance;
}

template< typename TComponent >
std::ostream &
operator<<(std::ostream & os, const RGBPixel< TComponent > & c)
{
  os <<  static_cast< typename NumericTraits< TComponent >::PrintType >( c[0] ) << "  ";
  os <<  static_cast< typename NumericTraits< TComponent >::PrintType >( c[1] ) << "  ";
  os <<  static_cast< typename NumericTraits< TComponent >::PrintType >( c[2] );
  return os;
}

template< typename TComponent >
std::istream &
operator>>(std::istream & is, RGBPixel< TComponent > & c)
{
  TComponent red;
  TComponent green;
  TComponent blue;

  is >> red >> green >> blue;
  c.SetRed(red);
  c.SetGreen(green);
  c.SetBlue(blue);
  return is;
}
} // end namespace itk

#endif

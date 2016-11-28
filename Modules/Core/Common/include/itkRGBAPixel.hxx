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
#ifndef itkRGBAPixel_hxx
#define itkRGBAPixel_hxx
#include "itkRGBAPixel.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * Assignment Operator
 */
template< typename T >
RGBAPixel< T > &
RGBAPixel< T >
::operator=(const Self & r)
{
  BaseArray::operator=(r);
  return *this;
}

/**
 * Assigment from a plain array
 */
template< typename T >
RGBAPixel< T > &
RGBAPixel< T >
::operator=(const ComponentType r[4])
{
  BaseArray::operator=(r);
  return *this;
}

/**
 * Returns a temporary copy of a vector
 */
template< typename T >
RGBAPixel< T >
RGBAPixel< T >
::operator+(const Self & r) const
{
  Self result;

  for ( unsigned int i = 0; i < 4; i++ )
    {
    result[i] = ( *this )[i] + r[i];
    }
  return result;
}

/**
 * Returns a temporary copy of a vector
 */
template< typename T >
RGBAPixel< T >
RGBAPixel< T >
::operator-(const Self & r) const
{
  Self result;

  for ( unsigned int i = 0; i < 4; i++ )
    {
    result[i] = ( *this )[i] - r[i];
    }
  return result;
}

/**
 * Returns a temporary copy of a vector
 */
template< typename T >
const RGBAPixel< T > &
RGBAPixel< T >
::operator+=(const Self & r)
{
  for ( unsigned int i = 0; i < 4; i++ )
    {
    ( *this )[i] += r[i];
    }
  return *this;
}

/**
 * Returns a temporary copy of a vector
 */
template< typename T >
const RGBAPixel< T > &
RGBAPixel< T >
::operator-=(const Self & r)
{
  for ( unsigned int i = 0; i < 4; i++ )
    {
    ( *this )[i] -= r[i];
    }
  return *this;
}

/**
 * Returns a temporary copy of a vector
 */
template< typename T >
RGBAPixel< T >
RGBAPixel< T >
::operator*(const ComponentType & r) const
{
  Self result;

  for ( unsigned int i = 0; i < 4; i++ )
    {
    result[i] = ( *this )[i] * r;
    }
  return result;
}

/**
 * Returns the results from a test for equality (all components must be equal)
 */
template< typename T >
bool
RGBAPixel< T >
::operator==(const Self & r) const
{
  for ( unsigned int i = 0; i < 4; i++ )
    {
    if ( ( *this )[i] != r[i] )
      {
      return false;
      }
    }
  return true;
}

/**
 * Compute luminance
 */
template< typename T >
typename RGBAPixel< T >::LuminanceType
RGBAPixel< T >
::GetLuminance() const
{
  const LuminanceType luminance =
    0.30  * static_cast< LuminanceType >( this->GetRed() )
    + 0.59  * static_cast< LuminanceType >( this->GetGreen() )
    + 0.11  * static_cast< LuminanceType >( this->GetBlue() );

  return luminance;
}

/**
 * Print content to an ostream
 */
template< typename TComponent >
std::ostream &
operator<<(std::ostream & os, const RGBAPixel< TComponent > & c)
{
  os <<  static_cast< typename NumericTraits< TComponent >::PrintType >( c[0] )  << "  ";
  os <<  static_cast< typename NumericTraits< TComponent >::PrintType >( c[1] )  << "  ";
  os <<  static_cast< typename NumericTraits< TComponent >::PrintType >( c[2] )  << "  ";
  os <<  static_cast< typename NumericTraits< TComponent >::PrintType >( c[3] );
  return os;
}

/**
 * Read content from an istream
 */
template< typename TComponent >
std::istream &
operator>>(std::istream & is, RGBAPixel< TComponent > & c)
{
  TComponent red;
  TComponent green;
  TComponent blue;
  TComponent alpha;

  is >> red >> green >> blue;
  c.SetRed(red);
  c.SetGreen(green);
  c.SetBlue(blue);
  c.SetAlpha(alpha);
  return is;
}
} // end namespace itk

#endif

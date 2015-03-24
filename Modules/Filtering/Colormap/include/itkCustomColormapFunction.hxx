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
#ifndef itkCustomColormapFunction_hxx
#define itkCustomColormapFunction_hxx

#include "itkCustomColormapFunction.h"
#include "itkMath.h"

namespace itk
{
namespace Function
{
template< typename TScalar, typename TRGBPixel >
typename CustomColormapFunction< TScalar, TRGBPixel >::RGBPixelType
CustomColormapFunction< TScalar, TRGBPixel >
::operator()(const TScalar & v) const
{
  // Map the input scalar between [0, 1].
  RealType value = this->RescaleInputValue(v);

  // Apply the color mapping.
  RealType red = 0.0;

  RealType     size = static_cast< RealType >( this->m_RedChannel.size() );
  unsigned int index = Math::Ceil< unsigned int >( value * ( size - 1.0 ) );

  if ( size == 1 || index < 1 )
    {
    red = this->m_RedChannel[0];
    }
  else if ( size > 1 )
    {
    RealType     p1 = this->m_RedChannel[index];
    RealType     m1 = this->m_RedChannel[index - 1u];
    RealType     d = p1 - m1;
    red = d * ( size - 1.0 ) * ( value - ( index - 1.0 ) / ( size - 1.0 ) )
          + m1;
    }

  RealType green = 0.0;

  size = static_cast< RealType >( this->m_GreenChannel.size() );
  index = Math::Ceil< unsigned int >( value * ( size - 1.0 ) );

  if ( size == 1 || index < 1 )
    {
    green = this->m_GreenChannel[0];
    }
  else if ( size > 1 )
    {
    RealType     p1 = this->m_GreenChannel[index];
    RealType     m1 = this->m_GreenChannel[index - 1u];
    RealType     d = p1 - m1;
    green = d * ( size - 1.0 ) * ( value - ( index - 1.0 ) / ( size - 1.0 ) )
            + m1;
    }

  RealType blue = 0.0;

  size = static_cast< RealType >( this->m_BlueChannel.size() );
  index = Math::Ceil< unsigned int >( value * ( size - 1.0 ) );

  if ( size == 1 || index < 1 )
    {
    blue = this->m_BlueChannel[0];
    }
  else if ( size > 1 )
    {
    RealType     p1 = this->m_BlueChannel[index];
    RealType     m1 = this->m_BlueChannel[index - 1u];
    RealType     d = p1 - m1;
    blue = d * ( size - 1.0 ) * ( value - ( index - 1.0 ) / ( size - 1.0 ) )
           + m1;
    }

  // Set the RGB components after rescaling the values.
  RGBPixelType pixel;
  NumericTraits<TRGBPixel>::SetLength(pixel, 3);

  pixel[0] = this->RescaleRGBComponentValue(red);
  pixel[1] = this->RescaleRGBComponentValue(green);
  pixel[2] = this->RescaleRGBComponentValue(blue);

  return pixel;
}
} // end namespace Function
} // end namespace itk

#endif

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

  // Setup some arrays and apply color mapping
  enum ColorNames { RED=0, GREEN=1, BLUE=2 };
  RealType RGBValue[3] = { 0.0 };
  const ChannelType * const ColorChannel[3] = { &m_RedChannel, &m_GreenChannel, &m_BlueChannel };

  for(size_t color = RED; color <= BLUE; ++color) //Go through all the colors
    {
    size_t size = ColorChannel[color]->size();
    size_t index = Math::Ceil< size_t, RealType >( value * static_cast< RealType >( size - 1 ) );

    if ( size == 1 || index < 1 )
      {
      RGBValue[color] = (*ColorChannel[color])[0];
      }
    else if ( size > 1 )
      {
      RealType     p1 = (*ColorChannel[color])[index];
      RealType     m1 = (*ColorChannel[color])[index - 1u];
      RealType     d = p1 - m1;
      RGBValue[color] = d * ( size - 1 ) * ( value - ( index - 1 ) / static_cast< RealType >( size - 1 ) ) + m1;
      }
    }

  // Set the RGB components after rescaling the values.
  RGBPixelType pixel;
  NumericTraits<TRGBPixel>::SetLength(pixel, 3);

  for( size_t color = static_cast< size_t >( RED ); color <= static_cast< size_t >( BLUE ); ++color )
    {
    pixel[color] = this->RescaleRGBComponentValue( RGBValue[color] );
    }

  return pixel;
}
} // end namespace Function
} // end namespace itk

#endif

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
#ifndef itkHotColormapFunction_hxx
#define itkHotColormapFunction_hxx

#include "itkHotColormapFunction.h"

namespace itk
{
namespace Function
{
template< typename TScalar, typename TRGBPixel >
typename HotColormapFunction< TScalar, TRGBPixel >::RGBPixelType
HotColormapFunction< TScalar, TRGBPixel >
::operator()(const TScalar & v) const
{
  // Map the input scalar between [0, 1].
  RealType value = this->RescaleInputValue(v);

  // Apply the color mapping.
  RealType red   = 63.0 / 26.0 * value - 1.0 / 13.0;

  red = std::max(0.0, red);
  red = std::min(1.0, red);

  RealType green = 63.0 / 26.0 * value - 11.0 / 13.0;
  green = std::max(0.0, green);
  green = std::min(1.0, green);

  RealType blue  = 4.5 * value - 3.5;
  blue = std::max(0.0, blue);
  blue = std::min(1.0, blue);

  // Set the rgb components after rescaling the values.
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

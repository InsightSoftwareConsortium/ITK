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
#include "itkScalarToRGBColormapImageFilter.h"

namespace itk
{
/** Define how to print enumerations */
std::ostream &
operator<<( std::ostream & out, const RGBColormapFilterEnumType value )
{
  const char * s = nullptr;
  switch ( value )
  {
    case RGBColormapFilterEnumType::Red:
      s = "RGBColormapFilterEnumType::Red";
      break;
    case RGBColormapFilterEnumType::Green:
      s = "RGBColormapFilterEnumType::Green";
      break;
    case RGBColormapFilterEnumType::Blue:
      s = "RGBColormapFilterEnumType::Blue";
      break;
    case RGBColormapFilterEnumType::Grey:
      s = "RGBColormapFilterEnumType::Grey";
      break;
    case RGBColormapFilterEnumType::Hot:
      s = "RGBColormapFilterEnumType::Hot";
      break;
    case RGBColormapFilterEnumType::Cool:
      s = "RGBColormapFilterEnumType::Cool";
      break;
    case RGBColormapFilterEnumType::Spring:
      s = "RGBColormapFilterEnumType";
      break;
    case RGBColormapFilterEnumType::Summer:
      s = "RGBColormapFilterEnumType::Summer";
      break;
    case RGBColormapFilterEnumType::Autumn:
      s = "RGBColormapFilterEnumType::Autumn";
      break;
    case RGBColormapFilterEnumType::Winter:
      s = "RGBColormapFilterEnumType::Winter";
      break;
    case RGBColormapFilterEnumType::Copper:
      s = "RGBColormapFilterEnumType::Copper";
      break;
    case RGBColormapFilterEnumType::Jet:
      s = "RGBColormapFilterEnumType::Jet";
      break;
    case RGBColormapFilterEnumType::HSV:
      s = "RGBColormapFilterEnumType::HSV";
      break;
    case RGBColormapFilterEnumType::OverUnder:
      s = "RGBColormapFilterEnumType::OverUnder";
      break;
    default:
      s = "INVALID VALUE FOR RGBColormapFilterEnumType";
  }
  return out << s;
}
} // namespace itk

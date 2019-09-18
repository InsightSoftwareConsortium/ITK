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
operator<<(std::ostream & out, const RGBColormapFilterEnumType value)
{
  return out << [value] {
    switch (value)
    {
      case RGBColormapFilterEnumType::Red:
        return "RGBColormapFilterEnumType::Red";
      case RGBColormapFilterEnumType::Green:
        return "RGBColormapFilterEnumType::Green";
      case RGBColormapFilterEnumType::Blue:
        return "RGBColormapFilterEnumType::Blue";
      case RGBColormapFilterEnumType::Grey:
        return "RGBColormapFilterEnumType::Grey";
      case RGBColormapFilterEnumType::Hot:
        return "RGBColormapFilterEnumType::Hot";
      case RGBColormapFilterEnumType::Cool:
        return "RGBColormapFilterEnumType::Cool";
      case RGBColormapFilterEnumType::Spring:
        return "RGBColormapFilterEnumType";
      case RGBColormapFilterEnumType::Summer:
        return "RGBColormapFilterEnumType::Summer";
      case RGBColormapFilterEnumType::Autumn:
        return "RGBColormapFilterEnumType::Autumn";
      case RGBColormapFilterEnumType::Winter:
        return "RGBColormapFilterEnumType::Winter";
      case RGBColormapFilterEnumType::Copper:
        return "RGBColormapFilterEnumType::Copper";
      case RGBColormapFilterEnumType::Jet:
        return "RGBColormapFilterEnumType::Jet";
      case RGBColormapFilterEnumType::HSV:
        return "RGBColormapFilterEnumType::HSV";
      case RGBColormapFilterEnumType::OverUnder:
        return "RGBColormapFilterEnumType::OverUnder";
      default:
        return "INVALID VALUE FOR RGBColormapFilterEnumType";
    }
  }();
}
} // namespace itk

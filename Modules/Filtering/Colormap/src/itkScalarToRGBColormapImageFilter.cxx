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
operator<<(std::ostream & out, const RGBColormapFilterEnum value)
{
  return out << [value] {
    switch (value)
    {
      case RGBColormapFilterEnum::Red:
        return "RGBColormapFilterEnum::Red";
      case RGBColormapFilterEnum::Green:
        return "RGBColormapFilterEnum::Green";
      case RGBColormapFilterEnum::Blue:
        return "RGBColormapFilterEnum::Blue";
      case RGBColormapFilterEnum::Grey:
        return "RGBColormapFilterEnum::Grey";
      case RGBColormapFilterEnum::Hot:
        return "RGBColormapFilterEnum::Hot";
      case RGBColormapFilterEnum::Cool:
        return "RGBColormapFilterEnum::Cool";
      case RGBColormapFilterEnum::Spring:
        return "RGBColormapFilterEnum";
      case RGBColormapFilterEnum::Summer:
        return "RGBColormapFilterEnum::Summer";
      case RGBColormapFilterEnum::Autumn:
        return "RGBColormapFilterEnum::Autumn";
      case RGBColormapFilterEnum::Winter:
        return "RGBColormapFilterEnum::Winter";
      case RGBColormapFilterEnum::Copper:
        return "RGBColormapFilterEnum::Copper";
      case RGBColormapFilterEnum::Jet:
        return "RGBColormapFilterEnum::Jet";
      case RGBColormapFilterEnum::HSV:
        return "RGBColormapFilterEnum::HSV";
      case RGBColormapFilterEnum::OverUnder:
        return "RGBColormapFilterEnum::OverUnder";
      default:
        return "INVALID VALUE FOR RGBColormapFilterEnum";
    }
  }();
}
} // namespace itk

/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
operator<<(std::ostream & out, const ScalarToRGBColormapImageFilterEnums::RGBColormapFilter value)
{
  return out << [value] {
    switch (value)
    {
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Red:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Red";
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Green:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Green";
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Blue:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Blue";
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Grey:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Grey";
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Hot:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Hot";
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Cool:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Cool";
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Spring:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter";
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Summer:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Summer";
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Autumn:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Autumn";
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Winter:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Winter";
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Copper:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Copper";
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Jet:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::Jet";
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::HSV:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::HSV";
      case ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::OverUnder:
        return "ScalarToRGBColormapImageFilterEnums::RGBColormapFilter::OverUnder";
      default:
        return "INVALID VALUE FOR ScalarToRGBColormapImageFilterEnums::RGBColormapFilter";
    }
  }();
}
} // namespace itk

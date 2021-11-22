/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkContourSpatialObject.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const ContourSpatialObjectEnums::InterpolationMethod value)
{
  return out << [value] {
    switch (value)
    {
      case ContourSpatialObjectEnums::InterpolationMethod::NO_INTERPOLATION:
        return "itk::ContourSpatialObjectEnums::InterpolationMethod::NO_INTERPOLATION";
      case ContourSpatialObjectEnums::InterpolationMethod::EXPLICIT_INTERPOLATION:
        return "itk::ContourSpatialObjectEnums::InterpolationMethod::EXPLICIT_INTERPOLATION";
      case ContourSpatialObjectEnums::InterpolationMethod::BEZIER_INTERPOLATION:
        return "itk::ContourSpatialObjectEnums::InterpolationMethod::BEZIER_INTERPOLATION";
      case ContourSpatialObjectEnums::InterpolationMethod::LINEAR_INTERPOLATION:
        return "itk::ContourSpatialObjectEnums::InterpolationMethod::LINEAR_INTERPOLATION";
      default:
        return "INVALID VALUE FOR itk::ContourSpatialObjectEnums::InterpolationMethod";
    }
  }();
}
} // namespace itk

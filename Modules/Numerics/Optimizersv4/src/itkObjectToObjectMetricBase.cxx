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
#include "itkObjectToObjectMetricBase.h"

namespace itk
{
/** Define how to print enumerations */
std::ostream &
operator<<(std::ostream & out, const GradientSourceEnum value)
{
  return out << [value] {
    switch (value)
    {
      case GradientSourceEnum::GRADIENT_SOURCE_FIXED:
        return "GradientSourceEnum::GRADIENT_SOURCE_FIXED";
      case GradientSourceEnum::GRADIENT_SOURCE_MOVING:
        return "GradientSourceEnum::GRADIENT_SOURCE_MOVING";
      case GradientSourceEnum::GRADIENT_SOURCE_BOTH:
        return "GradientSourceEnum::GRADIENT_SOURCE_BOTH";
      default:
        return "INVALID VALUE FOR GradientSourceEnum";
    }
  }();
}

/** Define how to print enumerations */
std::ostream &
operator<<(std::ostream & out, const MetricCategoryEnum value)
{
  return out << [value] {
    switch (value)
    {
      case MetricCategoryEnum::UNKNOWN_METRIC:
        return "MetricCategoryEnum::UNKNOWN_METRIC";
      case MetricCategoryEnum::OBJECT_METRIC:
        return "MetricCategoryEnum::OBJECT_METRIC";
      case MetricCategoryEnum::IMAGE_METRIC:
        return "MetricCategoryEnum::IMAGE_METRIC";
      case MetricCategoryEnum::POINT_SET_METRIC:
        return "MetricCategoryEnum::POINT_SET_METRIC";
      case MetricCategoryEnum::MULTI_METRIC:
        return "MetricCategoryEnum::MULTI_METRIC";
      default:
        return "INVALID VALUE FOR MetricCategoryEnum";
    }
  }();
}
} // namespace itk

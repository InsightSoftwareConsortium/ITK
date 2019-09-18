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
#include "itkObjectToObjectMetricBase.h"

namespace itk
{
/** Define how to print enumerations */
std::ostream &
operator<<(std::ostream & out, const SourceTypeOfGradient value)
{
  return out << [value] {
    switch (value)
    {
      case SourceTypeOfGradient::GRADIENT_SOURCE_FIXED:
        return "SourceTypeOfGradient::GRADIENT_SOURCE_FIXED";
      case SourceTypeOfGradient::GRADIENT_SOURCE_MOVING:
        return "SourceTypeOfGradient::GRADIENT_SOURCE_MOVING";
      case SourceTypeOfGradient::GRADIENT_SOURCE_BOTH:
        return "SourceTypeOfGradient::GRADIENT_SOURCE_BOTH";
      default:
        return "INVALID VALUE FOR SourceTypeOfGradient";
    }
  }();
}

/** Define how to print enumerations */
std::ostream &
operator<<(std::ostream & out, const CategoryTypeForMetric value)
{
  return out << [value] {
    switch (value)
    {
      case CategoryTypeForMetric::UNKNOWN_METRIC:
        return "CategoryTypeForMetric::UNKNOWN_METRIC";
      case CategoryTypeForMetric::OBJECT_METRIC:
        return "CategoryTypeForMetric::OBJECT_METRIC";
      case CategoryTypeForMetric::IMAGE_METRIC:
        return "CategoryTypeForMetric::IMAGE_METRIC";
      case CategoryTypeForMetric::POINT_SET_METRIC:
        return "CategoryTypeForMetric::POINT_SET_METRIC";
      case CategoryTypeForMetric::MULTI_METRIC:
        return "CategoryTypeForMetric::MULTI_METRIC";
      default:
        return "INVALID VALUE FOR CategoryTypeForMetric";
    }
  }();
}
} // namespace itk

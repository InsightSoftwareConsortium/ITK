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
  const char * s = nullptr;
  switch (value)
  {
    case SourceTypeOfGradient::GRADIENT_SOURCE_FIXED:
      s = "SourceTypeOfGradient::GRADIENT_SOURCE_FIXED";
      break;
    case SourceTypeOfGradient::GRADIENT_SOURCE_MOVING:
      s = "SourceTypeOfGradient::GRADIENT_SOURCE_MOVING";
      break;
    case SourceTypeOfGradient::GRADIENT_SOURCE_BOTH:
      s = "SourceTypeOfGradient::GRADIENT_SOURCE_BOTH";
      break;
    default:
      s = "INVALID VALUE FOR SourceTypeOfGradient";
  }
  return out << s;
}

/** Define how to print enumerations */
std::ostream &
operator<<(std::ostream & out, const CategoryTypeForMetric value)
{
  const char * s = nullptr;
  switch (value)
  {
    case CategoryTypeForMetric::UNKNOWN_METRIC:
      s = "CategoryTypeForMetric::UNKNOWN_METRIC";
      break;
    case CategoryTypeForMetric::OBJECT_METRIC:
      s = "CategoryTypeForMetric::OBJECT_METRIC";
      break;
    case CategoryTypeForMetric::IMAGE_METRIC:
      s = "CategoryTypeForMetric::IMAGE_METRIC";
      break;
    case CategoryTypeForMetric::POINT_SET_METRIC:
      s = "CategoryTypeForMetric::POINT_SET_METRIC";
      break;
    case CategoryTypeForMetric::MULTI_METRIC:
      s = "CategoryTypeForMetric::MULTI_METRIC";
      break;
    default:
      s = "INVALID VALUE FOR CategoryTypeForMetric";
  }
  return out << s;
}
} // namespace itk

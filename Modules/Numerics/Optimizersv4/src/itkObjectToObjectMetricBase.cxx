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
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const ObjectToObjectMetricBaseTemplateEnums::GradientSource value)
{
  return out << [value] {
    switch (value)
    {
      case ObjectToObjectMetricBaseTemplateEnums::GradientSource::GRADIENT_SOURCE_FIXED:
        return "itk::ObjectToObjectMetricBaseTemplateEnums::GradientSource::GRADIENT_SOURCE_FIXED";
      case ObjectToObjectMetricBaseTemplateEnums::GradientSource::GRADIENT_SOURCE_MOVING:
        return "itk::ObjectToObjectMetricBaseTemplateEnums::GradientSource::GRADIENT_SOURCE_MOVING";
      case ObjectToObjectMetricBaseTemplateEnums::GradientSource::GRADIENT_SOURCE_BOTH:
        return "itk::ObjectToObjectMetricBaseTemplateEnums::GradientSource::GRADIENT_SOURCE_BOTH";
      default:
        return "INVALID VALUE FOR itk::ObjectToObjectMetricBaseTemplateEnums::GradientSource";
    }
  }();
}

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const ObjectToObjectMetricBaseTemplateEnums::MetricCategory value)
{
  return out << [value] {
    switch (value)
    {
      case ObjectToObjectMetricBaseTemplateEnums::MetricCategory::UNKNOWN_METRIC:
        return "itk::ObjectToObjectMetricBaseTemplateEnums::MetricCategory::UNKNOWN_METRIC";
      case ObjectToObjectMetricBaseTemplateEnums::MetricCategory::OBJECT_METRIC:
        return "itk::ObjectToObjectMetricBaseTemplateEnums::MetricCategory::OBJECT_METRIC";
      case ObjectToObjectMetricBaseTemplateEnums::MetricCategory::IMAGE_METRIC:
        return "itk::ObjectToObjectMetricBaseTemplateEnums::MetricCategory::IMAGE_METRIC";
      case ObjectToObjectMetricBaseTemplateEnums::MetricCategory::POINT_SET_METRIC:
        return "itk::ObjectToObjectMetricBaseTemplateEnums::MetricCategory::POINT_SET_METRIC";
      case ObjectToObjectMetricBaseTemplateEnums::MetricCategory::MULTI_METRIC:
        return "itk::ObjectToObjectMetricBaseTemplateEnums::MetricCategory::MULTI_METRIC";
      default:
        return "INVALID VALUE FOR itk::ObjectToObjectMetricBaseTemplateEnums::MetricCategory";
    }
  }();
}
} // namespace itk

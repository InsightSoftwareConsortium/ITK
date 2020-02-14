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
#include "../include/itkImageRegistrationMethodv4.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const ImageRegistrationMethodv4Enums::MetricSamplingStrategy value)
{
  return out << [value] {
    switch (value)
    {
      case ImageRegistrationMethodv4Enums::MetricSamplingStrategy::NONE:
        return "itk::ImageRegistrationMethodv4Enums::MetricSamplingStrategy::NONE";
      case ImageRegistrationMethodv4Enums::MetricSamplingStrategy::REGULAR:
        return "itk::ImageRegistrationMethodv4Enums::MetricSamplingStrategy::REGULAR";
      case ImageRegistrationMethodv4Enums::MetricSamplingStrategy::RANDOM:
        return "itk::ImageRegistrationMethodv4Enums::MetricSamplingStrategy::RANDOM";
      default:
        return "INVALID VALUE FOR itk::ImageRegistrationMethodv4Enums::MetricSamplingStrategy";
    }
  }();
}
} // end namespace itk

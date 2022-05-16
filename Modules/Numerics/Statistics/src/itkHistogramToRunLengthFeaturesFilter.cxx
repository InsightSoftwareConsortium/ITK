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
#include "itkHistogramToRunLengthFeaturesFilter.h"

namespace itk
{
namespace Statistics
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature value)
{
  return out << [value] {
    switch (value)
    {
      case HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::ShortRunEmphasis:
        return "itk::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::ShortRunEmphasis";
      case HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::LongRunEmphasis:
        return "itk::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::LongRunEmphasis";
      case HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::GreyLevelNonuniformity:
        return "itk::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::GreyLevelNonuniformity";
      case HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::RunLengthNonuniformity:
        return "itk::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::RunLengthNonuniformity";
      case HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::LowGreyLevelRunEmphasis:
        return "itk::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::LowGreyLevelRunEmphasis";
      case HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::HighGreyLevelRunEmphasis:
        return "itk::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::HighGreyLevelRunEmphasis";
      case HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::ShortRunLowGreyLevelEmphasis:
        return "itk::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::ShortRunLowGreyLevelEmphasis";
      case HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::ShortRunHighGreyLevelEmphasis:
        return "itk::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::ShortRunHighGreyLevelEmphasis";
      case HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::LongRunLowGreyLevelEmphasis:
        return "itk::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::LongRunLowGreyLevelEmphasis";
      case HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::LongRunHighGreyLevelEmphasis:
        return "itk::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::LongRunHighGreyLevelEmphasis";
      default:
        return "INVALID VALUE FOR itk::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature";
    }
  }();
}
} // end of namespace Statistics
} // end of namespace itk

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
#include "itkHistogramToTextureFeaturesFilter.h"

namespace itk
{
namespace Statistics
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const HistogramToTextureFeaturesFilterEnums::TextureFeature value)
{
  return out << [value] {
    switch (value)
    {
      case HistogramToTextureFeaturesFilterEnums::TextureFeature::Energy:
        return "itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::Energy";
      case HistogramToTextureFeaturesFilterEnums::TextureFeature::Entropy:
        return "itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::Entropy";
      case HistogramToTextureFeaturesFilterEnums::TextureFeature::Correlation:
        return "itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::Correlation";
      case HistogramToTextureFeaturesFilterEnums::TextureFeature::InverseDifferenceMoment:
        return "itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::InverseDifferenceMoment";
      case HistogramToTextureFeaturesFilterEnums::TextureFeature::Inertia:
        return "itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::Inertia";
      case HistogramToTextureFeaturesFilterEnums::TextureFeature::ClusterShade:
        return "itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::ClusterShade";
      case HistogramToTextureFeaturesFilterEnums::TextureFeature::ClusterProminence:
        return "itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::ClusterProminence";
      case HistogramToTextureFeaturesFilterEnums::TextureFeature::HaralickCorrelation:
        return "itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::HaralickCorrelation";
      case HistogramToTextureFeaturesFilterEnums::TextureFeature::InvalidFeatureName:
        return "itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature::InvalidFeatureName";
      default:
        return "INVALID VALUE FOR itk::Statistics::HistogramToTextureFeaturesFilterEnums::TextureFeature";
    }
  }();
}
} // end of namespace Statistics
} // end of namespace itk

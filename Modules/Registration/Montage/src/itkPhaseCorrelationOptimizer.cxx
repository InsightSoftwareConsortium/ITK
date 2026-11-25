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
#include "itkPhaseCorrelationOptimizer.h"

namespace itk
{
/** Define how to print enumerations */
std::ostream &
operator<<(std::ostream & out, const PhaseCorrelationOptimizerEnums::PeakInterpolationMethod value)
{
  return out << [value] {
    switch (value)
    {
      case PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::None:
        return "PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::None";
      case PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::Parabolic:
        return "PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::Parabolic";
      case PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::Cosine:
        return "PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::Cosine";
      case PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::WeightedMeanPhase:
        return "PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::WeightedMeanPhase";
      // case PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::PhaseFrequencySlope:
      // return "PhaseCorrelationOptimizerEnums::PeakInterpolationMethod::PhaseFrequencySlope";
      default:
        return "INVALID VALUE FOR PeakInterpolationMethod";
    }
  }();
}
} // end namespace itk

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
#include "itkFastMarchingUpwindGradientImageFilter.h"

namespace itk
{

std::ostream &
operator<<(std::ostream & out, const FastMarchingUpwindGradientImageFilterEnums::TargetCondition value)
{
  return out << [value] {
    switch (value)
    {
      case FastMarchingUpwindGradientImageFilterEnums::TargetCondition::NoTargets:
        return "itk::FastMarchingUpwindGradientImageFilterEnums::TargetCondition::NoTargets";
      case FastMarchingUpwindGradientImageFilterEnums::TargetCondition::OneTarget:
        return "itk::FastMarchingUpwindGradientImageFilterEnums::TargetCondition::OneTarget";
      case FastMarchingUpwindGradientImageFilterEnums::TargetCondition::SomeTargets:
        return "itk::FastMarchingUpwindGradientImageFilterEnums::TargetCondition::SomeTargets";
      case FastMarchingUpwindGradientImageFilterEnums::TargetCondition::AllTargets:
        return "itk::FastMarchingUpwindGradientImageFilterEnums::TargetCondition::AllTargets";
      default:
        return "INVALID VALUE FOR itk::FastMarchingUpwindGradientImageFilterEnums::TargetCondition";
    }
  }();
}
} // end namespace itk

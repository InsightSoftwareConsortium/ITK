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
#include "../include/itkFastMarchingReachedTargetNodesStoppingCriterion.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition value)
{
  return out << [value] {
    switch (value)
    {
      case FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition::OneTarget:
        return "itk::FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition::OneTarget";
      case FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition::SomeTargets:
        return "itk::FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition::SomeTargets";
      case FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition::AllTargets:
        return "itk::FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition::AllTargets";
      default:
        return "INVALID VALUE FOR itk::FastMarchingReachedTargetNodesStoppingCriterionEnums::TargetCondition";
    }
  }();
}
} // end namespace itk

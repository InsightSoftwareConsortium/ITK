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
#include "itkMergeLabelMapFilter.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const MergeLabelMapFilterEnums::ChoiceMethod value)
{
  return out << [value] {
    switch (value)
    {
      case MergeLabelMapFilterEnums::ChoiceMethod::KEEP:
        return "itk::MergeLabelMapFilterEnums::ChoiceMethod::KEEP";
      case MergeLabelMapFilterEnums::ChoiceMethod::AGGREGATE:
        return "itk::MergeLabelMapFilterEnums::ChoiceMethod::AGGREGATE";
      case MergeLabelMapFilterEnums::ChoiceMethod::PACK:
        return "itk::MergeLabelMapFilterEnums::ChoiceMethod::PACK";
      case MergeLabelMapFilterEnums::ChoiceMethod::STRICT:
        return "itk::MergeLabelMapFilterEnums::ChoiceMethod::STRICT";
      default:
        return "INVALID VALUE FOR itk::MergeLabelMapFilterEnums::ChoiceMethod";
    }
  }();
}
} // namespace itk

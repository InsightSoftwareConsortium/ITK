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
#include "itkExtractImageFilter.h"

namespace itk
{
/** Define how to print enumeration */
std::ostream &
operator<<(std::ostream & out, const ExtractImageFilterCollapseStrategyEnum value)
{
  return out << [value] {
    switch (value)
    {
      case ExtractImageFilterCollapseStrategyEnum::DIRECTIONCOLLAPSETOUNKOWN:
        return "ExtractImageFilterCollapseStrategyEnum::DIRECTIONCOLLAPSETOUNKOWN";
      case ExtractImageFilterCollapseStrategyEnum::DIRECTIONCOLLAPSETOIDENTITY:
        return "ExtractImageFilterCollapseStrategyEnum::DIRECTIONCOLLAPSETOIDENTITY";
      case ExtractImageFilterCollapseStrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX:
        return "ExtractImageFilterCollapseStrategyEnum::DIRECTIONCOLLAPSETOSUBMATRIX";
      case ExtractImageFilterCollapseStrategyEnum::DIRECTIONCOLLAPSETOGUESS:
        return "ExtractImageFilterCollapseStrategyEnum::DIRECTIONCOLLAPSETOGUESS";
      default:
        return "INVALID VALUE FOR ExtractImageFilterCollapseStrategyEnum";
    }
  }();
}
} // end namespace itk

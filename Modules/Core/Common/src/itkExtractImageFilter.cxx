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
operator<<(std::ostream & out, const ExtractImageFilterEnums::DirectionCollapseStrategy value)
{
  return out << [value] {
    switch (value)
    {
      case ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOUNKOWN:
        return "ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOUNKOWN";
      case ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOIDENTITY:
        return "ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOIDENTITY";
      case ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOSUBMATRIX:
        return "ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOSUBMATRIX";
      case ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOGUESS:
        return "ExtractImageFilterEnums::DirectionCollapseStrategy::DIRECTIONCOLLAPSETOGUESS";
      default:
        return "INVALID VALUE FOR ExtractImageFilterEnums::DirectionCollapseStrategy";
    }
  }();
}
} // end namespace itk

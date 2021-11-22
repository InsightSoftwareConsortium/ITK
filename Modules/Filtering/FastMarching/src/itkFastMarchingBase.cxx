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
#include "itkFastMarchingBase.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const FastMarchingTraitsEnums::TopologyCheck value)
{
  return out << [value] {
    switch (value)
    {
      case FastMarchingTraitsEnums::TopologyCheck::Nothing:
        return "itk::FastMarchingTraitsEnums::TopologyCheck::Nothing";
      case FastMarchingTraitsEnums::TopologyCheck::NoHandles:
        return "itk::FastMarchingTraitsEnums::TopologyCheck::NoHandles";
      case FastMarchingTraitsEnums::TopologyCheck::Strict:
        return "itk::FastMarchingTraitsEnums::TopologyCheck::Strict";
      default:
        return "INVALID VALUE FOR itk::FastMarchingTraitsEnums::TopologyCheck";
    }
  }();
}
} // end namespace itk

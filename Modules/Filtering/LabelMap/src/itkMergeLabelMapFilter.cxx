/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include "itkMergeLabelMapFilter.h"

namespace itk
{
/**Define how to print enumerations */
std::ostream &
operator<<(std::ostream & out, const ChoiceMethod value)
{
  const char * s = 0;
  switch (value)
  {
    case ChoiceMethod::KEEP:
      s = "ChoiceMethod::KEEP";
      break;
    case ChoiceMethod::AGGREGATE:
      s = "ChoiceMethod::AGGREGATE";
      break;
    case ChoiceMethod::PACK:
      s = "ChoiceMethod::PACK";
      break;
    case ChoiceMethod::STRICT:
      s = "ChoiceMethod::STRICT";
      break;
    default:
      s = "INVALID VALUE FOR ChoiceMethod";
  }
  return out << s;
}
} // namespace itk

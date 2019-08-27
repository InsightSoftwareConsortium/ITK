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
#include "itkMultiStartOptimizerv4.h"

namespace itk
{
/**Define how to print enumerations. */
std::ostream &
operator<<(std::ostream & out, const StopType value)
{
  const char * s = nullptr;
  switch (value)
  {
    case StopType::MAXIMUM_NUMBER_OF_ITERATIONS:
      s = "StopType::MAXIMUM_NUMBER_OF_ITERATIONS";
      break;
    case StopType::COSTFUNCTION_ERROR:
      s = "StopType::COSTFUNCTION_ERROR";
      break;
    case StopType::UPDATE_PARAMETERS_ERROR:
      s = "StopType::UPDATE_PARAMETERS_ERROR";
      break;
    case StopType::STEP_TOO_SMALL:
      s = "StopType::STEP_TOO_SMALL";
      break;
    case StopType::CONVERGENCE_CHECKER_PASSED:
      s = "StopType::CONVERGENCE_CHECKER_PASSED";
      break;
    case StopType::OTHER_ERROR:
      s = "StopType::OTHER_ERROR";
      break;
    default:
      s = "INVALID VALUE FOR StopType";
  }
  return out << s;
}
} // namespace itk

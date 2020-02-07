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
#include "itkMultiStartOptimizerv4.h"

namespace itk
{
/**Define how to print enumerations. */
std::ostream &
operator<<(std::ostream & out, const StopEnum value)
{
  return out << [value] {
    switch (value)
    {
      case StopEnum::MAXIMUM_NUMBER_OF_ITERATIONS:
        return "StopEnum::MAXIMUM_NUMBER_OF_ITERATIONS";
      case StopEnum::COSTFUNCTION_ERROR:
        return "StopEnum::COSTFUNCTION_ERROR";
      case StopEnum::UPDATE_PARAMETERS_ERROR:
        return "StopEnum::UPDATE_PARAMETERS_ERROR";
      case StopEnum::STEP_TOO_SMALL:
        return "StopEnum::STEP_TOO_SMALL";
      case StopEnum::CONVERGENCE_CHECKER_PASSED:
        return "StopEnum::CONVERGENCE_CHECKER_PASSED";
      case StopEnum::OTHER_ERROR:
        return "StopEnum::OTHER_ERROR";
      default:
        return "INVALID VALUE FOR StopEnum";
    }
  }();
}
} // namespace itk

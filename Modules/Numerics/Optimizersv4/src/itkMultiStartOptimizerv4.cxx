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
  return out << [value] {
    switch (value)
    {
      case StopType::MAXIMUM_NUMBER_OF_ITERATIONS:
        return "StopType::MAXIMUM_NUMBER_OF_ITERATIONS";
      case StopType::COSTFUNCTION_ERROR:
        return "StopType::COSTFUNCTION_ERROR";
      case StopType::UPDATE_PARAMETERS_ERROR:
        return "StopType::UPDATE_PARAMETERS_ERROR";
      case StopType::STEP_TOO_SMALL:
        return "StopType::STEP_TOO_SMALL";
      case StopType::CONVERGENCE_CHECKER_PASSED:
        return "StopType::CONVERGENCE_CHECKER_PASSED";
      case StopType::OTHER_ERROR:
        return "StopType::OTHER_ERROR";
      default:
        return "INVALID VALUE FOR StopType";
    }
  }();
}
} // namespace itk

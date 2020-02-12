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
#include "../include/itkExpectationMaximizationMixtureModelEstimator.h"

namespace itk
{
namespace Statistics
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const ExpectationMaximizationMixtureModelEstimatorEnums::TERMINATION_CODE value)
{
  return out << [value] {
    switch (value)
    {
      case ExpectationMaximizationMixtureModelEstimatorEnums::TERMINATION_CODE::CONVERGED:
        return "itk::Statistics::ExpectationMaximizationMixtureModelEstimatorEnums::TERMINATION_CODE::CONVERGED";
      case ExpectationMaximizationMixtureModelEstimatorEnums::TERMINATION_CODE::NOT_CONVERGED:
        return "itk::Statistics::ExpectationMaximizationMixtureModelEstimatorEnums::TERMINATION_CODE::NOT_CONVERGED";
      default:
        return "INVALID VALUE FOR itk::Statistics::ExpectationMaximizationMixtureModelEstimatorEnums::TERMINATION_CODE";
    }
  }();
}
} // end of namespace Statistics
} // end of namespace itk

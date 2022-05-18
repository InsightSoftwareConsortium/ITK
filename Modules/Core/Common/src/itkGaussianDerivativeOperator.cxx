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
#include "itkGaussianDerivativeOperator.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const GaussianDerivativeOperatorEnums::InterpolationMode value)
{
  return out << [value] {
    switch (value)
    {
      case GaussianDerivativeOperatorEnums::InterpolationMode::NearestNeighbourInterpolation:
        return "itk::GaussianDerivativeOperatorEnums::InterpolationMode::NearestNeighbourInterpolation";
      case GaussianDerivativeOperatorEnums::InterpolationMode::LinearInterpolation:
        return "itk::GaussianDerivativeOperatorEnums::InterpolationMode::LinearInterpolation";
      default:
        return "INVALID VALUE FOR itk::GaussianDerivativeOperatorEnums::InterpolationMode";
    }
  }();
}
} // namespace itk

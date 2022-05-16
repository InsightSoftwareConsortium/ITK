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
#include "itkESMDemonsRegistrationFunction.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const ESMDemonsRegistrationFunctionEnums::Gradient value)
{
  return out << [value] {
    switch (value)
    {
      case ESMDemonsRegistrationFunctionEnums::Gradient::Symmetric:
        return "itk::ESMDemonsRegistrationFunctionEnums::Gradient::Symmetric";
      case ESMDemonsRegistrationFunctionEnums::Gradient::Fixed:
        return "itk::ESMDemonsRegistrationFunctionEnums::Gradient::Fixed";
      case ESMDemonsRegistrationFunctionEnums::Gradient::WarpedMoving:
        return "itk::ESMDemonsRegistrationFunctionEnums::Gradient::WarpedMoving";
      case ESMDemonsRegistrationFunctionEnums::Gradient::MappedMoving:
        return "itk::ESMDemonsRegistrationFunctionEnums::Gradient::MappedMoving";
      default:
        return "INVALID VALUE FOR itk::ESMDemonsRegistrationFunctionEnums::Gradient";
    }
  }();
}
} // end namespace itk

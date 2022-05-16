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
#include "itkSymmetricEigenAnalysisImageFilter.h"

namespace itk
{
namespace Functor
{
/** Define how to print enumerations */
std::ostream &
operator<<(std::ostream & out, const EigenValueOrderEnum value)
{
  return out << [value] {
    switch (value)
    {
      case EigenValueOrderEnum::OrderByValue:
        return "EigenValueOrderEnum::OrderByValue";
      case EigenValueOrderEnum::OrderByMagnitude:
        return "EigenValueOrderEnum::OrderByMagnitude";
      case EigenValueOrderEnum::DoNotOrder:
        return "EigenValueOrderEnum::DoNotOrder";
      default:
        return "INVALID VALUE FOR EigenValueOrderEnum";
    }
  }();
}
} // namespace Functor
} // end namespace itk

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
#include "itkSymmetricEigenAnalysisImageFilter.h"

namespace itk
{
namespace Functor
{
/** Define how to print enumerations */
std::ostream &
operator<<(std::ostream & out, const OrderTypeOfEigenValue value)
{
  return out << [value] {
    switch (value)
    {
      case OrderTypeOfEigenValue::OrderByValue:
        return "OrderTypeOfEigenValue::OrderByValue";
      case OrderTypeOfEigenValue::OrderByMagnitude:
        return "OrderTypeOfEigenValue::OrderByMagnitude";
      case OrderTypeOfEigenValue::DoNotOrder:
        return "OrderTypeOfEigenValue::DoNotOrder";
      default:
        return "INVALID VALUE FOR OrderTypeOfEigenValue";
    }
  }();
}
} // namespace Functor
} // end namespace itk

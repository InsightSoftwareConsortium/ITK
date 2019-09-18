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
#include "itkSymmetricEigenAnalysis.h"

namespace itk
{
std::ostream &
operator<<(std::ostream & out, const OrderType value)
{
  return out << [value] {
    switch (value)
    {
      case OrderType::OrderByValue:
        return "OrderType::OrderByValue";
      case OrderType::OrderByMagnitude:
        return "OrderType::OrderByMagnitude";
      case OrderType::DoNotOrder:
        return "OrderType::DoNotOrder";
      default:
        return "INVALID VALUE FOR OrderType";
    }
  }();
}
} // end namespace itk

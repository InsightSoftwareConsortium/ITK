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
#include "itkTreeIteratorBase.h"

namespace itk
{
/** Print enumerations */
std::ostream &
operator<<(std::ostream & out, const TreeIteratorBaseNodeEnum value)
{
  return out << [value] {
    switch (value)
    {
      case TreeIteratorBaseNodeEnum::UNDEFIND:
        return "TreeIteratorBaseNodeEnum::UNDEFIND";
      case TreeIteratorBaseNodeEnum::PREORDER:
        return "TreeIteratorBaseNodeEnum::PREORDER";
      case TreeIteratorBaseNodeEnum::INORDER:
        return "TreeIteratorBaseNodeEnum::INORDER";
      case TreeIteratorBaseNodeEnum::POSTORDER:
        return "TreeIteratorBaseNodeEnum::POSTORDER";
      case TreeIteratorBaseNodeEnum::LEVELORDER:
        return "TreeIteratorBaseNodeEnum::LEVELORDER";
      case TreeIteratorBaseNodeEnum::CHILD:
        return "TreeIteratorBaseNodeEnum::CHILD";
      case TreeIteratorBaseNodeEnum::ROOT:
        return "TreeIteratorBaseNodeEnum::ROOT";
      case TreeIteratorBaseNodeEnum::LEAF:
        return "TreeIteratorBaseNodeEnum::LEAF";
      default:
        return "INVALID VALUE FOR TreeIteratorBaseNodeEnum";
    }
  }();
}
} // end namespace itk

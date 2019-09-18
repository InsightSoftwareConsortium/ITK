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
operator<<(std::ostream & out, const TreeIteratorBaseNodeType value)
{
  return out << [value] {
    switch (value)
    {
      case TreeIteratorBaseNodeType::UNDEFIND:
        return "TreeIteratorBaseNodeType::UNDEFIND";
      case TreeIteratorBaseNodeType::PREORDER:
        return "TreeIteratorBaseNodeType::PREORDER";
      case TreeIteratorBaseNodeType::INORDER:
        return "TreeIteratorBaseNodeType::INORDER";
      case TreeIteratorBaseNodeType::POSTORDER:
        return "TreeIteratorBaseNodeType::POSTORDER";
      case TreeIteratorBaseNodeType::LEVELORDER:
        return "TreeIteratorBaseNodeType::LEVELORDER";
      case TreeIteratorBaseNodeType::CHILD:
        return "TreeIteratorBaseNodeType::CHILD";
      case TreeIteratorBaseNodeType::ROOT:
        return "TreeIteratorBaseNodeType::ROOT";
      case TreeIteratorBaseNodeType::LEAF:
        return "TreeIteratorBaseNodeType::LEAF";
      default:
        return "INVALID VALUE FOR TreeIteratorBaseNodeType";
    }
  }();
}
} // end namespace itk

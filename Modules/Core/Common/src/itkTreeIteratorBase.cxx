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
operator<<( std::ostream & out, const TreeIteratorBaseNodeType value )
{
  const char * s = nullptr;
  switch ( value )
  {
    case TreeIteratorBaseNodeType::UNDEFIND:
      s = "TreeIteratorBaseNodeType::UNDEFIND";
      break;
    case TreeIteratorBaseNodeType::PREORDER:
      s = "TreeIteratorBaseNodeType::PREORDER";
      break;
    case TreeIteratorBaseNodeType::INORDER:
      s = "TreeIteratorBaseNodeType::INORDER";
      break;
    case TreeIteratorBaseNodeType::POSTORDER:
      s = "TreeIteratorBaseNodeType::POSTORDER";
      break;
    case TreeIteratorBaseNodeType::LEVELORDER:
      s = "TreeIteratorBaseNodeType::LEVELORDER";
      break;
    case TreeIteratorBaseNodeType::CHILD:
      s = "TreeIteratorBaseNodeType::CHILD";
      break;
    case TreeIteratorBaseNodeType::ROOT:
      s = "TreeIteratorBaseNodeType::ROOT";
      break;
    case TreeIteratorBaseNodeType::LEAF:
      s = "TreeIteratorBaseNodeType::LEAF";
      break;
    default:
      s = "INVALID VALUE FOR TreeIteratorBaseNodeType";
  }
  return out << s;
}
} // end namespace itk

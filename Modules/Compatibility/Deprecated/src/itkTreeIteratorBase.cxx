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
#include "itkTreeIteratorBase.h"

namespace itk
{
/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const TreeIteratorBaseEnums::TreeIteratorBaseNode value)
{
  return out << [value] {
    switch (value)
    {
      case TreeIteratorBaseEnums::TreeIteratorBaseNode::UNDEFIND:
        return "itk::TreeIteratorBaseEnums::TreeIteratorBaseNode::UNDEFIND";
      case TreeIteratorBaseEnums::TreeIteratorBaseNode::PREORDER:
        return "itk::TreeIteratorBaseEnums::TreeIteratorBaseNode::PREORDER";
      case TreeIteratorBaseEnums::TreeIteratorBaseNode::INORDER:
        return "itk::TreeIteratorBaseEnums::TreeIteratorBaseNode::INORDER";
      case TreeIteratorBaseEnums::TreeIteratorBaseNode::POSTORDER:
        return "itk::TreeIteratorBaseEnums::TreeIteratorBaseNode::POSTORDER";
      case TreeIteratorBaseEnums::TreeIteratorBaseNode::LEVELORDER:
        return "itk::TreeIteratorBaseEnums::TreeIteratorBaseNode::LEVELORDER";
      case TreeIteratorBaseEnums::TreeIteratorBaseNode::CHILD:
        return "itk::TreeIteratorBaseEnums::TreeIteratorBaseNode::CHILD";
      case TreeIteratorBaseEnums::TreeIteratorBaseNode::ROOT:
        return "itk::TreeIteratorBaseEnums::TreeIteratorBaseNode::ROOT";
      case TreeIteratorBaseEnums::TreeIteratorBaseNode::LEAF:
        return "itk::TreeIteratorBaseEnums::TreeIteratorBaseNode::LEAF";
      default:
        return "INVALID VALUE FOR itk::TreeIteratorBaseEnums::TreeIteratorBaseNode";
    }
  }();
}
} // end namespace itk

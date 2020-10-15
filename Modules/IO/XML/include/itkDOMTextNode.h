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

#ifndef itkDOMTextNode_h
#define itkDOMTextNode_h

#include "itkDOMNode.h"

#include <string>

namespace itk
{

/**
 * \class DOMTextNode
 * \brief Class to represent a special DOM node that holds a text string.
 *
 * A text node has no attributes and children. So a text node is always a leaf node,
 * and the special attribute "id" cannot be used.
 * In this implementation, a text node is internally represented using a special tag name of "!".
 * This is not a problem as "!" is not a valid tag name in any XML file.
 *
 * \ingroup ITKIOXML
 */
class DOMTextNode : public DOMNode
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DOMTextNode);

  /** Standard class type aliases. */
  using Self = DOMTextNode;
  using Superclass = DOMNode;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DOMTextNode, DOMNode);

  /** Functions to set/get the enclosed text of this node. */
  itkSetMacro(Text, std::string &);
  itkGetConstReferenceMacro(Text, std::string);

protected:
  DOMTextNode() { this->SetName("!"); }

private:
  /** Variable to hold the text string of this node. */
  std::string m_Text;
};

} // namespace itk

#endif // itkDOMTextNode_h

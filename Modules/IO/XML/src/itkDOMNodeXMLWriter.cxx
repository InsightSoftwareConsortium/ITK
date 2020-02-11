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

#include "itkDOMNodeXMLWriter.h"

#include <fstream>

namespace itk
{

DOMNodeXMLWriter::DOMNodeXMLWriter()
  : m_IndentStep("  ")
{}

/**
 * Function called by Update() or end-users to write the input DOM object
 * to an output stream such as file, string, console, etc.
 */
void
DOMNodeXMLWriter::Update(std::ostream & os, std::string indent)
{
  const InputType * input = this->GetInput();
  if (input == nullptr)
  {
    itkExceptionMacro("input object is null");
  }

  // if it is a text node
  const auto * tnode = dynamic_cast<const DOMTextNode *>(input);
  if (tnode)
  {
    os << indent << tnode->GetText() << std::endl;
    return;
  }

  // write the start tag name
  os << indent << "<" << input->GetName();

  // write the "id" attribute if it is present
  std::string id = input->GetID();
  if (!id.empty())
  {
    os << " id=\"" << id << "\"";
  }

  // write other attributes
  using AttributesListType = InputType::AttributesListType;
  AttributesListType attributes;
  input->GetAllAttributes(attributes);
  for (auto & attribute : attributes)
  {
    os << " " << attribute.first << "=\"" << attribute.second << "\"";
  }

  // write the ending of the start tag, and all children if applicable
  using ConstChildrenListType = InputType::ConstChildrenListType;
  ConstChildrenListType children;
  input->GetAllChildren(children);
  if (!children.empty())
  {
    // write the closing bracket for the start tag
    os << ">" << std::endl;
    // write the children
    for (auto & i : children)
    {
      this->SetInput(i);
      this->Update(os, indent + this->m_IndentStep);
      this->SetInput(input);
    }
    // write the end tag
    os << indent << "</" << input->GetName() << ">" << std::endl;
  }
  else
  {
    // write the special closing bracket for the start tag if it has no children
    os << "/>" << std::endl;
  }
}

/**
 * Function called by end-users to write the input DOM object to the output XML file.
 */
void
DOMNodeXMLWriter::Update()
{
  std::ofstream os(this->m_FileName.c_str());
  if (!os.is_open())
  {
    itkExceptionMacro("failed openning the output XML file");
  }

  this->Update(os);

  os.close();
}

} // namespace itk

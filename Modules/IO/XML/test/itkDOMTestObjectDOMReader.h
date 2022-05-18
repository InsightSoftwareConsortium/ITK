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

#ifndef itkDOMTestObjectDOMReader_h
#define itkDOMTestObjectDOMReader_h

#include "itkDOMReader.h"
#include "itkDOMTestObject.h"

namespace itk
{

class DOMTestObjectDOMReader : public DOMReader<DOMTestObject>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DOMTestObjectDOMReader);

  /** Standard class type aliases. */
  using Self = DOMTestObjectDOMReader;
  using Superclass = DOMReader<DOMTestObject>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DOMTestObjectDOMReader, DOMReader);

protected:
  DOMTestObjectDOMReader() = default;

  /**
   * This function is called automatically when update functions are performed.
   * It should fill the contents of the output object by pulling information from the intermediate DOM object.
   */
  void
  GenerateData(const DOMNodeType * inputdom, const void *) override;
};

inline void
DOMTestObjectDOMReader::GenerateData(const DOMNodeType * inputdom, const void *)
{
  OutputType * output = this->GetOutput();
  if (output == nullptr)
  {
    auto object = OutputType::New();
    output = (OutputType *)object;
    this->SetOutput(output);
  }

  FancyString   s;
  std::ifstream ifs;

  if (inputdom->GetName() != "DOMTestObject")
  {
    itkExceptionMacro("tag name DOMTestObject is expected");
  }

  // read child foo
  const DOMNodeType * foo = inputdom->GetChild("foo");
  if (foo == nullptr)
  {
    itkExceptionMacro("child foo not found");
  }
  s = foo->GetAttribute("fname");
  output->SetFooFileName(s);
  // read the foo value from file
  ifs.open(s.ToString().c_str());
  if (!ifs.is_open())
  {
    itkExceptionMacro("cannot read foo file");
  }
  std::string fooValue;
  ifs >> fooValue;
  output->SetFooValue(fooValue);
  ifs.close();
}

} // namespace itk

#endif // itkDOMTestObjectDOMReader_h

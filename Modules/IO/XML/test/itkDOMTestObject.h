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

#ifndef itkDOMTestObject_h
#define itkDOMTestObject_h

#include "itkObject.h"
#include <string>

namespace itk
{

class DOMTestObject : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DOMTestObject);

  /** Standard class type aliases. */
  using Self = DOMTestObject;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DOMTestObject, Object);

  /** Functions to set/get foo value. */
  itkSetMacro(FooValue, std::string);
  itkGetConstMacro(FooValue, std::string);

  /** Functions to set/get foo file name. */
  itkSetMacro(FooFileName, std::string);
  itkGetConstMacro(FooFileName, std::string);

private:
  DOMTestObject() = default;

  std::string m_FooValue;
  std::string m_FooFileName;
};

} // namespace itk

#endif // itkDOMTestObject_h

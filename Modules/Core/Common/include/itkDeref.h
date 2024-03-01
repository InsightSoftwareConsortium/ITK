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
#ifndef itkDeref_h
#define itkDeref_h

#include "itkMacro.h"
#include <string>
#include <typeinfo>

namespace itk
{

/** \class DerefError
 * Exception thrown when trying to dereference a null pointer.
 * \ingroup ITKCommon
 */
class DerefError : public ExceptionObject
{
public:
  // Inherit the constructors from its base class.
  using ExceptionObject::ExceptionObject;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(DerefError);
};


/** Dereferences the specified pointer, when the pointer is not null. Throws a `DerefError` exception when the pointer
 * is null. Aims to avoid undefined behavior from accidentally dereferencing a null pointer.
 */
template <typename T>
T &
Deref(T * const ptr)
{
  if (ptr == nullptr)
  {
    itkSpecializedMessageExceptionMacro(
      DerefError, "The pointer passed to `itk::Deref(T*)` is null! T's typeid name: `" << typeid(T).name() << '`');
  }
  return *ptr;
}
} // namespace itk

#endif

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
#ifndef itkFunctionBase_h
#define itkFunctionBase_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{
/**
 * \class FunctionBase
 * \brief Base class for all ITK function objects
 *
 * FunctionBase is the base class for ITK function objects. Specifically,
 * the abstract method Evaluate() maps a point from the input space to a point
 * in the output space.
 *
 * Subclasses must override Evaluate().
 *
 * This class is template over the input (domain) type and
 * the output (range) type.
 *
 * \ingroup Functions
 *
 * \ingroup ITKCommon
 */
template <typename TInput, typename TOutput>
class ITK_TEMPLATE_EXPORT FunctionBase : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FunctionBase);

  /** Standard class type aliases. */
  using Self = FunctionBase;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FunctionBase, Object);

  /** Input type */
  using InputType = TInput;

  /** Output type */
  using OutputType = TOutput;

  /** Evaluate at the specified input position */
  virtual OutputType
  Evaluate(const InputType & input) const = 0;

protected:
  FunctionBase() = default;
  ~FunctionBase() override = default;
};
} // end namespace itk

#endif

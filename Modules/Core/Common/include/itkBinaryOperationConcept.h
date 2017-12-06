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

#ifndef itkBinaryOperationConcept_h
#define itkBinaryOperationConcept_h

#include "itkPromoteType.h"

/// \cond HIDE_META_PROGRAMMING
namespace itk {
namespace Details {
namespace op {
/** Root Binary Operation Concept.
 * All binary operations are expected to inherit from this type.
 * These types will help to implement binary operations between arrays (\c
 * VariableLengthVector) through Expression Template technique.
 *
 * \c BinaryOperationConcept subclasses are expected to provide a template
 * function \c apply() that implements the actual binary operation (addition,
 * substraction, ...).
 * \ingroup ITKCommon
 * \sa \c itk::VariableLengthVector
 * \sa \c itk::VariableLengthVectorExpression
 */
struct BinaryOperationConcept { };

/** Addition operation.
 * The result type will be automatically promototed to the best type.
 * \ingroup ITKCommon
 */
struct Plus : BinaryOperationConcept
  {
  template <typename T1, typename T2>
    static typename mpl::PromoteType<T1, T2>::Type Apply(T1 const& lhs, T2 const& rhs)
      { return lhs + rhs; }
  };

/** Subtraction operation.
 * The result type will be automatically promototed to the best type.
 * \ingroup ITKCommon
 */
struct Sub : BinaryOperationConcept
  {
  template <typename T1, typename T2>
    static typename mpl::PromoteType<T1, T2>::Type Apply(T1 const& lhs, T2 const& rhs)
      { return lhs - rhs; }
  };

/** Multiplication operation.
 * The result type will be automatically promototed to the best type.
 * \ingroup ITKCommon
 */
struct Mult : BinaryOperationConcept
  {
  template <typename T1, typename T2>
    static typename mpl::PromoteType<T1, T2>::Type Apply(T1 const& lhs, T2 const& rhs)
      { return lhs * rhs; }
  };

/** Division operation.
 * The result type will be automatically promototed to the best type.
 * \ingroup ITKCommon
 */
struct Div : BinaryOperationConcept
  {
  template <typename T1, typename T2>
    static typename mpl::PromoteType<T1, T2>::Type Apply(T1 const& lhs, T2 const& rhs)
      { return lhs / rhs; }
  };

} // itk::Details::op namespace
} // itk::Details namespace
} // itk namespace
/// \endcond

#endif // itkBinaryOperationConcept_h

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
#ifndef itkInteriorExteriorSpatialFunction_h
#define itkInteriorExteriorSpatialFunction_h

#include "itkSpatialFunction.h"

namespace itk
{
/** \class InteriorExteriorSpatialFunction
 * \brief Returns whether or not a location is "inside" or "outside" a function
 *
 * InteriorExteriorSpatialFunction is a specialized version of SpatialFunction
 * where the output type is a boolean. In particular, the return type is understood
 * to mean the following:
 *
 * A return of 1 means inside or on the surface of the function,
 * 0 means outside the function
 *
 * There is no implied meaning in the terms "inside" or "outside"; although
 * the standard assumption is that "inside" means "bounded by a closed surface",
 * alternative definitions are also fine. For example, inside might be one side
 * of a plane, outside the other side.
 *
 * A typical use for an InteriorExteriorSpatialFunction is to generate test
 * primitives of arbitrary dimensionality, in conjunction with
 * itk::SpatialFunctionImageEvaluatorFilter or itk::FloodFilledSpatialFunctionConditionalIterator
 *
 * \ingroup SpatialFunctions
 * \ingroup ITKCommon
 */

template <unsigned int VDimension = 3, typename TInput = Point<double, VDimension>>
class ITK_TEMPLATE_EXPORT InteriorExteriorSpatialFunction : public SpatialFunction<bool, VDimension, TInput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(InteriorExteriorSpatialFunction);

  /** Standard class type aliases. */
  using Self = InteriorExteriorSpatialFunction;
  using Superclass = SpatialFunction<bool, VDimension, TInput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(InteriorExteriorSpatialFunction, SpatialFunction);

  /** Input type for the function */
  using InputType = typename Superclass::InputType;

  /** Output type for the function */
  using OutputType = typename Superclass::OutputType;

  /** Evaluate the function at a given position.
   * A return of 1 means inside or on the surface of the function,
   * 0 means outside the function
   * The actual definition of inside/outside is left up to the subclass */
  OutputType
  Evaluate(const InputType & input) const override = 0;

protected:
  InteriorExteriorSpatialFunction() = default;
  ~InteriorExteriorSpatialFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkInteriorExteriorSpatialFunction.hxx"
#endif

#endif

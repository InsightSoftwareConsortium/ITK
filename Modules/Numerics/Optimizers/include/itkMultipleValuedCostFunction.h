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
#ifndef itkMultipleValuedCostFunction_h
#define itkMultipleValuedCostFunction_h

#include "itkCostFunction.h"
#include "itkArray2D.h"
#include "itkNumericTraits.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class MultipleValuedCostFunction
 * \brief This class is a base for the CostFunctions returning a
 * multiple values

 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT MultipleValuedCostFunction : public CostFunction
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MultipleValuedCostFunction);

  /** Standard class type aliases. */
  using Self = MultipleValuedCostFunction;
  using Superclass = CostFunction;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultipleValuedCostFunction, CostFunction);

  /**  ParametersType type alias.
   *  It defines a position in the optimization search space. */
  using ParametersType = Superclass::ParametersType;

  /**  MeasureType type alias.
   *  It defines a type used to return the cost function value. */
  using MeasureType = Array<double>;

  /**  DerivativeType type alias.
   *  It defines a type used to return the cost function derivative.  */
  using DerivativeType = Array2D<double>;

  /** This method returns the value of the cost function corresponding
   * to the specified parameters.
   * This method MUST be overloaded by derived classes   */
  virtual MeasureType
  GetValue(const ParametersType & parameters) const = 0;

  /** Return the number of values that are computed by the
   *  multivalued cost function.
   *  This method MUST be overloaded by derived classes */
  virtual unsigned int
  GetNumberOfValues() const = 0;

  /** This method returns the derivative of the cost function corresponding
   * to the specified parameters
   * This method MUST be overloaded by derived classes   */
  virtual void
  GetDerivative(const ParametersType & parameters, DerivativeType & derivative) const = 0;

protected:
  MultipleValuedCostFunction() = default;
  ~MultipleValuedCostFunction() override;
};
} // end namespace itk

#endif

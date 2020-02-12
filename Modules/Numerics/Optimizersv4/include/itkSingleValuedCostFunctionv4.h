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
#ifndef itkSingleValuedCostFunctionv4_h
#define itkSingleValuedCostFunctionv4_h

#include "itkCostFunction.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 *\class SingleValuedCostFunctionv4Template
 * \brief This class is a base for a CostFunction that returns a
 * single value.
 *
 * This class differs from the SingleValuedCostFunction in that it is fine
 * tunned for managing very large numbers of parameters. For example, to be
 * used in conditions where the number of parameters is in the range of
 * thousands or even millions. Due to the large number of parameters, the API
 * of this class avoids any copying of the parameters array, and of the classes
 * that have dimensionality related to the size of the parameters array.
 *
 * As you look at the code of this class, keep in mind that the types
 * ParametersType and DerivativeType will be some sort of array-like type with
 * millions of elements.
 *
 * Derived classes must provide implementations for:
 *  GetValue
 *  GetValueAndDerivative
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizersv4
 */
template <typename TInternalComputationValueType>
class SingleValuedCostFunctionv4Template : public CostFunctionTemplate<TInternalComputationValueType>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SingleValuedCostFunctionv4Template);

  /** Standard class type aliases. */
  using Self = SingleValuedCostFunctionv4Template;
  using Superclass = CostFunctionTemplate<TInternalComputationValueType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SingleValuedCostFunctionv4Template, CostFunctionTemplate);

  /**  MeasureType type alias.
   *  It defines a type used to return the cost function value. */
  using MeasureType = TInternalComputationValueType;

  /**  ParametersType type alias.
   *  It defines a position in the optimization search space. */
  using ParametersType = typename Superclass::ParametersType;

  /** DerivativeType type alias.
   *  It defines a type used to return the cost function derivative.  */
  using DerivativeType = Array<TInternalComputationValueType>;

  /** This method returns the value of the cost function corresponding
   * to the specified parameters.    */
  virtual MeasureType
  GetValue() const = 0;

  /** This method returns the value and derivative of the cost function.
   * \c derivative will be sized and allocated as needed by metric.
   * If it's already proper size, no new allocation is done. */
  virtual void
  GetValueAndDerivative(MeasureType & value, DerivativeType & derivative) const = 0;

protected:
  SingleValuedCostFunctionv4Template() = default;
  ~SingleValuedCostFunctionv4Template() override = default;
};

/** This helps to meet backward compatibility */
using SingleValuedCostFunctionv4 = SingleValuedCostFunctionv4Template<double>;

} // end namespace itk

#endif

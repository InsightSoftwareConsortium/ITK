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
#ifndef itkMultipleValuedNonLinearOptimizer_h
#define itkMultipleValuedNonLinearOptimizer_h

#include "itkNonLinearOptimizer.h"
#include "itkMultipleValuedCostFunction.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class MultipleValuedNonLinearOptimizer
 * \brief This class is a base for the Optimization methods that
 * optimize a multiple valued function.
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT MultipleValuedNonLinearOptimizer : public NonLinearOptimizer
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MultipleValuedNonLinearOptimizer);

  /** Standard class type aliases. */
  using Self = MultipleValuedNonLinearOptimizer;
  using Superclass = NonLinearOptimizer;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Type of the Cost Function   */
  using CostFunctionType = MultipleValuedCostFunction;
  using CostFunctionPointer = CostFunctionType::Pointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultipleValuedNonLinearOptimizer, NonLinearOptimizer);

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  using ParametersType = Superclass::ParametersType;

  /**  Measure type.
   *  It defines a type used to return the cost function value.
   *  Here an Array is used for Multivalued functions   */
  using MeasureType = Array<double>;

  /**  Derivative type.
   *  It defines a type used to return the cost function derivative.
   *  Here a bidimensional Array is used for Multivalued functions   */
  using DerivativeType = Array2D<double>;

  /** Set the cost function. */
  virtual void
  SetCostFunction(CostFunctionType * costFunction);

protected:
  MultipleValuedNonLinearOptimizer();
  ~MultipleValuedNonLinearOptimizer() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  CostFunctionPointer m_CostFunction;
};
} // end namespace itk

#endif

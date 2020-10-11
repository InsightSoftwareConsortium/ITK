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
#ifndef itkSingleValuedNonLinearOptimizer_h
#define itkSingleValuedNonLinearOptimizer_h

#include "itkNonLinearOptimizer.h"
#include "itkSingleValuedCostFunction.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class SingleValuedNonLinearOptimizer
 * \brief This class is a base for the Optimization methods that
 * optimize a single valued function.
 *
 * \ingroup Numerics Optimizers
 *
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT SingleValuedNonLinearOptimizer : public NonLinearOptimizer
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SingleValuedNonLinearOptimizer);

  /** Standard "Self" type alias. */
  using Self = SingleValuedNonLinearOptimizer;
  using Superclass = NonLinearOptimizer;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SingleValuedNonLinearOptimizer, NonLinearOptimizer);

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  using ParametersType = Superclass::ParametersType;

  /** Type of the Cost Function   */
  using CostFunctionType = SingleValuedCostFunction;
  using CostFunctionPointer = CostFunctionType::Pointer;

  /**  Measure type.
   *  It defines a type used to return the cost function value.  */
  using MeasureType = CostFunctionType::MeasureType;

  /**  Derivative type.
   *  It defines a type used to return the cost function derivative. */
  using DerivativeType = CostFunctionType::DerivativeType;

  /** Set the cost function. */
  virtual void
  SetCostFunction(CostFunctionType * costFunction);

  /** Get the cost function. */
  itkGetModifiableObjectMacro(CostFunction, CostFunctionType);

  /** Get the cost function value at the given parameters. */
  MeasureType
  GetValue(const ParametersType & parameters) const;

protected:
  SingleValuedNonLinearOptimizer();
  ~SingleValuedNonLinearOptimizer() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  CostFunctionPointer m_CostFunction;
};
} // end namespace itk

#endif

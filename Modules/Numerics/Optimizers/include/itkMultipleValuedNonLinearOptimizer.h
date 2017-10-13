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
class ITKOptimizers_EXPORT MultipleValuedNonLinearOptimizer:
  public NonLinearOptimizer
{
public:
  /** Standard class typedefs. */
  typedef MultipleValuedNonLinearOptimizer Self;
  typedef NonLinearOptimizer               Superclass;
  typedef SmartPointer< Self >             Pointer;
  typedef SmartPointer< const Self >       ConstPointer;

  /** Type of the Cost Function   */
  typedef  MultipleValuedCostFunction CostFunctionType;
  typedef  CostFunctionType::Pointer  CostFunctionPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultipleValuedNonLinearOptimizer, NonLinearOptimizer);

  /**  Parameters type.
   *  It defines a position in the optimization search space. */
  typedef Superclass::ParametersType ParametersType;

  /**  Measure type.
   *  It defines a type used to return the cost function value.
   *  Here an Array is used for Multivalued functions   */
  typedef Array< double > MeasureType;

  /**  Derivative type.
   *  It defines a type used to return the cost function derivative.
   *  Here a bidimensional Array is used for Multivalued functions   */
  typedef Array2D< double > DerivativeType;

  /** Set the cost function. */
  virtual void SetCostFunction(CostFunctionType *costFunction);

protected:
  MultipleValuedNonLinearOptimizer();
  virtual ~MultipleValuedNonLinearOptimizer() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  CostFunctionPointer m_CostFunction;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultipleValuedNonLinearOptimizer);
};
} // end namespace itk

#endif

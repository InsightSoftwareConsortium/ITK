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
class ITKOptimizers_EXPORT MultipleValuedCostFunction:
  public CostFunction
{
public:
  /** Standard class typedefs. */
  typedef MultipleValuedCostFunction Self;
  typedef CostFunction               Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultipleValuedCostFunction, CostFunction);

  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef Superclass::ParametersType ParametersType;

  /**  MeasureType typedef.
   *  It defines a type used to return the cost function value. */
  typedef Array< double > MeasureType;

  /**  GradientType typedef.
   *  It defines a type used to return the cost function derivative.  */
  typedef Array2D< double > DerivativeType;

  /** This method returns the value of the cost function corresponding
    * to the specified parameters.
    * This method MUST be overloaded by derived classes   */
  virtual MeasureType GetValue(const ParametersType & parameters) const = 0;

  /** Return the number of values that are computed by the
   *  multivalued cost function.
   *  This method MUST be overloaded by derived classes */
  virtual unsigned int GetNumberOfValues(void) const  = 0;

  /** This method returns the derivative of the cost function corresponding
    * to the specified parameters
    * This method MUST be overloaded by derived classes   */
  virtual void GetDerivative(const ParametersType & parameters,
                             DerivativeType & derivative) const = 0;

protected:
  MultipleValuedCostFunction() {}
  virtual ~MultipleValuedCostFunction() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultipleValuedCostFunction);
};
} // end namespace itk

#endif

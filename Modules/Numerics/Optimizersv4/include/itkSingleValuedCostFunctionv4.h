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
#ifndef __itkSingleValuedCostFunctionv4_h
#define __itkSingleValuedCostFunctionv4_h

#include "itkCostFunction.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class SingleValuedCostFunctionTemplatev4
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
template< class TInternalComputationValueType >
class SingleValuedCostFunctionTemplatev4:
  public CostFunctionTemplate<TInternalComputationValueType>
{
public:
  /** Standard class typedefs. */
  typedef SingleValuedCostFunctionTemplatev4                    Self;
  typedef CostFunctionTemplate<TInternalComputationValueType>   Superclass;
  typedef SmartPointer< Self >                                  Pointer;
  typedef SmartPointer< const Self >                            ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SingleValuedCostFunctionTemplatev4, CostFunctionTemplate);

  /**  MeasureType typedef.
   *  It defines a type used to return the cost function value. */
  typedef TInternalComputationValueType                  MeasureType;

  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef typename Superclass::ParametersType      ParametersType;

  /** DerivativeType typedef.
   *  It defines a type used to return the cost function derivative.  */
  typedef Array< TInternalComputationValueType > DerivativeType;

  /** This method returns the value of the cost function corresponding
    * to the specified parameters.    */
  virtual MeasureType GetValue() const = 0;

  /** This method returns the value and derivative of the cost function.
   * \c derivative will be sized and allocated as needed by metric.
   * If it's already proper size, no new allocation is done. */
  virtual void GetValueAndDerivative(MeasureType & value,
                                     DerivativeType & derivative) const = 0;

protected:
  SingleValuedCostFunctionTemplatev4() {}
  virtual ~SingleValuedCostFunctionTemplatev4() {}

private:
  SingleValuedCostFunctionTemplatev4(const Self &); //purposely not implemented
  void operator=(const Self &);           //purposely not implemented
};

/** This helps to meet backward compatibility */
typedef SingleValuedCostFunctionTemplatev4<double> SingleValuedCostFunctionv4;

} // end namespace itk

#endif

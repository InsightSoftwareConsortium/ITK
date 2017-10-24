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
#ifndef itkMultipleValuedNonLinearVnlOptimizer_h
#define itkMultipleValuedNonLinearVnlOptimizer_h

#include "itkMultipleValuedNonLinearOptimizer.h"
#include "itkMultipleValuedVnlCostFunctionAdaptor.h"
#include "itkCommand.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class MultipleValuedNonLinearVnlOptimizer
 * \brief This class is a base for the Optimization methods that
 * optimize a multi-valued function.
 *
 * It is an Adaptor class for optimizers provided by the vnl library
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT MultipleValuedNonLinearVnlOptimizer:
  public MultipleValuedNonLinearOptimizer
{
public:
  /** Standard class typedefs. */
  typedef MultipleValuedNonLinearVnlOptimizer Self;
  typedef MultipleValuedNonLinearOptimizer    Superclass;
  typedef SmartPointer< Self >                Pointer;
  typedef SmartPointer< const Self >          ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultipleValuedNonLinearVnlOptimizer,
               MultipleValueNonLinearOptimizer);

  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef Superclass::ParametersType ParametersType;

  /** Set the cost Function. This method has to be overloaded
   *  by derived classes because the CostFunctionAdaptor requires
   *  to know the number of parameters at construction time. This
   *  number of parameters is obtained at run-time from the itkCostFunction.
   *  As a consequence each derived optimizer should construct its own
   *  CostFunctionAdaptor when overloading this method  */
  virtual void SetCostFunction(MultipleValuedCostFunction *costFunction) ITK_OVERRIDE = 0;

  /**  Define if the Cost function should provide a customized
       Gradient computation or the gradient can be computed internally
       using a default approach  */
  void SetUseCostFunctionGradient(bool);

  void UseCostFunctionGradientOn()
  {
    this->SetUseCostFunctionGradient(true);
  }

  void UseCostFunctionGradientOff()
  {
    this->SetUseCostFunctionGradient(false);
  }

  bool GetUseCostFunctionGradient() const;

  /** Return Cached Values. These method have the advantage of not triggering a
   * recomputation of the metric value, but it has the disadvantage of
   * returning a value that may not be the one corresponding to the
   * current parameters. For GUI update purposes, this method is a
   * good option, for mathematical validation you should rather call
   * GetValue(). */
  itkGetConstReferenceMacro(CachedValue, MeasureType);
  itkGetConstReferenceMacro(CachedDerivative, DerivativeType);
  itkGetConstReferenceMacro(CachedCurrentPosition, ParametersType);

protected:
  MultipleValuedNonLinearVnlOptimizer();
  virtual ~MultipleValuedNonLinearVnlOptimizer() ITK_OVERRIDE;

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  typedef MultipleValuedVnlCostFunctionAdaptor CostFunctionAdaptorType;

  void SetCostFunctionAdaptor(CostFunctionAdaptorType *adaptor);

  const CostFunctionAdaptorType * GetCostFunctionAdaptor() const;

  CostFunctionAdaptorType * GetCostFunctionAdaptor();

  /** The purpose of this method is to get around the lack of const
   *  correctness in vnl cost_functions and optimizers */
  CostFunctionAdaptorType * GetNonConstCostFunctionAdaptor() const;

  /** Command observer that will interact with the ITKVNL cost-function
   * adaptor in order to generate iteration events. This will allow to overcome
   * the limitation of VNL optimizers not offering callbacks for every
   * iteration */
  typedef ReceptorMemberCommand< Self > CommandType;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultipleValuedNonLinearVnlOptimizer);

  /** Callback function for the Command Observer */
  void IterationReport(const EventObject & event);

  CostFunctionAdaptorType *m_CostFunctionAdaptor;
  bool                     m_UseGradient;

  CommandType::Pointer m_Command;

  mutable ParametersType m_CachedCurrentPosition;
  mutable MeasureType    m_CachedValue;
  mutable DerivativeType m_CachedDerivative;
};
} // end namespace itk

#endif

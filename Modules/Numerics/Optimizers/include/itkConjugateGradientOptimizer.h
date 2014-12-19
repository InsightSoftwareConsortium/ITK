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
#ifndef itkConjugateGradientOptimizer_h
#define itkConjugateGradientOptimizer_h

#include "itkIntTypes.h"
#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_conjugate_gradient.h"

namespace itk
{
/** \class ConjugateGradientOptimizer
 * \brief Wrap of the vnl_conjugate_gradient
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ConjugateGradientOptimizer:
  public SingleValuedNonLinearVnlOptimizer

{
public:
  /** Standard class typedefs. */
  typedef ConjugateGradientOptimizer        Self;
  typedef SingleValuedNonLinearVnlOptimizer Superclass;
  typedef SmartPointer< Self >              Pointer;
  typedef SmartPointer< const Self >        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConjugateGradientOptimizer, SingleValuedNonLinearOptimizer);

  /** InternalParameters typedef. */
  typedef   vnl_vector< double > InternalParametersType;

  /** Internal Optimizer Type */
  typedef   vnl_conjugate_gradient InternalOptimizerType;

  /** Method for getting access to the internal optimizer */
  vnl_conjugate_gradient * GetOptimizer(void);

  /** Start optimization with an initial value. */
  virtual void StartOptimization(void) ITK_OVERRIDE;

  /** Plug in a Cost Function into the optimizer  */
  virtual void SetCostFunction(SingleValuedCostFunction *costFunction) ITK_OVERRIDE;

  /** Return the number of iterations performed so far */
  SizeValueType GetNumberOfIterations(void) const;

  SizeValueType GetCurrentIteration(void) const;

  /** Return Current Value */
  MeasureType GetValue() const;

protected:
  ConjugateGradientOptimizer();
  virtual ~ConjugateGradientOptimizer();

  typedef Superclass::CostFunctionAdaptorType CostFunctionAdaptorType;

private:
  ConjugateGradientOptimizer(const Self &); //purposely not implemented
  void operator=(const Self &);             //purposely not implemented

  /**  The vnl optimization method for conjugate gradient. */
  bool                   m_OptimizerInitialized;
  InternalOptimizerType *m_VnlOptimizer;
};
} // end namespace itk

#endif

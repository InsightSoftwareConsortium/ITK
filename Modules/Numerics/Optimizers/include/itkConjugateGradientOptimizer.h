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
#ifndef itkConjugateGradientOptimizer_h
#define itkConjugateGradientOptimizer_h

#include "itkIntTypes.h"
#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_conjugate_gradient.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class ConjugateGradientOptimizer
 * \brief Wrap of the vnl_conjugate_gradient
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT ConjugateGradientOptimizer : public SingleValuedNonLinearVnlOptimizer

{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ConjugateGradientOptimizer);

  /** Standard class type aliases. */
  using Self = ConjugateGradientOptimizer;
  using Superclass = SingleValuedNonLinearVnlOptimizer;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConjugateGradientOptimizer, SingleValuedNonLinearOptimizer);

  /** InternalParameters type alias. */
  using InternalParametersType = vnl_vector<double>;

  /** Internal Optimizer Type */
  using InternalOptimizerType = vnl_conjugate_gradient;

  /** Method for getting access to the internal optimizer */
  vnl_conjugate_gradient *
  GetOptimizer();

  /** Start optimization with an initial value. */
  void
  StartOptimization() override;

  /** Plug in a Cost Function into the optimizer  */
  void
  SetCostFunction(SingleValuedCostFunction * costFunction) override;

  /** Return the number of iterations performed so far */
  SizeValueType
  GetNumberOfIterations() const;

  SizeValueType
  GetCurrentIteration() const;

  /** Return Current Value */
  MeasureType
  GetValue() const;

protected:
  ConjugateGradientOptimizer();
  ~ConjugateGradientOptimizer() override;

  using CostFunctionAdaptorType = Superclass::CostFunctionAdaptorType;

private:
  /**  The vnl optimization method for conjugate gradient. */
  bool                    m_OptimizerInitialized;
  InternalOptimizerType * m_VnlOptimizer;
};
} // end namespace itk

#endif

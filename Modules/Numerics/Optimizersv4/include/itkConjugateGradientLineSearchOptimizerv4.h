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
#ifndef __itkConjugateGradientLineSearchOptimizerv4_h
#define __itkConjugateGradientLineSearchOptimizerv4_h

#include "itkGradientDescentLineSearchOptimizerv4.h"
#include "itkOptimizerParameterScalesEstimator.h"
#include "itkWindowConvergenceMonitoringFunction.h"

namespace itk
{
/** \class ConjugateGradientLineSearchOptimizerv4
 *  \brief Conjugate gradient descent optimizer with a golden section line search for nonlinear optimization.
 *
 * ConjugateGradientLineSearchOptimizer implements a conjugate gradient descent optimizer
 * that is followed by a line search to find the best value for the learning rate.
 * At each iteration the current position is updated according to
 *
 * \f[
 *        p_{n+1} = p_n
 *                + \mbox{learningRateByGoldenSectionLineSearch}
 *                 \, d
 * \f]
 *
 * where d is defined as the Polak-Ribiere conjugate gradient.
 *
 * Options are identical to the superclass's.
 *
 * \ingroup ITKOptimizersv4
 */
class ITK_EXPORT ConjugateGradientLineSearchOptimizerv4
  : public GradientDescentLineSearchOptimizerv4
{
public:
  /** Standard class typedefs. */
  typedef ConjugateGradientLineSearchOptimizerv4  Self;
  typedef GradientDescentOptimizerv4              Superclass;
  typedef SmartPointer< Self >                    Pointer;
  typedef SmartPointer< const Self >              ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConjugateGradientLineSearchOptimizerv4, GradientDescentLineSearchOptimizerv4);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Derivative type */
  typedef Superclass::DerivativeType      DerivativeType;

  /** Metric type over which this class is templated */
  typedef Superclass::MeasureType                  MeasureType;
  typedef Superclass::InternalComputationValueType InternalComputationValueType;

  /** Type for the convergence checker */
  typedef itk::Function::WindowConvergenceMonitoringFunction<double> ConvergenceMonitoringType;

  virtual void StartOptimization( bool doOnlyInitialization = false );

protected:

  /** Advance one Step following the gradient direction.
   * Includes transform update. */
  virtual void AdvanceOneStep(void);

  /** Default constructor */
  ConjugateGradientLineSearchOptimizerv4();

  /** Destructor */
  virtual ~ConjugateGradientLineSearchOptimizerv4();

  virtual void PrintSelf( std::ostream & os, Indent indent ) const;

private:

  DerivativeType               m_LastGradient;
  DerivativeType               m_ConjugateGradient;

  ConjugateGradientLineSearchOptimizerv4( const Self & ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

};

} // end namespace itk

#endif

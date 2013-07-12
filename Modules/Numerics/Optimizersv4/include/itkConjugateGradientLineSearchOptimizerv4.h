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
  /** \class ConjugateGradientLineSearchOptimizerTemplatev4
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
template<class TInternalComputationValueType>
class ConjugateGradientLineSearchOptimizerTemplatev4
: public GradientDescentLineSearchOptimizerTemplatev4<TInternalComputationValueType>
{
public:
  /** Standard class typedefs. */
  typedef ConjugateGradientLineSearchOptimizerTemplatev4                        Self;
  typedef GradientDescentLineSearchOptimizerTemplatev4<TInternalComputationValueType> Superclass;
  typedef SmartPointer< Self >                                                  Pointer;
  typedef SmartPointer< const Self >                                            ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ConjugateGradientLineSearchOptimizerTemplatev4, Superclass);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** It should be possible to derive the internal computation type from the class object. */
  typedef TInternalComputationValueType            InternalComputationValueType;

  /** Derivative type */
  typedef typename Superclass::DerivativeType      DerivativeType;

  /** Metric type over which this class is templated */
  typedef typename Superclass::MeasureType                  MeasureType;

  /** Type for the convergence checker */
  typedef itk::Function::WindowConvergenceMonitoringFunction<TInternalComputationValueType> ConvergenceMonitoringType;

  virtual void StartOptimization( bool doOnlyInitialization = false );

protected:

  /** Advance one Step following the gradient direction.
   * Includes transform update. */
  virtual void AdvanceOneStep(void);

  /** Default constructor */
  ConjugateGradientLineSearchOptimizerTemplatev4();

  /** Destructor */
  virtual ~ConjugateGradientLineSearchOptimizerTemplatev4();

  virtual void PrintSelf( std::ostream & os, Indent indent ) const;

private:

  DerivativeType               m_LastGradient;
  DerivativeType               m_ConjugateGradient;

  ConjugateGradientLineSearchOptimizerTemplatev4( const Self & ); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

};

/** This helps to meet backward compatibility */
typedef ConjugateGradientLineSearchOptimizerTemplatev4<double> ConjugateGradientLineSearchOptimizerv4;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConjugateGradientLineSearchOptimizerv4.hxx"
#endif

#endif

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
#ifndef __itkLBFGSOptimizer_h
#define __itkLBFGSOptimizer_h

#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_lbfgs.h"

namespace itk
{
/** \class LBFGSOptimizer
 * \brief Wrap of the vnl_lbfgs algorithm
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class LBFGSOptimizer:
  public SingleValuedNonLinearVnlOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef LBFGSOptimizer                    Self;
  typedef SingleValuedNonLinearVnlOptimizer Superclass;
  typedef SmartPointer< Self >              Pointer;
  typedef SmartPointer< const Self >        ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LBFGSOptimizer, SingleValuedNonLinearVnlOptimizer);

  /** InternalParameters typedef. */
  typedef   vnl_vector< double > InternalParametersType;

  /** Internal optimizer type. */
  typedef   vnl_lbfgs InternalOptimizerType;

  /** Method for getting access to the internal optimizer. */
  vnl_lbfgs * GetOptimizer(void);

  /** Start optimization with an initial value. */
  void StartOptimization(void);

  /** Plug in a Cost Function into the optimizer  */
  virtual void SetCostFunction(SingleValuedCostFunction *costFunction);

  /** Set/Get the optimizer trace flag. If set to true, the optimizer
   * prints out information every iteration.
   */
  virtual void SetTrace(bool flag);

  itkGetMacro(Trace, bool);
  itkBooleanMacro(Trace);

  /** Set/Get the maximum number of function evaluations allowed. */
  virtual void SetMaximumNumberOfFunctionEvaluations(unsigned int n);

  itkGetMacro(MaximumNumberOfFunctionEvaluations, unsigned int);

  /** Set/Get the gradient convergence tolerance. This is a positive
   * real number that determines the accuracy with which the solution is to
   * be found. The optimization terminates when:
   * ||G|| < gtol max(1,||X||) where ||.|| denotes the Euclidean norm.
   */
  virtual void SetGradientConvergenceTolerance(double gtol);

  itkGetMacro(GradientConvergenceTolerance, double);

  /** Set/Get the line search accuracy. This is a positive real number
   * with a default value of 0.9, which controls the accuracy of the line
   * search. If the function and gradient evalutions are inexpensive with
   * respect to the cost of the iterations it may be advantageous to set
   * the value to a small value (say 0.1).
   */
  virtual void SetLineSearchAccuracy(double tol);

  itkGetMacro(LineSearchAccuracy, double);

  /** Set/Get the default step size. This is a positive real number
   * with a default value of 1.0 which determines the stpe size in the line
   * search.
   */
  virtual void SetDefaultStepLength(double stp);

  itkGetMacro(DefaultStepLength, double);

  /** Return Current Value */
  MeasureType GetValue() const;

  /** Get the reason for termination */
  const std::string GetStopConditionDescription() const;

protected:
  LBFGSOptimizer();
  virtual ~LBFGSOptimizer();
  void PrintSelf(std::ostream & os, Indent indent) const;

  typedef Superclass::CostFunctionAdaptorType CostFunctionAdaptorType;

private:
  LBFGSOptimizer(const Self &); //purposely not implemented
  void operator=(const Self &); //purposely not implemented

  bool                       m_OptimizerInitialized;
  InternalOptimizerType *    m_VnlOptimizer;
  mutable std::ostringstream m_StopConditionDescription;

  bool         m_Trace;
  unsigned int m_MaximumNumberOfFunctionEvaluations;
  double       m_GradientConvergenceTolerance;
  double       m_LineSearchAccuracy;
  double       m_DefaultStepLength;
};
} // end namespace itk

#endif

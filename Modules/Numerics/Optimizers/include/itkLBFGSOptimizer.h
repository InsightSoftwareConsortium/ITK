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
#ifndef itkLBFGSOptimizer_h
#define itkLBFGSOptimizer_h

#include "itkSingleValuedNonLinearVnlOptimizer.h"
#include "vnl/algo/vnl_lbfgs.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class LBFGSOptimizer
 * \brief Wrap of the vnl_lbfgs algorithm for use in ITKv4 registration framework.
 * The vnl_lbfgs is a wrapper for the NETLIB fortran code by Nocedal [1].
 *
 * LBFGS is a quasi-Newton method. Quasi-Newton methods use an approximate estimate
 * of the inverse Hessian \f$ (\nabla^2 f(x) )^{-1} \f$ to scale the gradient step:
 * \f[
 * x_{n+1} = x_n - s (\nabla^2 f(x_n) )^{-1} \nabla f(x)
 * \f]
 * with \f$ s \f$ the step size.
 *
 * The inverse Hessian is approximated from the gradients of previous iteration and
 * thus only the gradient of the objective function is required.
 *
 * The step size \f$ s \f$ is determined through line search with the approach
 * by More and Thuente [4]. This line search approach finds a step size such that
 * \f[
 * \lVert \nabla f(x + s (\nabla^2 f(x_n) )^{-1} \nabla f(x) ) \rVert
 *   \le
 * \nu \lVert \nabla f(x) \rVert
 * \f]
 * The parameter \f$ \nu \f$ is set through SetLineSearchAccuracy() (default 0.9)
 * The default step length, i.e. starting step length for the line search,
 * is set through SetDefaultStepLength() (default 1.0).
 *
 * The optimization stops when either the gradient satisfies the condition
 * \f[
 * \lVert \nabla f(x) \rVert \le \epsilon \max(1, \lVert X \rVert)
 * \f]
 * or a maximum number of function evaluations has been reached.
 * The tolerance \f$\epsilon\f$ is set through SetGradientConvergenceTolerance()
 * (default 1e-5) and the maximum number of function evaluations is set
 * through SetMaximumNumberOfFunctionEvaluations() (default 2000).
 *
 * Note: The scales set through SetScales should be set or left at one.
 * Otherwise the Hessian approximation will be disturbed and the
 * optimizer is unlikely to find a minima.
 *
 *
 * References:
 *
 * [1] [NETLIB lbfgs](http://users.iems.northwestern.edu/~nocedal/lbfgs.html)
 *
 * [2] Jorge Nocedal.
 * Updating Quasi-Newton Matrices with Limited Storage.
 * Mathematics of Computation, Vol. 35, No. 151, pp. 773-782, 1980.
 *
 * [3] Dong C. Liu and Jorge Nocedal.
 * On the limited memory BFGS method for large scale optimization.
 * Mathematical Programming B, Vol. 45, No. 3, pp. 503-528, 1989.
 *
 * [4] More, J. J. and D. J. Thuente.
 * Line Search Algorithms with Guaranteed Sufficient Decrease.
 * ACM Transactions on Mathematical Software 20, no. 3 (1994): 286-307.
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT LBFGSOptimizer:
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
  vnl_lbfgs * GetOptimizer();

  /** Start optimization with an initial value. */
  virtual void StartOptimization(void) ITK_OVERRIDE;

  /** Plug in a Cost Function into the optimizer  */
  virtual void SetCostFunction(SingleValuedCostFunction *costFunction) ITK_OVERRIDE;

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
  virtual const std::string GetStopConditionDescription() const ITK_OVERRIDE;

protected:
  LBFGSOptimizer();
  virtual ~LBFGSOptimizer() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  typedef Superclass::CostFunctionAdaptorType CostFunctionAdaptorType;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LBFGSOptimizer);

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

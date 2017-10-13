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
#ifndef itkLBFGSOptimizerv4_h
#define itkLBFGSOptimizerv4_h

#include "itkLBFGSOptimizerBasev4.h"
#include "vnl/algo/vnl_lbfgs.h"
#include "ITKOptimizersv4Export.h"

namespace itk
{
/** \class LBFGSOptimizerv4
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
 * Note: The scaling of the optimization paramaters, set through SetScales(),
 * should be set or left at one. Otherwise the Hessian approximation as well as
 * the line search will be disturbed and the optimizer is unlikely to find a minima.
 *
 *
 * References:
 *
 * [1] [NETLIB lbfgs](http://users.iems.northwestern.edu/~nocedal/lbfgs.html)
 *
 * [2] Jorge Nocedal.
 * Updating Quasi-Newton Matrices with Limited Storage.
 * Mathematics of Computation, Vol. 35, No. 151, pp. 773–782, 1980.
 *
 * [3] Dong C. Liu and Jorge Nocedal.
 * On the limited memory BFGS method for large scale optimization.
 * Mathematical Programming B, Vol. 45, No. 3, pp. 503-528, 1989.
 *
 * [4] More, J. J. and D. J. Thuente.
 * Line Search Algorithms with Guaranteed Sufficient Decrease.
 * ACM Transactions on Mathematical Software 20, no. 3 (1994): 286–307.
 *
 * \ingroup ITKOptimizersv4
 */

class ITKOptimizersv4_EXPORT LBFGSOptimizerv4:
    public LBFGSOptimizerBasev4< vnl_lbfgs >
{
public:
  /** Standard "Self" typedef. */
  typedef LBFGSOptimizerv4                  Self;
  typedef LBFGSOptimizerBasev4<vnl_lbfgs>   Superclass;
  typedef SmartPointer< Self >              Pointer;
  typedef SmartPointer< const Self >        ConstPointer;

  typedef Superclass::MetricType     MetricType;
  typedef Superclass::ParametersType ParametersType;
  typedef Superclass::ScalesType     ScalesType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LBFGSOptimizerv4, Superclass);

  /** Start optimization with an initial value. */
  virtual void StartOptimization(bool doOnlyInitialization = false) ITK_OVERRIDE;

  /** Plug in a Cost Function into the optimizer  */
  virtual void SetMetric(MetricType *metric) ITK_OVERRIDE;

  void VerboseOn();
  void VerboseOff();

  /** Set/Get the line search accuracy. This is a positive real number
   * with a default value of 0.9, which controls the accuracy of the line
   * search. If the function and gradient evalutions are inexpensive with
   * respect to the cost of the iterations it may be advantageous to set
   * the value to a small value (say 0.1).
   */
  void SetLineSearchAccuracy(double tol);

  itkGetConstMacro(LineSearchAccuracy, double);

  /** Set/Get the default step size. This is a positive real number
   * with a default value of 1.0 which determines the step size in the line
   * search.
   */
  void SetDefaultStepLength(double stp);

  itkGetConstMacro(DefaultStepLength, double);

protected:
  LBFGSOptimizerv4();
  virtual ~LBFGSOptimizerv4() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** InternalParameters typedef. */
  typedef vnl_vector< double >  InternalParametersType;

  /** Internal optimizer type. */
  typedef   vnl_lbfgs           InternalOptimizerType;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LBFGSOptimizerv4);

  bool         m_Verbose;
  double       m_LineSearchAccuracy;
  double       m_DefaultStepLength;
};
} // end namespace itk
#endif

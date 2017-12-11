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
#ifndef itkLBFGS2Optimizerv4_h
#define itkLBFGS2Optimizerv4_h


#include "itkObjectToObjectOptimizerBase.h"
#include "ITKOptimizersv4Export.h"
#include "itkAutoPointer.h"


namespace itk
{
/** \class LBFGS2Optimizerv4
 * \brief Wrap of the libLBFGS[1] algorithm for use in ITKv4 registration framework.
 * LibLBFGS is a translation of LBFGS code by Nocedal [2] and adds the orthantwise
 * limited-memmory Quais-Newton method [3] for optimization with L1-norm on the
 * parameters.
 *
 * LBFGS is a quasi-Newton method uses an approximate estimate of the inverse Hessian
 * \f$ (\nabla^2 f(x) )^-1 \f$ to scale the gradient step:
 * \f[
 * x_{n+1} = x_n - s (\nabla^2 f(x_n) )^-1 \nabla f(x)
 * \f]
 * with \f$ s \f$ the step size.
 *
 * The inverse Hessian is approximated from the gradients of previous iteration and
 * thus only the gradient of the objective function is required.
 *
 * The step size \f$ s \f$ is determined through line search which defaults to
 * the approach by More and Thuente [4]. This line search approach finds a step
 * size such that
 * \f[
 * \lVert \nabla f(x + s (\nabla^2 f(x_n) )^{-1} \nabla f(x) ) \rVert
 *   \le
 * \nu \lVert \nabla f(x) \rVert
 * \f]
 * The parameter \f$\nu\f$ is set through SetLineSearchAccuracy() (default 0.9)
 * and SetGradientLineSearchAccuracy()
 *
 * Instead of the More-Tunete method, backtracking with three different
 * conditions [7] are availabe and can be set through SetLineSearch():
 *  - LINESEARCH_BACKTRACKING_ARMIJO
 *  - LINESEARCH_BACKTRACKING_WOLFE
 *  - LINESEARCH_BACKTRACKING_STRONG_WOLFE
 *
 * The optimization stops when either the gradient satisfies the condition
 * \f[
 * \lVert \nabla f(x) \rVert \le \epsilon \max(1, \lVert X \rVert)
 * \f]
 * or a maximum number of function evaluations has been reached.
 * The tolerance \f$\epsilon\f$ is set through SetSolutionAccuracy()
 * (default 1e-5) and the maximum number of function evaluations is set
 * through SetMaximumIterations() (default 0 = no maximum).
 *
 *
 * References:
 *
 * [1] [libLBFGS](http://www.chokkan.org/software/liblbfgs/)
 *
 * [2] [NETLIB lbfgs](http://users.iems.northwestern.edu/~nocedal/lbfgs.html)
 *
 * [3] Galen Andrew and Jianfeng Gao.
 * Scalable training of L1-regularized log-linear models.
 * 24th International Conference on Machine Learning, pp. 33-40, 2007.
 *
 * [4] Jorge Nocedal.
 * Updating Quasi-Newton Matrices with Limited Storage.
 * Mathematics of Computation, Vol. 35, No. 151, pp. 773–782, 1980.
 *
 * [5] Dong C. Liu and Jorge Nocedal.
 * On the limited memory BFGS method for large scale optimization.
 * Mathematical Programming B, Vol. 45, No. 3, pp. 503-528, 1989.
 *
 * [6] More, J. J. and D. J. Thuente.
 * Line Search Algorithms with Guaranteed Sufficient Decrease.
 * ACM Transactions on Mathematical Software 20, no. 3 (1994): 286–307.
 *
 * [7] John E. Dennis and Robert B. Schnabel.
 * Numerical Methods for Unconstrained Optimization and Nonlinear Equations,
 * Englewood Cliffs, 1983.
 *
 * \ingroup ITKOptimizersv4
 */


class ITKOptimizersv4_EXPORT LBFGS2Optimizerv4:
    public ObjectToObjectOptimizerBaseTemplate<double>{

public:

  enum LineSearchMethod{
    /** The default algorithm (MoreThuente method). */
    LINESEARCH_DEFAULT = 0,
    /** MoreThuente method proposd by More and Thuente. */
    LINESEARCH_MORETHUENTE = 0,
    /**
     * Backtracking method with the Armijo condition.
     *  The backtracking method finds the step length such that it satisfies
     *  the sufficient decrease (Armijo) condition,
     *    - f(x + a * d) <= f(x) + lbfgs_parameter_t::ftol * a * g(x)^T d,
     *
     *  where x is the current point, d is the current search direction, and
     *  a is the step length.
     */
    LINESEARCH_BACKTRACKING_ARMIJO = 1,
    /** The backtracking method with the defualt (regular Wolfe) condition. */
    LINESEARCH_BACKTRACKING = 2,
    /**
     * Backtracking method with regular Wolfe condition.
     *  The backtracking method finds the step length such that it satisfies
     *  both the Armijo condition (LINESEARCH_BACKTRACKING_ARMIJO)
     *  and the curvature condition,
     *    - g(x + a * d)^T d >= lbfgs_parameter_t::wolfe * g(x)^T d,
     *
     *  where x is the current point, d is the current search direction, and
     *  a is the step length.
     */
    LINESEARCH_BACKTRACKING_WOLFE = 2,
    /**
     * Backtracking method with strong Wolfe condition.
     *  The backtracking method finds the step length such that it satisfies
     *  both the Armijo condition (LINESEARCH_BACKTRACKING_ARMIJO)
     *  and the following condition,
     *    - |g(x + a * d)^T d| <= lbfgs_parameter_t::wolfe * |g(x)^T d|,
     *
     *  where x is the current point, d is the current search direction, and
     *  a is the step length.
     */
    LINESEARCH_BACKTRACKING_STRONG_WOLFE = 3,
  };


  /**
   * currently only double is used in lbfgs need to figure
   * out how to make it a template parameter and set the required
   * define so lbfgs.h uses the corrcet version
   **/
  typedef double PrecisionType;


  /** Standard "Self" typedef. */
  typedef LBFGS2Optimizerv4                           Self;
  typedef ObjectToObjectOptimizerBaseTemplate<double> Superclass;
  typedef SmartPointer< Self >                        Pointer;
  typedef SmartPointer< const Self >                  ConstPointer;

  typedef Superclass::MetricType     MetricType;
  typedef Superclass::ParametersType ParametersType;
  typedef Superclass::ScalesType     ScalesType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LBFGS2Optimizerv4, Superclass);

  /** Start optimization with an initial value. */
  virtual void StartOptimization(bool doOnlyInitialization = false) ITK_OVERRIDE;

  virtual const StopConditionReturnStringType GetStopConditionDescription() const ITK_OVERRIDE;

  /** This optimizer does not support scaling of the derivatives. */
  virtual void SetScales(const ScalesType &) ITK_OVERRIDE;

  /** This optimizer does not support weighting of the derivatives. */
  virtual void SetWeights(const ScalesType ) ITK_OVERRIDE;
  /**
  * Set/Get the number of corrections to approximate the inverse hessian matrix.
  * The L-BFGS routine stores the computation results of previous \c m
  * iterations to approximate the inverse hessian matrix of the current
  * iteration. This parameter controls the size of the limited memories
  * (corrections). The default value is \c 6. Values less than \c 3 are
  * not recommended. Large values will result in excessive computing time.
  */
  void SetHessianApproximationAccuracy(int m);
  int GetHessianApproximationAccuracy() const;

  /**
  * Set/Get epsilon for convergence test.
  * This parameter determines the accuracy with which the solution is to
  * be found. A minimization terminates when
  * \f$||g|| < \epsilon * max(1, ||x||)\f$,
  * where ||.|| denotes the Euclidean (L2) norm. The default value is
  * \c 1e-5.
  */
  void SetSolutionAccuracy(PrecisionType epsilon);
  PrecisionType GetSolutionAccuracy() const;

  /**
  * Set/Ger distance for delta-based convergence test.
  * This parameter determines the distance, in iterations, to compute
  * the rate of decrease of the objective function. If the value of this
  * parameter is zero, the library does not perform the delta-based
  * convergence test. The default value is \c 0.
  */
  void SetDeltaConvergenceDistance(int nPast);
  int GetDeltaConvergenceDistance() const;

  /**
  * Delta for convergence test.
  * This parameter determines the minimum rate of decrease of the
  * objective function. The library stops iterations when the
  * following condition is met:
  * \f$(f' - f) / f < \delta\f$,
  * where f' is the objective value of past iterations ago, and f is
  * the objective value of the current iteration.
  * The default value is \c 0.
  */
  void SetDeltaConvergenceTolerance(PrecisionType tol);
  PrecisionType GetDeltaConvergenceTolerance() const;


  /**
   * The maximum number of iterations.
   *  The lbfgs() function terminates an optimization process with
   *  \c LBFGSERR_MAXIMUMITERATION status code when the iteration count
   *  exceedes this parameter. Setting this parameter to zero continues an
   *  optimization process until a convergence or error. The default value
   *  is \c 0.
   */
  void SetMaximumIterations(int maxIterations);
  int GetMaximumIterations() const;

  /** Aliased to Set/Get MaximumIterations to match base class interface.
   */
  virtual SizeValueType GetNumberOfIterations() const  ITK_OVERRIDE { return GetMaximumIterations(); }
  virtual void SetNumberOfIterations( const SizeValueType _arg ) ITK_OVERRIDE { SetMaximumIterations(static_cast<int>(_arg)); }

  /**
   * The line search algorithm.
   * This parameter specifies a line search algorithm to be used by the
   * L-BFGS routine. See lbfgs.h for enumeration of line search type.
   * Defaults to More-Thuente's method.
   */
  void SetLineSearch(const LineSearchMethod &linesearch);
  LineSearchMethod GetLineSearch() const;

  /**
   * The maximum number of trials for the line search.
   *  This parameter controls the number of function and gradients evaluations
   *  per iteration for the line search routine. The default value is \c 20.
   */
  void SetMaximumLineSearchEvaluations(int n);
  int GetMaximumLineSearchEvaluations() const;

  /**
   * The minimum step of the line search routine.
   *  The default value is \c 1e-20. This value need not be modified unless
   *  the exponents are too large for the machine being used, or unless the
   *  problem is extremely badly scaled (in which case the exponents should
   *  be increased).
   */
  void SetMinimumLineSearchStep(PrecisionType step);
  PrecisionType GetMinimumLineSearchStep() const;

  /**
   * The maximum step of the line search.
   *  The default value is \c 1e+20. This value need not be modified unless
   *  the exponents are too large for the machine being used, or unless the
   *  problem is extremely badly scaled (in which case the exponents should
   *  be increased).
   */
  void SetMaximumLineSearchStep(PrecisionType step);
  PrecisionType GetMaximumLineSearchStep() const;

  /**
   * A parameter to control the accuracy of the line search routine.
   *  The default value is \c 1e-4. This parameter should be greater
   *  than zero and smaller than \c 0.5.
   */
  void SetLineSearchAccuracy( PrecisionType ftol );
  PrecisionType GetLineSearchAccuracy() const;


  /**
   * A coefficient for the Wolfe condition.
   *  This parameter is valid only when the backtracking line-search
   *  algorithm is used with the Wolfe condition,
   *  LINESEARCH_BACKTRACKING_STRONG_WOLFE or
   *  LINESEARCH_BACKTRACKING_WOLFE .
   *  The default value is \c 0.9. This parameter should be greater
   *  than the \c ftol parameter and smaller than \c 1.0.
   */
  void SetWolfeCoefficient( PrecisionType wc );
  PrecisionType GetWolfeCoefficient() const;


  /**
   * A parameter to control the gradient accuracy of the More-Thuente
   * line search routine.
   * The default value is \c 0.9. If the function and gradient
   * evaluations are inexpensive with respect to the cost of the
   * iteration (which is sometimes the case when solving very large
   * problems) it may be advantageous to set this parameter to a small
   * value. A typical small value is \c 0.1. This parameter shuold be
   * greater than the \c ftol parameter (\c 1e-4) and smaller than
   * \c 1.0.
   */
  void SetLineSearchGradientAccuracy( PrecisionType gtol );
  PrecisionType GetLineSearchGradientAccuracy() const;

  /**
   * The machine precision for floating-point values.
   *  This parameter must be a positive value set by a client program to
   *  estimate the machine precision. The line search routine will terminate
   *  with the status code (\c LBFGSERR_ROUNDING_ERROR) if the relative width
   *  of the interval of uncertainty is less than this parameter.
   */
  void SetMachinePrecisionTolerance(PrecisionType xtol);
  PrecisionType GetMachinePrecisionTolerance() const;

  /**
   * Coeefficient for the L1 norm of variables.
   *  This parameter should be set to zero for standard minimization
   *  problems. Setting this parameter to a positive value activates
   *  Orthant-Wise Limited-memory Quasi-Newton (OWL-QN) method, which
   *  minimizes the objective function F(x) combined with the L1 norm |x|
   *  of the variables, \f$F(x) + C |x|}. \f$. This parameter is the coefficient
   *  for the |x|, i.e., C. As the L1 norm |x| is not differentiable at
   *  zero, the library modifies function and gradient evaluations from
   *  a client program suitably; a client program thus have only to return
   *  the function value F(x) and gradients G(x) as usual. The default value
   *  is zero.
   */
  void SetOrthantwiseCoefficient( PrecisionType orthant_c);
  PrecisionType GetOrthantwiseCoefficient() const;

  /**
   * Start index for computing L1 norm of the variables.
   *  This parameter is valid only for OWL-QN method
   *  (i.e., \f$ orthantwise_c != 0 \f$). This parameter b (0 <= b < N)
   *  specifies the index number from which the library computes the
   *  L1 norm of the variables x,
   *  \f[
   *      |x| := |x_{b}| + |x_{b+1}| + ... + |x_{N}| .
   *  \f]
   *  In other words, variables \f$x_1, ..., x_{b-1}\f$ are not used for
   *  computing the L1 norm. Setting \c b, (0 < b < N), one can protect
   *  variables, \f$x_1, ..., x_{b-1}\f$ (e.g., a bias term of logistic
   *  regression) from being regularized. The default value is zero.
   */
  void SetOrthantwiseStart(int start);
  int GetOrthantwiseStart() const;

  /**
   * End index for computing L1 norm of the variables.
   *  This parameter is valid only for OWL-QN method
   *  (i.e., \f$ orthantwise_c != 0 \f$). This parameter \c e, (0 < e <= N)
   *  specifies the index number at which the library stops computing the
   *  L1 norm of the variables x,
   */
  void SetOrthantwiseEnd(int end);
  int GetOrthantwiseEnd() const;

  /** Get paramater norm of current iteration */
  itkGetConstMacro(CurrentParameterNorm, PrecisionType);
  /** Get gradient norm of current iteration */
  itkGetConstMacro(CurrentGradientNorm, PrecisionType);
   //itkGetConstMacro(CurrentParameter, double* );
   //itkGetConstMacro(CurrentGradient, double* );
  /** Get stepsize of current iteration */
  itkGetConstMacro(CurrentStepSize, PrecisionType);
  /** Get number of evaluations for current iteration */
  itkGetConstMacro(CurrentNumberOfEvaluations, PrecisionType);

protected:
  LBFGS2Optimizerv4();
  virtual ~LBFGS2Optimizerv4() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;


  /** Progress callback from libLBFGS forwards it to the specific instance */
  static int UpdateProgressCallback( void *Instance,
             const PrecisionType *x,
             const PrecisionType *g,
             const PrecisionType fx,
             const PrecisionType xnorm,
             const PrecisionType gnorm,
             const PrecisionType step,
             int n,
             int k,
             int ls
           );

  /** Update the progress as reported from libLBFSG and notify itkObject */
  int UpdateProgress( const PrecisionType *x,
                      const PrecisionType *g,
                      const PrecisionType fx,
                      const PrecisionType xnorm,
                      const PrecisionType gnorm,
                      const PrecisionType step,
                      int n,
                      int k,
                      int ls
                    );

  //** Function evluation callback from libLBFGS frowrad to instance */
  static PrecisionType EvaluateCostCallback( void *instance,
                                               const PrecisionType *x,
                                               PrecisionType *g,
                                               const int n,
                                               const PrecisionType step
                                             );

  PrecisionType EvaluateCost( const PrecisionType *x,
                                PrecisionType *g,
                                const int n,
                                const PrecisionType step
                              );

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LBFGS2Optimizerv4);

  // Private Implementation (Pimpl), to hide liblbfgs data structures
  class PrivateImplementationHolder;

  AutoPointer<PrivateImplementationHolder> m_Pimpl;

  /** Progress update variables */
  const double *m_CurrentGradient;
  const double *m_CurrentParameter;

  double m_CurrentStepSize;
  double m_CurrentParameterNorm;
  double m_CurrentGradientNorm;
  int    m_CurrentNumberOfEvaluations;

  int m_StatusCode;
};

} // end namespace itk
#endif

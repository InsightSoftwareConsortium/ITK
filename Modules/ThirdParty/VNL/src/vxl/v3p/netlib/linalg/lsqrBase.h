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
#ifndef LSQR_lsqr_h
#define LSQR_lsqr_h

#include <iosfwd>

/** \class lsqrBase
 *
 *  \brief implement a solver for a set of linear equations.
 *
 *   LSQR  finds a solution x to the following problems:
 *
 *   1. Unsymmetric equations:    Solve  A*x = b
 *
 *   2. Linear least squares:     Solve  A*x = b
 *                                in the least-squares sense
 *
 *   3. Damped least squares:     Solve  (   A    )*x = ( b )
 *                                       ( damp*I )     ( 0 )
 *                                in the least-squares sense
 *
 *   where A is a matrix with m rows and n columns, b is an m-vector,
 *   and damp is a scalar.  (All quantities are real.)
 *   The matrix A is treated as a linear operator.  It is accessed
 *   by means of subroutine calls with the following purpose:
 *
 *   call Aprod1(m,n,x,y)  must compute y = y + A*x  without altering x.
 *   call Aprod2(m,n,x,y)  must compute x = x + A'*y without altering y.
 *
 *   LSQR uses an iterative method to approximate the solution.
 *   The number of iterations required to reach a certain accuracy
 *   depends strongly on the scaling of the problem.  Poor scaling of
 *   the rows or columns of A should therefore be avoided where
 *   possible.
 *
 *   For example, in problem 1 the solution is unaltered by
 *   row-scaling.  If a row of A is very small or large compared to
 *   the other rows of A, the corresponding row of ( A  b ) should be
 *   scaled up or down.
 *
 *   In problems 1 and 2, the solution x is easily recovered
 *   following column-scaling.  Unless better information is known,
 *   the nonzero columns of A should be scaled so that they all have
 *   the same Euclidean norm (e.g., 1.0).
 *
 *   In problem 3, there is no freedom to re-scale if damp is
 *   nonzero.  However, the value of damp should be assigned only
 *   after attention has been paid to the scaling of A.
 *
 *   The parameter damp is intended to help regularize
 *   ill-conditioned systems, by preventing the true solution from
 *   being very large.  Another aid to regularization is provided by
 *   the parameter Acond, which may be used to terminate iterations
 *   before the computed solution becomes very large.
 *
 *   This class is a direct C++ translation from the Fortran90 version
 *   of the solver that is available at
 *   http://www.stanford.edu/group/SOL/software.html
 *   distributed under a BSD license.
 *
 *   This class is a replacement for the lsqr code taken from netlib.
 *   That code had to be removed because it is copyrighted by ACM and
 *   its license was incompatible with a BSD license.
 *
 */
class lsqrBase
{
public:

  lsqrBase();
  virtual ~lsqrBase();

  /**
   * computes y = y + A*x without altering x,
   * where A is a matrix of dimensions A[m][n].
   * The size of the vector x is n.
   * The size of the vector y is m.
   */
  virtual void Aprod1(unsigned int m, unsigned int n, const double * x, double * y ) const = 0;

  /**
   * computes x = x + A'*y without altering y,
   * where A is a matrix of dimensions A[m][n].
   * The size of the vector x is n.
   * The size of the vector y is m.
   */
  virtual void Aprod2(unsigned int m, unsigned int n, double * x, const double * y ) const = 0;

  /**
   * returns sqrt( a**2 + b**2 )
   * with precautions to avoid overflow.
   */
  double D2Norm( double a, double b ) const;

  /**
   * returns sqrt( x' * x )
   * with precautions to avoid overflow.
   */
  double Dnrm2( unsigned int n, const double *x ) const;

  /**
   * Scale a vector by multiplying with a constant
   */
  void Scale( unsigned int n, double factor, double *x ) const;

  /**  A logical variable to say if the array se(*) of standard error estimates
   * should be computed.  If m > n  or  damp > 0,  the system is overdetermined
   * and the standard errors may be useful.  (See the first LSQR reference.)
   * Otherwise (m <= n  and  damp = 0) they do not mean much.  Some time and
   * storage can be saved by setting wantse = .false. and using any convenient
   * array for se(*), which won't be touched.  If you call this method with the
   * flag ON, then you MUST provide a working memory array to store the standard
   * error estimates, via the method SetStandardErrorEstimates()
   */
  void SetStandardErrorEstimatesFlag( bool );

  /** An estimate of the relative error in the data
   *  defining the matrix A.  For example, if A is
   *  accurate to about 6 digits, set atol = 1.0e-6.
   */
  void SetToleranceA( double );

  /** An estimate of the relative error in the data
   *  defining the rhs b.  For example, if b is
   *  accurate to about 6 digits, set btol = 1.0e-6.
   */
  void SetToleranceB( double );

  /** An upper limit on cond(Abar), the apparent
   *  condition number of the matrix Abar.
   *  Iterations will be terminated if a computed
   *  estimate of cond(Abar) exceeds conlim.
   *  This is intended to prevent certain small or
   *  zero singular values of A or Abar from
   *  coming into effect and causing unwanted growth
   *  in the computed solution.
   *
   *  conlim and damp may be used separately or
   *  together to regularize ill-conditioned systems.
   *
   *  Normally, conlim should be in the range
   *  1000 to 1/eps.
   *  Suggested value:
   *  conlim = 1/(100*eps)  for compatible systems,
   *  conlim = 1/(10*sqrt(eps)) for least squares.
   *
   * Note: Any or all of atol, btol, conlim may be set to zero.
   * The effect will be the same as the values eps, eps, 1/eps.
   *
   */
  void SetUpperLimitOnConditional( double );

  /**  the relative precision of floating-point arithmetic.
   *   On most machines, eps is about 1.0e-7 and 1.0e-16
   *   in single and double precision respectively.
   *   We expect eps to be about 1e-16 always.
   */
  void SetEpsilon( double );

  /**
   *   The damping parameter for problem 3 above.
   *   (damp should be 0.0 for problems 1 and 2.)
   *   If the system A*x = b is incompatible, values
   *   of damp in the range 0 to sqrt(eps)*norm(A)
   *   will probably have a negligible effect.
   *   Larger values of damp will tend to decrease
   *   the norm of x and reduce the number of
   *   iterations required by LSQR.
   *
   *   The work per iteration and the storage needed
   *   by LSQR are the same for all values of damp.
   *
   */
  void SetDamp( double );

  /**  An upper limit on the number of iterations.
   *   Suggested value:
   *   itnlim = n/2   for well-conditioned systems
   *                  with clustered singular values,
   *   itnlim = 4*n   otherwise.
   */
  void SetMaximumNumberOfIterations( unsigned int );

  /**
   * If provided, a summary will be printed out to this stream during
   * the execution of the Solve function.
   */
  void SetOutputStream( std::ostream & os );

  /** Provide the array where the standard error estimates will be stored.
   *  You MUST provide this working memory array if you turn on the computation
   *  of standard error estimates with teh method SetStandardErrorEstimatesFlag().
   */
  void SetStandardErrorEstimates( double * array );

  /**
   *   Returns an integer giving the reason for termination:
   *
   *     0       x = 0  is the exact solution.
   *             No iterations were performed.
   *
   *     1       The equations A*x = b are probably compatible.
   *             Norm(A*x - b) is sufficiently small, given the
   *             values of atol and btol.
   *
   *     2       damp is zero.  The system A*x = b is probably
   *             not compatible.  A least-squares solution has
   *             been obtained that is sufficiently accurate,
   *             given the value of atol.
   *
   *     3       damp is nonzero.  A damped least-squares
   *             solution has been obtained that is sufficiently
   *             accurate, given the value of atol.
   *
   *     4       An estimate of cond(Abar) has exceeded conlim.
   *             The system A*x = b appears to be ill-conditioned,
   *             or there could be an error in Aprod1 or Aprod2.
   *
   *     5       The iteration limit itnlim was reached.
   *
   */
  unsigned int GetStoppingReason() const;


  /** Returns the actual number of iterations performed. */
  unsigned int GetNumberOfIterationsPerformed() const;


  /**
   *   An estimate of the Frobenius norm of Abar.
   *   This is the square-root of the sum of squares
   *   of the elements of Abar.
   *   If damp is small and the columns of A
   *   have all been scaled to have length 1.0,
   *   Anorm should increase to roughly sqrt(n).
   *   A radically different value for Anorm may
   *   indicate an error in Aprod1 or Aprod2.
   */
  double GetFrobeniusNormEstimateOfAbar() const;


  /**
   *   An estimate of cond(Abar), the condition
   *   number of Abar.  A very high value of Acond
   *   may again indicate an error in Aprod1 or Aprod2.
   */
  double GetConditionNumberEstimateOfAbar() const;


  /** An estimate of the final value of norm(rbar),
   *  the function being minimized (see notation
   *  above).  This will be small if A*x = b has
   *  a solution.
   */
  double GetFinalEstimateOfNormRbar() const;


  /** An estimate of the final value of
   *  norm( Abar(transpose)*rbar ), the norm of
   *  the residual for the normal equations.
   *  This should be small in all cases.  (Arnorm
   *  will often be smaller than the true value
   *  computed from the output vector x.)
   */
  double GetFinalEstimateOfNormOfResiduals() const;


  /**
   * An estimate of norm(x) for the final solution x.
   */
  double GetFinalEstimateOfNormOfX() const;


  /**
   *    Execute the solver
   *
   *    solves Ax = b or min ||Ax - b|| with or without damping,
   *
   *    m is the size of the input  vector b
   *    n is the size of the output vector x
   */
  void Solve( unsigned int m, unsigned int n, const double * b, double * x );

private:

  void TerminationPrintOut();

  double Anorm;
  double Acond;
  double bnorm;
  double rnorm;
  double Arnorm;
  double xnorm;
  double dxmax;

  double atol;
  double btol;
  double conlim;

  double eps;
  double damp;
  bool   damped;

  unsigned int itnlim;
  unsigned int itn;

  unsigned int istop;

  unsigned int maxdx;

  std::ostream * nout;

  bool   wantse;
  double * se;
};

#endif

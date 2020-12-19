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
#ifndef LSQR_lsmr_h
#define LSQR_lsmr_h

#include <iosfwd>

/** \class lsmrBase
 *
 *  \brief LSMR solves Ax = b or min ||Ax - b|| with or without damping,
 * using the iterative algorithm of David Fong and Michael Saunders:
 *     http://www.stanford.edu/group/SOL/software/lsmr.html
 *
 * The original fortran code is maintained by
 *     David Fong       <clfong@stanford.edu>
 *     Michael Saunders <saunders@stanford.edu>
 *     Systems Optimization Laboratory (SOL)
 *     Stanford University
 *     Stanford, CA 94305-4026, USA
 *
 * 17 Jul 2010: F90 LSMR derived from F90 LSQR and lsqr.m.
 * 07 Sep 2010: Local reorthogonalization now works (localSize > 0).
 *
 *
 * LSMR  finds a solution x to the following problems:
 *
 * 1. Unsymmetric equations:    Solve  A*x = b
 *
 * 2. Linear least squares:     Solve  A*x = b
 *                              in the least-squares sense
 *
 * 3. Damped least squares:     Solve  (   A    )*x = ( b )
 *                                     ( damp*I )     ( 0 )
 *                              in the least-squares sense
 *
 * where A is a matrix with m rows and n columns, b is an m-vector,
 * and damp is a scalar.  (All quantities are real.)
 * The matrix A is treated as a linear operator.  It is accessed
 * by means of subroutine calls with the following purpose:
 *
 * call Aprod1(m,n,x,y)  must compute y = y + A*x  without altering x.
 * call Aprod2(m,n,x,y)  must compute x = x + A'*y without altering y.
 *
 * LSMR uses an iterative method to approximate the solution.
 * The number of iterations required to reach a certain accuracy
 * depends strongly on the scaling of the problem.  Poor scaling of
 * the rows or columns of A should therefore be avoided where
 * possible.
 *
 * For example, in problem 1 the solution is unaltered by
 * row-scaling.  If a row of A is very small or large compared to
 * the other rows of A, the corresponding row of ( A  b ) should be
 * scaled up or down.
 *
 * In problems 1 and 2, the solution x is easily recovered
 * following column-scaling.  Unless better information is known,
 * the nonzero columns of A should be scaled so that they all have
 * the same Euclidean norm (e.g., 1.0).
 *
 * In problem 3, there is no freedom to re-scale if damp is
 * nonzero.  However, the value of damp should be assigned only
 * after attention has been paid to the scaling of A.
 *
 * The parameter damp is intended to help regularize
 * ill-conditioned systems, by preventing the true solution from
 * being very large.  Another aid to regularization is provided by
 * the parameter condA, which may be used to terminate iterations
 * before the computed solution becomes very large.
 *
 * Note that x is not an input parameter.
 * If some initial estimate x0 is known and if damp = 0,
 * one could proceed as follows:
 *
 * 1. Compute a residual vector     r0 = b - A*x0.
 * 2. Use LSMR to solve the system  A*dx = r0.
 * 3. Add the correction dx to obtain a final solution x = x0 + dx.
 *
 * This requires that x0 be available before and after the call
 * to LSMR.  To judge the benefits, suppose LSMR takes k1 iterations
 * to solve A*x = b and k2 iterations to solve A*dx = r0.
 * If x0 is "good", norm(r0) will be smaller than norm(b).
 * If the same stopping tolerances atol and btol are used for each
 * system, k1 and k2 will be similar, but the final solution x0 + dx
 * should be more accurate.  The only way to reduce the total work
 * is to use a larger stopping tolerance for the second system.
 * If some value btol is suitable for A*x = b, the larger value
 * btol*norm(b)/norm(r0)  should be suitable for A*dx = r0.
 *
 * Preconditioning is another way to reduce the number of iterations.
 * If it is possible to solve a related system M*x = b efficiently,
 * where M approximates A in some helpful way
 * (e.g. M - A has low rank or its elements are small relative to
 * those of A), LSMR may converge more rapidly on the system
 *       A*M(inverse)*z = b,
 * after which x can be recovered by solving M*x = z.
 *
 * NOTE: If A is symmetric, LSMR should not be used*
 * Alternatives are the symmetric conjugate-gradient method (CG)
 * and/or SYMMLQ.
 * SYMMLQ is an implementation of symmetric CG that applies to
 * any symmetric A and will converge more rapidly than LSMR.
 * If A is positive definite, there are other implementations of
 * symmetric CG that require slightly less work per iteration
 * than SYMMLQ (but will take the same number of iterations).
 *
 *
 * Notation
 * --------
 * The following quantities are used in discussing the subroutine
 * parameters:
 *
 * Abar   =  (  A   ),        bbar  =  (b)
 *           (damp*I)                  (0)
 *
 * r      =  b - A*x,         rbar  =  bbar - Abar*x
 *
 * normr  =  sqrt( norm(r)**2  +  damp**2 * norm(x)**2 )
 *        =  norm( rbar )
 *
 * eps    =  the relative precision of floating-point arithmetic.
 *           On most machines, eps is about 1.0e-7 and 1.0e-16
 *           in single and double precision respectively.
 *           We expect eps to be about 1e-16 always.
 *
 * LSMR  minimizes the function normr with respect to x.
 *
 *
 * Parameters
 * ----------
 * m       input      m, the number of rows in A.
 *
 * n       input      n, the number of columns in A.
 *
 * Aprod1, Aprod2     See above.
 *
 * damp    input      The damping parameter for problem 3 above.
 *                    (damp should be 0.0 for problems 1 and 2.)
 *                    If the system A*x = b is incompatible, values
 *                    of damp in the range 0 to sqrt(eps)*norm(A)
 *                    will probably have a negligible effect.
 *                    Larger values of damp will tend to decrease
 *                    the norm of x and reduce the number of
 *                    iterations required by LSMR.
 *
 *                    The work per iteration and the storage needed
 *                    by LSMR are the same for all values of damp.
 *
 * b(m)    input      The rhs vector b.
 *
 * x(n)    output     Returns the computed solution x.
 *
 * atol    input      An estimate of the relative error in the data
 *                    defining the matrix A.  For example, if A is
 *                    accurate to about 6 digits, set atol = 1.0e-6.
 *
 * btol    input      An estimate of the relative error in the data
 *                    defining the rhs b.  For example, if b is
 *                    accurate to about 6 digits, set btol = 1.0e-6.
 *
 * conlim  input      An upper limit on cond(Abar), the apparent
 *                    condition number of the matrix Abar.
 *                    Iterations will be terminated if a computed
 *                    estimate of cond(Abar) exceeds conlim.
 *                    This is intended to prevent certain small or
 *                    zero singular values of A or Abar from
 *                    coming into effect and causing unwanted growth
 *                    in the computed solution.
 *
 *                    conlim and damp may be used separately or
 *                    together to regularize ill-conditioned systems.
 *
 *                    Normally, conlim should be in the range
 *                    1000 to 1/eps.
 *                    Suggested value:
 *                    conlim = 1/(100*eps)  for compatible systems,
 *                    conlim = 1/(10*sqrt(eps)) for least squares.
 *
 *         Note: Any or all of atol, btol, conlim may be set to zero.
 *         The effect will be the same as the values eps, eps, 1/eps.
 *
 * itnlim  input      An upper limit on the number of iterations.
 *                    Suggested value:
 *                    itnlim = n/2   for well-conditioned systems
 *                                   with clustered singular values,
 *                    itnlim = 4*n   otherwise.
 *
 * localSize input    No. of vectors for local reorthogonalization.
 *            0       No reorthogonalization is performed.
 *           >0       This many n-vectors "v" (the most recent ones)
 *                    are saved for reorthogonalizing the next v.
 *                    localSize need not be more than min(m,n).
 *                    At most min(m,n) vectors will be allocated.
 *
 * nout    input      File number for printed output.  If positive,
 *                    a summary will be printed on file nout.
 *
 * istop   output     An integer giving the reason for termination:
 *
 *            0       x = 0  is the exact solution.
 *                    No iterations were performed.
 *
 *            1       The equations A*x = b are probably compatible.
 *                    Norm(A*x - b) is sufficiently small, given the
 *                    values of atol and btol.
 *
 *            2       damp is zero.  The system A*x = b is probably
 *                    not compatible.  A least-squares solution has
 *                    been obtained that is sufficiently accurate,
 *                    given the value of atol.
 *
 *            3       damp is nonzero.  A damped least-squares
 *                    solution has been obtained that is sufficiently
 *                    accurate, given the value of atol.
 *
 *            4       An estimate of cond(Abar) has exceeded conlim.
 *                    The system A*x = b appears to be ill-conditioned,
 *                    or there could be an error in Aprod1 or Aprod2.
 *
 *            5       The iteration limit itnlim was reached.
 *
 * itn     output     The number of iterations performed.
 *
 * normA   output     An estimate of the Frobenius norm of Abar.
 *                    This is the square-root of the sum of squares
 *                    of the elements of Abar.
 *                    If damp is small and the columns of A
 *                    have all been scaled to have length 1.0,
 *                    normA should increase to roughly sqrt(n).
 *                    A radically different value for normA may
 *                    indicate an error in Aprod1 or Aprod2.
 *
 * condA   output     An estimate of cond(Abar), the condition
 *                    number of Abar.  A very high value of condA
 *                    may again indicate an error in Aprod1 or Aprod2.
 *
 * normr   output     An estimate of the final value of norm(rbar),
 *                    the function being minimized (see notation
 *                    above).  This will be small if A*x = b has
 *                    a solution.
 *
 * normAr  output     An estimate of the final value of
 *                    norm( Abar'*rbar ), the norm of
 *                    the residual for the normal equations.
 *                    This should be small in all cases.  (normAr
 *                    will often be smaller than the true value
 *                    computed from the output vector x.)
 *
 * normx   output     An estimate of norm(x) for the final solution x.
 *
 * Precision
 * ---------
 * The number of iterations required by LSMR will decrease
 * if the computation is performed in higher precision.
 * At least 15-digit arithmetic should normally be used.
 * "real(dp)" declarations should normally be 8-byte words.
 * If this ever changes, the BLAS routines  dnrm2, dscal
 * (Lawson, et al., 1979) will also need to be changed.
 *
 *
 * Reference
 * ---------
 * http://www.stanford.edu/group/SOL/software/lsmr.html
 * ------------------------------------------------------------------
 *
 * LSMR development:
 * 21 Sep 2007: Fortran 90 version of LSQR implemented.
 *              Aprod1, Aprod2 implemented via f90 interface.
 * 17 Jul 2010: LSMR derived from LSQR and lsmr.m.
 * 07 Sep 2010: Local reorthogonalization now working.
 *-------------------------------------------------------------------
 */
class lsmrBase
{
public:

  lsmrBase();
  virtual ~lsmrBase();

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

  /**  No. of vectors for local reorthogonalization.
   * n=0  No reorthogonalization is performed.
   * n>0  This many n-vectors "v" (the most recent ones)
   *      are saved for reorthogonalizing the next v.
   *      localSize need not be more than min(m,n).
   *      At most min(m,n) vectors will be allocated.
   */
  void SetLocalSize( unsigned int n );

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
   *   iterations required by LSMR.
   *
   *   The work per iteration and the storage needed
   *   by LSMR are the same for all values of damp.
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

  double normA;
  double condA;
  double normb;
  double normr;
  double normAr;
  double normx;
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
  unsigned int localSize;
  std::ostream * nout;
};

#endif

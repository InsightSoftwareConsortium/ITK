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
#include "lsmrBase.h"

#include <algorithm>
#include <cmath>
#include <iostream>
#include <numeric>
#include <vector>

inline void daxpy( unsigned int n, double alpha, const double * x, double * y )
{
  const double * xend = x+n;
  while ( x!=xend ) {
    *y++ += alpha * *x++;
  }
}

#define Sqr(x) ((x)*(x))

lsmrBase::lsmrBase()
{
  this->eps = 1e-16;
  this->atol = 1e-6;
  this->btol = 1e-6;
  this->conlim = 1.0 / ( 10 * sqrt( this->eps ) );
  this->itnlim = 10;
  this->nout = NULL;
  this->istop = 0;
  this->itn = 0;
  this->normA = 0.0;
  this->condA = 0.0;
  this->normr = 0.0;
  this->normAr = 0.0;
  this->normx = 0.0;
  this->normb = 0.0;
  this->dxmax = 0.0;
  this->maxdx = 0;
  this->damp = 0.0;
  this->damped = false;
  this->localSize = 0;
}


lsmrBase::~lsmrBase()
{
}


unsigned int
lsmrBase::GetStoppingReason() const
{
  return this->istop;
}


unsigned int
lsmrBase::GetNumberOfIterationsPerformed() const
{
  return this->itn;
}


double
lsmrBase::GetFrobeniusNormEstimateOfAbar() const
{
  return this->normA;
}


double
lsmrBase::GetConditionNumberEstimateOfAbar() const
{
  return this->condA;
}


double
lsmrBase::GetFinalEstimateOfNormRbar() const
{
  return this->normr;
}


double
lsmrBase::GetFinalEstimateOfNormOfResiduals() const
{
  return this->normAr;
}


double
lsmrBase::GetFinalEstimateOfNormOfX() const
{
  return this->normx;
}


void
lsmrBase::SetLocalSize( unsigned int n )
{
  this->localSize = n;
}


void
lsmrBase::SetEpsilon( double value )
{
  this->eps = value;
}


void
lsmrBase::SetDamp( double value )
{
  this->damp = value;
}


void
lsmrBase::SetToleranceA( double value )
{
  this->atol = value;
}


void
lsmrBase::SetToleranceB( double value )
{
  this->btol = value;
}


void
lsmrBase::SetMaximumNumberOfIterations( unsigned int value )
{
  this->itnlim = value;
}


void
lsmrBase::SetUpperLimitOnConditional( double value )
{
  this->conlim = value;
}


void
lsmrBase::SetOutputStream( std::ostream & os )
{
  this->nout = &os;
}


/**
 *  returns sqrt( a**2 + b**2 )
 *  with precautions to avoid overflow.
 */
double
lsmrBase::D2Norm( double a, double b ) const
{
  const double scale = std::abs(a) + std::abs(b);
  const double zero = 0.0;

  if( scale == zero )
    {
      return zero;
    }

  const double sa = a / scale;
  const double sb = b / scale;

  return scale * sqrt( sa * sa + sb * sb );
}


/** Simplified for this use from the BLAS version. */
void
lsmrBase::Scale( unsigned int n, double factor, double *x ) const
{
  double * xend = x + n;
  while( x != xend )
    {
      *x++ *= factor;
    }
}

double
lsmrBase::Dnrm2( unsigned int n, const double *x ) const
{
  double magnitudeOfLargestElement = 0.0;

  double sumOfSquaresScaled = 1.0;

  for ( unsigned int i = 0; i < n; i++ )
    {
      if ( x[i] != 0.0 )
    {
      double dx = x[i];
      const double absxi = std::abs(dx);

      if ( magnitudeOfLargestElement < absxi )
        {
          // rescale the sum to the range of the new element
          dx = magnitudeOfLargestElement / absxi;
          sumOfSquaresScaled = sumOfSquaresScaled * (dx * dx) + 1.0;
          magnitudeOfLargestElement = absxi;
        }
      else
        {
          // rescale the new element to the range of the sum
          dx = absxi / magnitudeOfLargestElement;
          sumOfSquaresScaled += dx * dx;
        }
    }
    }

  const double norm = magnitudeOfLargestElement * sqrt( sumOfSquaresScaled );

  return norm;
}

/**
 *
 *  The array b must have size m
 *
 */
void lsmrBase::
Solve( unsigned int m, unsigned int n, const double * b, double * x )
{
  const double zero = 0.0;
  const double one = 1.0;

  double test1;
  double test2;

  // Initialize.

  unsigned int localVecs = std::min( localSize, std::min( m,n ) );

  if( this->nout )
    {
    (*this->nout) << " Enter LSMR.       Least-squares solution of  Ax = b\n" << std::endl;
    (*this->nout) << " The matrix  A  has " << m << " rows   and " << n << " columns" << std::endl;
    (*this->nout) << " damp   = " << this->damp << std::endl;
    (*this->nout) << " atol   = " << this->atol << ", conlim = " << this->conlim << std::endl;
    (*this->nout) << " btol   = " << this->btol << ", itnlim = " << this->itnlim << std::endl;
    (*this->nout) << " localSize (no. of vectors for local reorthogonalization) = " << this->localSize << std::endl;
    }

  int pfreq = 20;
  int pcount = 0;
  this->damped = ( this->damp > zero );

  std::vector<double> workBuffer( m+5*n+n*localVecs );
  double * u = &workBuffer[0];
  double * v = u+m;
  double * w = v+n;
  double * h = w+n;
  double * hbar = h+n;
  double * localV = hbar+n;

  //-------------------------------------------------------------------
  //  Set up the first vectors u and v for the bidiagonalization.
  //  These satisfy  beta*u = b,  alpha*v = A(transpose)*u.
  //-------------------------------------------------------------------
  std::copy( b, b+m, u );
  std::fill( v, v+n, zero);
  std::fill( w, v+n, zero);
  std::fill( x, x+n, zero);
  this->Scale( m, (-1.0), u );
  this->Aprod1( m, n, x, u );
  this->Scale( m, (-1.0), u );

  double alpha = zero;

  double beta =  this->Dnrm2( m, u );

  if( beta > zero ) {
    this->Scale( m, ( one / beta ), u );
    this->Aprod2( m, n, v, u );   //     v = A'*u
    alpha = this->Dnrm2( n, v );
  }

  if( alpha > zero )
    {
      this->Scale( n, ( one / alpha ), v );
      std::copy( v, v+n, w );
    }

  this->normAr = alpha * beta;

  if ( this->normAr == zero )
    {
      this->TerminationPrintOut();
      return;
    }

  // Initialization for local reorthogonalization.
  bool localOrtho = false;
  bool localVQueueFull = false;
  unsigned int localPointer = 0;
  if ( localVecs > 0 ) {
    localOrtho      = true;
    std::copy( v, v+n, localV );
  }

  // Initialize variables for 1st iteration.
  this->itn = 0;
  double zetabar  = alpha*beta;
  double alphabar = alpha;
  double rho      = one;
  double rhobar   = one;
  double cbar     = one;
  double sbar     = zero;

  std::copy( v, v+n, h );
  std::fill( hbar, hbar+n, zero);

  // Initialize variables for estimation of ||r||.
  double betadd      = beta;
  double betad       = zero;
  double rhodold     = one;
  double tautildeold = zero;
  double thetatilde  = zero;
  double zeta        = zero;
  double d           = zero;

  // Initialize variables for estimation of ||A|| and cond(A).

  double normA2  = alpha*alpha;
  double maxrbar = zero;
  double minrbar = 1e+100;

  // Items for use in stopping rules.
  this->normb  = beta;
  this->istop  = 0;
  double ctol   = zero;

  if (this->conlim > zero) {
    ctol = one/this->conlim;
  }
  this->normr  = beta;

  if ( this->nout )
    {
      if ( damped )
    {
      (*this->nout) << "   Itn       x(1)           norm rbar    Abar'rbar"
        " Compatible    LS    norm Abar cond Abar\n";
    }
      else
    {
      (*this->nout) << "   Itn       x(1)            norm r         A'r   "
        " Compatible    LS      norm A    cond A\n";
    }

      test1 = one;
      test2 = alpha / beta;

      (*this->nout) << this->itn << ", " << x[0] << ", " << this->normr << ", " << this->normA << ", " << test1 << ", " << test2 << std::endl;
    }

  //  Main iteration loop
  do {
    this->itn++;

    //----------------------------------------------------------------
    //  Perform the next step of the bidiagonalization to obtain the
    //  next beta, u, alpha, v.  These satisfy
    //      beta*u = A*v  - alpha*u,
    //     alpha*v = A'*u -  beta*v.
    //----------------------------------------------------------------
    this->Scale( m, (-alpha), u );

    this->Aprod1( m, n, v, u );   //   u = A * v

    beta = this->Dnrm2( m, u );

    if ( beta > zero )
      {
    this->Scale( m, (one/beta), u );
    if ( localOrtho ) {
      if (localPointer+1 < localVecs) {
        localPointer = localPointer + 1;
      } else {
        localPointer = 0;
        localVQueueFull = true;
      }
      std::copy( v, v+n, localV+localPointer*n );
    }
    this->Scale( n, (- beta), v );
    this->Aprod2( m, n, v, u );    // v = A'*u
    if ( localOrtho ) {
      unsigned int localOrthoLimit = localVQueueFull ? localVecs : localPointer+1;

      for( unsigned int localOrthoCount =0; localOrthoCount<localOrthoLimit;
           ++localOrthoCount) {
        double d = std::inner_product(v,v+n,localV+n*localOrthoCount,0.0);
        daxpy( n, -d, localV+localOrthoCount*n, v );
      }
    }

    alpha  = this->Dnrm2( n, v );

    if ( alpha > zero )
      {
            this->Scale( n, (one/alpha), v );
      }
      }

    // At this point, beta = beta_{k+1}, alpha = alpha_{k+1}.


    //----------------------------------------------------------------
    // Construct rotation Qhat_{k,2k+1}.

    double alphahat = this->D2Norm( alphabar, damp );
    double chat     = alphabar/alphahat;
    double shat     = damp/alphahat;

    // Use a plane rotation (Q_i) to turn B_i to R_i.

    double rhoold   = rho;
    rho      = D2Norm(alphahat, beta);
    double c        = alphahat/rho;
    double s        = beta/rho;
    double thetanew = s*alpha;
    alphabar = c*alpha;

    // Use a plane rotation (Qbar_i) to turn R_i^T into R_i^bar.

    double rhobarold = rhobar;
    double zetaold   = zeta;
    double thetabar  = sbar*rho;
    double rhotemp   = cbar*rho;
    rhobar    = this->D2Norm(cbar*rho, thetanew);
    cbar      = cbar*rho/rhobar;
    sbar      = thetanew/rhobar;
    zeta      =   cbar*zetabar;
    zetabar   = - sbar*zetabar;

    // Update h, h_hat, x.

    for( unsigned int i=0;i<n;++i) {
      hbar[i] = h[i] - (thetabar*rho/(rhoold*rhobarold))*hbar[i];
      x[i] = x[i] + (zeta/(rho*rhobar))*hbar[i];
      h[i] = v[i] - (thetanew/rho)*h[i];
    }

    // Estimate ||r||.

    // Apply rotation Qhat_{k,2k+1}.
    double betaacute =   chat* betadd;
    double betacheck = - shat* betadd;

    // Apply rotation Q_{k,k+1}.
    double betahat   =   c*betaacute;
    betadd    = - s*betaacute;

    // Apply rotation Qtilde_{k-1}.
    // betad = betad_{k-1} here.

    double thetatildeold = thetatilde;
    double rhotildeold   = this->D2Norm(rhodold, thetabar);
    double ctildeold     = rhodold/rhotildeold;
    double stildeold     = thetabar/rhotildeold;
    thetatilde    = stildeold* rhobar;
    rhodold       =   ctildeold* rhobar;
    betad         = - stildeold*betad + ctildeold*betahat;

    // betad   = betad_k here.
    // rhodold = rhod_k  here.

    tautildeold   = (zetaold - thetatildeold*tautildeold)/rhotildeold;
    double taud          = (zeta - thetatilde*tautildeold)/rhodold;
    d             = d + betacheck*betacheck;
    this->normr         = sqrt(d + Sqr(betad - taud) + Sqr(betadd));

    // Estimate ||A||.
    normA2        = normA2 + Sqr(beta);
    this->normA   = sqrt(normA2);
    normA2        = normA2 + Sqr(alpha);

    // Estimate cond(A).
    maxrbar       = std::max(maxrbar,rhobarold);
    if (this->itn > 1) {
      minrbar    = std::min(minrbar,rhobarold);
    }
    this->condA   = std::max(maxrbar,rhotemp)/std::min(minrbar,rhotemp);

    //----------------------------------------------------------------
    //Test for convergence.
    //---------------------------------------------------------------

    // Compute norms for convergence testing.
    this->normAr  = std::abs(zetabar);
    this->normx   = this->Dnrm2(n, x);

    // Now use these norms to estimate certain other quantities,
    // some of which will be small near a solution.

    test1   = this->normr / this->normb;
    test2   = this->normAr/(this->normA*this->normr);
    double test3   = one/this->condA;
    double t1      = test1/(one + this->normA*this->normx/this->normb);
    double rtol    = this->btol + this->atol*this->normA*normx/this->normb;

    // The following tests guard against extremely small values of
    // atol, btol or ctol.  (The user may have set any or all of
    // the parameters atol, btol, conlim  to 0.)
    // The effect is equivalent to the normAl tests using
    // atol = eps,  btol = eps,  conlim = 1/eps.

    if ( this->itn >= this->itnlim ) this->istop = 7;
    if (one+test3 <=  one) this->istop = 6;
    if (one+test2 <=  one) this->istop = 5;
    if (one+t1    <=  one) this->istop = 4;

    // Allow for tolerances set by the user.

    if ( test3   <= ctol ) this->istop = 3;
    if ( test2   <= this->atol ) this->istop = 2;
    if ( test1   <= rtol ) this->istop = 1;

    //----------------------------------------------------------------
    // See if it is time to print something.
    //----------------------------------------------------------------
    if ( this->nout ) {
      bool prnt = false;
      if ( n<=40 ) prnt = true;
      if ( this->itn <= 10 ) prnt = true;
      if ( this->itn >= this->itnlim-10 ) prnt = true;
      if ( (this->itn % 10)  ==  0 ) prnt = true;
      if ( test3 <=  1.1*ctol ) prnt = true;
      if ( test2 <=  1.1*this->atol ) prnt = true;
      if ( test1 <=  1.1*rtol ) prnt = true;
      if ( this->istop!=0 ) prnt = true;

      if ( prnt ) { // Print a line for this iteration
    if ( pcount >= pfreq ) { // Print a heading first
      pcount = 0;
      if ( damped )
        {
          (*this->nout) << "   Itn       x(1)           norm rbar    Abar'rbar"
        " Compatible    LS    norm Abar cond Abar\n";
        } else {
        (*this->nout) << "   Itn       x(1)            norm r         A'r   "
          " Compatible    LS      norm A    cond A\n";
      }
    }
    pcount = pcount + 1;
    (*this->nout)
      << this->itn << ", " << x[0] << ", " <<this->normr << ", " << this->normAr << ", " << test1 << ", " << test2
      << ", " << this->normA << ", " << this->condA << std::endl;
      }
    }

  } while ( this->istop == 0);

  this->TerminationPrintOut();
}


void lsmrBase::
TerminationPrintOut()
{
  const char * msg[] = {
    "The exact solution is  x = 0                         ",
    "Ax - b is small enough, given atol, btol             ",
    "The least-squares solution is good enough, given atol",
    "The estimate of cond(Abar) has exceeded conlim       ",
    "Ax - b is small enough for this machine              ",
    "The LS solution is good enough for this machine      ",
    "Cond(Abar) seems to be too large for this machine    ",
    "The iteration limit has been reached                 " };

  if ( this->damped && this->istop==2 ) this->istop=3;

  if ( this->nout ) {
    (*this->nout) << " Exit  LSMR.       istop  = " << this->istop << "     ,itn    = " << this->itn << std::endl
          << " Exit  LSMR.       normA  = " << this->normA << "     ,condA  = " << this->condA << std::endl
          << " Exit  LSMR.       normb  = " << this->normb << "     ,normx  = " << this->normx << std::endl
          << " Exit  LSMR.       normr  = " << this->normr << "     ,normAr = " << this->normAr << std::endl
          << " Exit  LSMR.       " << msg[this->istop] << std::endl;
  }
}

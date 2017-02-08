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

#include "lsqrBase.h"

#include <cmath>
#include <iostream>

lsqrBase::lsqrBase()
{
  this->eps = 1e-16;
  this->atol = 1e-6;
  this->btol = 1e-6;
  this->conlim = 1.0 / ( 10 * sqrt( this->eps ) );
  this->itnlim = 10;
  this->nout = NULL;
  this->istop = 0;
  this->itn = 0;
  this->Anorm = 0.0;
  this->Acond = 0.0;
  this->rnorm = 0.0;
  this->Arnorm = 0.0;
  this->xnorm = 0.0;
  this->bnorm = 0.0;
  this->dxmax = 0.0;
  this->maxdx = 0;
  this->wantse = false;
  this->se = NULL;
  this->damp = 0.0;
  this->damped = false;
}


lsqrBase::~lsqrBase()
{
}


unsigned int
lsqrBase::GetStoppingReason() const
{
  return this->istop;
}


unsigned int
lsqrBase::GetNumberOfIterationsPerformed() const
{
  return this->itn;
}


double
lsqrBase::GetFrobeniusNormEstimateOfAbar() const
{
  return this->Anorm;
}


double
lsqrBase::GetConditionNumberEstimateOfAbar() const
{
  return this->Acond;
}


double
lsqrBase::GetFinalEstimateOfNormRbar() const
{
  return this->rnorm;
}


double
lsqrBase::GetFinalEstimateOfNormOfResiduals() const
{
  return this->Arnorm;
}


double
lsqrBase::GetFinalEstimateOfNormOfX() const
{
  return this->xnorm;
}


void
lsqrBase::SetStandardErrorEstimatesFlag(bool flag)
{
  this->wantse = flag;
}


void
lsqrBase::SetEpsilon( double value )
{
  this->eps = value;
}


void
lsqrBase::SetDamp( double value )
{
  this->damp = value;
}


void
lsqrBase::SetToleranceA( double value )
{
  this->atol = value;
}


void
lsqrBase::SetToleranceB( double value )
{
  this->btol = value;
}


void
lsqrBase::SetMaximumNumberOfIterations( unsigned int value )
{
  this->itnlim = value;
}


void
lsqrBase::SetUpperLimitOnConditional( double value )
{
  this->conlim = value;
}


void
lsqrBase::SetStandardErrorEstimates( double * array )
{
  this->se = array;
}


void
lsqrBase::SetOutputStream( std::ostream & os )
{
  this->nout = &os;
}


/**
 *  returns sqrt( a**2 + b**2 )
 *  with precautions to avoid overflow.
 */
double
lsqrBase::D2Norm( double a, double b ) const
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
lsqrBase::Scale( unsigned int n, double factor, double *x ) const
{
  double * xend = x + n;
  while( x != xend )
    {
    *x++ *= factor;
    }
}


/** Simplified for this use from the BLAS version. */
double
lsqrBase::Dnrm2( unsigned int n, const double *x ) const
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
void lsqrBase::
Solve( unsigned int m, unsigned int n, const double * b, double * x )
{
  const double zero = 0.0;
  const double one = 1.0;

  if( this->nout )
    {
    (*this->nout) << "Enter LSQR " << std::endl;
    (*this->nout) << m << ", " << n << std::endl;
    (*this->nout) << this->damp << ", " << this->wantse << std::endl;
    (*this->nout) << this->atol << ", " << this->conlim << std::endl;
    (*this->nout) << this->btol << ", " << this->itnlim << std::endl;
    }

  this->damped = ( this->damp > zero );

  this->itn = 0;
  this->istop = 0;

  unsigned int nstop = 0;
  this->maxdx = 0;

  double ctol = zero;
  if( this->conlim > zero )
    {
    ctol = one / this->conlim;
    }

  this->Anorm = zero;
  this->Acond = zero;

  double dnorm = zero;
  this->dxmax = zero;
  double res2 = zero;
  double psi = zero;

  this->xnorm = zero;

  double xnorm1 = zero;
  double cs2 = -one;
  double sn2 = zero;
  double z = zero;

  double * u = new double[m];
  double * v = new double[n];
  double * w = new double[n];

  //-------------------------------------------------------------------
  //  Set up the first vectors u and v for the bidiagonalization.
  //  These satisfy  beta*u = b,  alpha*v = A(transpose)*u.
  //-------------------------------------------------------------------
  std::copy( b, b+m, u );
  std::fill( v, v+n, zero);
  std::fill( w, w+n, zero);
  std::fill( x, x+n, zero);

  if( this->wantse )
    {
    std::fill( se, se+n, zero);
    }

  double alpha = zero;

  double beta =  this->Dnrm2( m, u );

  if( beta > zero )
    {
    this->Scale( m, ( one / beta ), u );
    this->Aprod2( m, n, v, u );   //     v = A'*u
    alpha = this->Dnrm2( n, v );
    }

  if( alpha > zero )
    {
    this->Scale( n, ( one / alpha ), v );
    std::copy( v, v+n, w );
    }

  this->Arnorm = alpha * beta;

  if ( this->Arnorm == zero )
    {
    this->TerminationPrintOut();

    // Release locally allocated arrays.
    delete [] u;
    delete [] v;
    delete [] w;

    return;
    }

  double rhobar = alpha;
  double phibar = beta;

  this->bnorm = beta;
  this->rnorm = beta;

  double test1 = 0.0;
  double test2 = 0.0;


  if ( this->nout )
    {
    if ( damped )
      {
      (*this->nout) << " Itn       x(0)           Function"\
      "     Compatible   LS     Norm Abar Cond Abar alfa_opt" << std::endl;
      }
    else
      {
      (*this->nout) << " Itn       x(0)           Function"\
      "     Compatible   LS        Norm A    Cond A" << std::endl;
      }

    test1 = one;
    test2 = alpha / beta;

    this->nout->width(6);
    (*this->nout) << this->itn;
    this->nout->precision(9);
    this->nout->width(17);
    (*this->nout) << x[0] << " ";
    this->nout->precision(2);
    this->nout->width(10);
    (*this->nout) << rnorm << " ";
    this->nout->precision(1);
    this->nout->width(9);
    (*this->nout) << test1 << " ";
    (*this->nout) << test2 << " ";
    (*this->nout) << std::endl;
    }


  double temp;
  double test3;
  double rtol;


  //
  //  Main itertation loop
  //
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

    //
    //  Accumulate Anorm = ||Bk|| = norm([alpha beta damp]).
    //
    temp   = this->D2Norm( alpha, beta );
    temp   = this->D2Norm( temp , damp );
    this->Anorm  = this->D2Norm( this->Anorm, temp );

    if ( beta > zero )
      {
      this->Scale( m, (one/beta), u );
      this->Scale( n, (- beta), v );
      this->Aprod2( m, n, v, u );    // v = A'*u

      alpha  = this->Dnrm2( n, v );

      if ( alpha > zero )
        {
        this->Scale( n, (one/alpha), v );
        }
     }

    //----------------------------------------------------------------
    //  Use a plane rotation to eliminate the damping parameter.
    //  This alters the diagonal (rhobar) of the lower-bidiagonal matrix.
    //---------------------------------------------------------------
    double rhbar1 = rhobar;

    if ( damped )
      {
      rhbar1 = this->D2Norm( rhobar, damp );
      const double cs1    = rhobar / rhbar1;
      const double sn1    = this->damp / rhbar1;
      psi    = sn1 * phibar;
      phibar = cs1 * phibar;
      }

    //----------------------------------------------------------------
    // Use a plane rotation to eliminate the subdiagonal element (beta)
    // of the lower-bidiagonal matrix, giving an upper-bidiagonal matrix.
    //----------------------------------------------------------------
    double rho    =   this->D2Norm( rhbar1, beta );
    double cs     =   rhbar1/rho;
    double sn     =   beta  /rho;
    double theta  =   sn * alpha;
    rhobar = - cs * alpha;
    double phi    =   cs * phibar;
    phibar =   sn * phibar;
    double tau    =   sn * phi;


    //----------------------------------------------------------------
    //  Update  x, w  and (perhaps) the standard error estimates.
    //---------------------------------------------------------------
    double t1     =     phi / rho;
    double t2     = - theta / rho;
    double t3     =     one / rho;
    double dknorm =    zero;

    if ( this->wantse )
      {
      for ( unsigned int i = 0; i < n; i++ )
        {
        double t = w[i];
        x[i]   = t1 * t +  x[i];
        w[i]   = t2 * t +  v[i];
        t      = ( t3 * t ) * ( t3 * t );
        se[i]  = t + se[i];
        dknorm = t + dknorm;
        }
      }
    else
      {
      for ( unsigned int i = 0; i < n; i++ )
        {
        double t = w[i];
        x[i]   = t1 * t + x[i];
        w[i]   = t2 * t + v[i];
        dknorm = ( t3 * t )*( t3 * t ) + dknorm;
        }
      }


    //----------------------------------------------------------------
    //  Monitor the norm of d_k, the update to x.
    //  dknorm = norm( d_k )
    //  dnorm  = norm( D_k ),       where   D_k = (d_1, d_2, ..., d_k )
    //  dxk    = norm( phi_k d_k ), where new x = x_k + phi_k d_k.
    //----------------------------------------------------------------
    dknorm = sqrt( dknorm );
    dnorm  = this->D2Norm( dnorm, dknorm );
    double dxk  = fabs( phi* dknorm );
    if ( this->dxmax < dxk)
      {
      this->dxmax  = dxk;
      this->maxdx  = this->itn;
      }


    //----------------------------------------------------------------
    //  Use a plane rotation on the right to eliminate the
    //  super-diagonal element (theta) of the upper-bidiagonal matrix.
    //  Then use the result to estimate  norm(x).
    //----------------------------------------------------------------
    const double delta  =   sn2 * rho;
    const double gambar = - cs2 * rho;
    const double rhs    =   phi - delta * z;
    const double zbar   =   rhs   /gambar;
    this->xnorm  =   this->D2Norm( xnorm1, zbar );
    const double gamma  =   this->D2Norm( gambar, theta );
    cs2    =   gambar / gamma;
    sn2    =   theta  / gamma;
    z      =   rhs    / gamma;
    xnorm1 =   this->D2Norm( xnorm1, z );

    //----------------------------------------------------------------
    //  Test for convergence.
    //  First, estimate the norm and condition of the matrix  Abar,
    //  and the norms of  rbar  and  Abar(transpose)*rbar.
    //----------------------------------------------------------------
    this->Acond  = this->Anorm * dnorm;
    res2   = this->D2Norm( res2, psi );
    this->rnorm  = this->D2Norm( res2, phibar );

    this->rnorm  += 1e-30;       //  Prevent rnorm == 0.0
    this->Arnorm = alpha * fabs( tau );


    // Now use these norms to estimate certain other quantities,
    // some of which will be small near a solution.

    const double alfopt = sqrt( this->rnorm / ( dnorm * this->xnorm ) );
    test1  = this->rnorm / bnorm;
    test2  = this->Arnorm / ( this->Anorm * this->rnorm );
    test3  = one   / this->Acond;
    t1     = test1 / ( one + this->Anorm* xnorm / bnorm );
    rtol   = btol  +   atol* this->Anorm* xnorm / bnorm;


    // The following tests guard against extremely small values of
    // atol, btol  or  ctol.  (The user may have set any or all of
    // the parameters  atol, btol, conlim  to zero.)
    // The effect is equivalent to the normal tests using
    // atol = eps,  btol = eps,  conlim = 1/eps.

    t3 = one + test3;
    t2 = one + test2;
    t1 = one + t1;
    if ( this->itn >= this->itnlim ) istop = 5;
    if ( t3  <= one    ) istop = 4;
    if ( t2  <= one    ) istop = 2;
    if ( t1  <= one    ) istop = 1;


    //  Allow for tolerances set by the user.

    if ( test3 <= ctol ) istop = 4;
    if ( test2 <= this->atol ) istop = 2;
    if ( test1 <= rtol ) istop = 1;


    //----------------------------------------------------------------
    // See if it is time to print something.
    //----------------------------------------------------------------
    bool prnt = false;
    if ( this->nout )
      {
      if (n     <=        40) prnt = true;
      if (this->itn   <=        10) prnt = true;
      if (this->itn   >= this->itnlim-10) prnt = true;
      if ( (this->itn % 10)  ==  0) prnt = true;
      if (test3 <=  2.0*ctol) prnt = true;
      if (test2 <= 10.0*atol) prnt = true;
      if (test1 <= 10.0*rtol) prnt = true;
      if (istop !=         0) prnt = true;

      if ( prnt ) // Print a line for this iteration.
        {
        this->nout->width(6);
        (*this->nout) << this->itn << " ";
        this->nout->precision(9);
        this->nout->width(17);
        (*this->nout) << x[0] << " ";
        this->nout->precision(2);
        this->nout->width(10);
        (*this->nout) << rnorm << " ";
        this->nout->precision(1);
        this->nout->width(9);
        (*this->nout) << test1 << " ";
        (*this->nout) << test2 << " ";
        (*this->nout) << this->Anorm << " ";
        (*this->nout) << this->Acond << " ";
        (*this->nout) << alfopt << " ";
        (*this->nout) << std::endl;
        }
      }


    //----------------------------------------------------------------
    // Stop if appropriate.
    // The convergence criteria are required to be met on  nconv
    // consecutive iterations, where  nconv  is set below.
    // Suggested value:  nconv = 1, 2  or  3.
    //----------------------------------------------------------------

    if (istop == 0)
      {
      nstop = 0;
      }
    else
      {
      const unsigned int nconv = 1;
      nstop = nstop + 1;

      if ( ( nstop < nconv ) && ( this->itn < this->itnlim ) )
        {
        istop = 0;
        }
      }

    } while ( istop == 0);

  //===================================================================
  // End of iteration loop.
  //===================================================================


  if ( this->wantse )         //  Finish off the
    {                         //  standard error estimates.
    double t = one;

    if ( m > n )
      {
      t = m - n;
      }

    if ( damped )
      {
      t = m;
      }

    t = this->rnorm / sqrt(t);

    for ( unsigned int i = 0; i < n; i++ )
      {
      se[i] = t * sqrt( se[i] );
      }
    }

  this->TerminationPrintOut();

  // Release locally allocated arrays.
  delete [] u;
  delete [] v;
  delete [] w;
}


void lsqrBase::
TerminationPrintOut()
{
  // Decide if istop = 2 or 3.
  if ( this->damped && this->istop == 2)
    {
    this->istop = 3;
    }

  if ( this->nout )
    {
    std::string exitt = " Exit LSQR. ";

    (*this->nout) << exitt.c_str();
    (*this->nout) << "istop = ";
    this->nout->width(6);
    (*this->nout) << istop;

    (*this->nout) << " itn = ";
    this->nout->width(15);
    (*this->nout) << this->itn;

    (*this->nout) << std::endl;

    (*this->nout) << exitt.c_str();
    (*this->nout) << "Anorm = ";
    this->nout->precision(5);
    this->nout->width(12);
    (*this->nout) << this->Anorm;

    (*this->nout) << "Acond = ";
    this->nout->precision(5);
    this->nout->width(12);
    (*this->nout) << this->Acond;

    (*this->nout) << std::endl;

    (*this->nout) << exitt.c_str();
    (*this->nout) << "bnorm = ";
    this->nout->precision(5);
    this->nout->width(12);
    (*this->nout) << this->bnorm;

    (*this->nout) << "xnorm = ";
    this->nout->precision(5);
    this->nout->width(12);
    (*this->nout) << this->xnorm;

    (*this->nout) << std::endl;

    (*this->nout) << exitt.c_str();
    (*this->nout) << "rnorm = ";
    this->nout->precision(5);
    this->nout->width(12);
    (*this->nout) << this->rnorm;

    (*this->nout) << "Arnorm = ";
    this->nout->precision(5);
    this->nout->width(12);
    (*this->nout) << this->Arnorm;

    (*this->nout) << std::endl;

    (*this->nout) << exitt.c_str();
    (*this->nout) << "max dx = ";
    this->nout->precision(1);
    this->nout->width(8);
    (*this->nout) << this->dxmax;

    (*this->nout) << " occurred at itn = ";
    this->nout->width(8);
    (*this->nout) << this->maxdx;
    this->nout->precision(1);
    this->nout->width(8);

    (*this->nout) << std::endl;

    (*this->nout) << exitt.c_str();
    (*this->nout) << this->dxmax / (this->xnorm+1.0e-30);

    (*this->nout) << std::endl;

    (*this->nout) << exitt.c_str();

    switch( this->istop )
      {
      case 0:
        (*this->nout) << "The exact solution is  x = 0 " << std::endl;
        break;
      case 1:
        (*this->nout) << "'A solution to Ax = b was found, given atol, btol " << std::endl;
        break;
      case 2:
        (*this->nout) << "'A least-squares solution was found, given atol " << std::endl;
        break;
      case 3:
        (*this->nout) << " 'A damped least-squares solution was found, given atol " << std::endl;
        break;
      case 4:
        (*this->nout) << " 'Cond(Abar) seems to be too large, given conlim " << std::endl;
        break;
      case 5:
        (*this->nout) << " 'The iteration limit was reached " << std::endl;
        break;
      }

    }

}

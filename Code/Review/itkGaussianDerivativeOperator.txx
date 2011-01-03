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
#ifndef __itkGaussianDerivativeOperator_txx
#define __itkGaussianDerivativeOperator_txx

#include "itkGaussianDerivativeOperator.h"
#include "itkOutputWindow.h"
#include "itkMacro.h"

namespace itk
{
template< class TPixel, unsigned int VDimension, class TAllocator >
typename GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::CoefficientVector
GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::GenerateCoefficients()
{
  CoefficientVector coeff;

  // Use image spacing to modify variance
  m_Variance /= ( m_Spacing * m_Spacing );

  // Calculate normalization factor for derivatives when necessary
  double norm = m_NormalizeAcrossScale && m_Order ? vcl_pow(m_Variance, m_Order / 2.0) : 1.0;

  if ( !this->GetUseDerivativeOperator() )
    {
    // Coefficient of the polynomial that multiplies the gaussian
    // Gaussian derivatives always take the form
    // G'(n)(x,t) = P(n)(x,t) * G(x,t)
    // where P(n)(x,sigma) is a polynomial of the same order as the derivative.
    // For first order derivatives the polynomial simply corresponds to
    // multiplying
    // the gaussian by -x/t.

    std::vector< int > polyCoeffs;

    if ( m_Order == 1 ) // -x/t
      {
      polyCoeffs.push_back(0);
      polyCoeffs.push_back(-1);
      }
    else if ( m_Order == 2 ) // ( x^2-t )/t^2
      {
      polyCoeffs.push_back(-1);
      polyCoeffs.push_back(0);
      polyCoeffs.push_back(1);
      }
    else if ( m_Order == 3 ) // (-x^3+3xt)/t^3
      {
      polyCoeffs.push_back(0);
      polyCoeffs.push_back(3);
      polyCoeffs.push_back(0);
      polyCoeffs.push_back(-1);
      }
    else if ( m_Order > 3 ) // recursively calculate derivative of polynomial
      {
      polyCoeffs.push_back(0);
      polyCoeffs.push_back(3);
      polyCoeffs.push_back(0);
      polyCoeffs.push_back(-1);

      unsigned int i, j;
      for ( i = 4; i <= m_Order; ++i )
        {
        if ( i % 2 == 0 ) // even order
          {
          for ( j = 1; j < polyCoeffs.size(); j += 2 )
            {
            polyCoeffs[j - 1] += j * polyCoeffs[j];
            if ( j < polyCoeffs.size() - 1 )
              {
              polyCoeffs[j + 1] -= polyCoeffs[j];
              }
            polyCoeffs[j] = 0;
            }
          polyCoeffs.push_back(1); // add highest order new element
          }
        else // odd order
          {
          polyCoeffs[1] = -polyCoeffs[0];
          polyCoeffs[0] = 0;
          for ( j = 2; j < polyCoeffs.size(); j += 2 )
            {
            polyCoeffs[j - 1] += j * polyCoeffs[j];
            polyCoeffs[j + 1] -= polyCoeffs[j];
            polyCoeffs[j] = 0;
            }
          polyCoeffs.push_back(-1); // add highest order new element
          }
        }
      }

    // Now create coefficients as if they were zero order coeffs

    double sum;
    int    i;
    int    j;
    typename CoefficientVector::iterator it;

    const double et           = vcl_exp(-m_Variance);
    const double cap          = 1.0 - m_MaximumError;

    // Create the kernel coefficients as a std::vector
    sum = 0.0;
    coeff.push_back( et * ModifiedBesselI0(m_Variance) );
    sum += coeff[0];
    coeff.push_back( et * ModifiedBesselI1(m_Variance) );
    sum += coeff[1] * 2.0;

    for ( i = 2; sum < cap; i++ )
      {
      coeff.push_back( et * ModifiedBesselI(i, m_Variance) );
      sum += coeff[i] * 2.0;
      if ( coeff[i] <= 0.0 )
        {
        break;                     // failsafe
        }
      if ( coeff.size() > m_MaximumKernelWidth )
        {
        itkWarningMacro("Kernel size has exceeded the specified maximum width of "
                        << m_MaximumKernelWidth << " and has been truncated to "
                        << static_cast< unsigned int >( coeff.size() ) << " elements.  You can raise "
                                                                           "the maximum width using the SetMaximumKernelWidth method.");
        break;
        }
      }

    // Normalize the coefficients so they sum one
    for ( it = coeff.begin(); it < coeff.end(); ++it )
      {
      *it /= sum;
      }

    // Make symmetric
    j = static_cast< int >( coeff.size() ) - 1;
    coeff.insert(coeff.begin(), j, 0);
    for ( i = 0, it = coeff.end() - 1; i < j; --it, ++i )
      {
      coeff[i] = *it;
      }

    if ( m_Order == 0 )
      {
      return coeff;
      }

    // Now multiply modify the coefficients taking into account the derivative
    // polynomial
    // and order
    unsigned int k;
    for ( i = -(int)coeff.size() / 2, it = coeff.begin(); i <= (int)coeff.size() / 2; ++i )
      {
      sum = 0.0;
      if ( m_Order % 2 == 0 ) // even
        {
        for ( j = 0, k = (int)m_Order / 2; j < (int)polyCoeffs.size(); j += 2, k-- )
          {
          sum += polyCoeffs[j] * vcl_pow(m_Variance, (double)k) * vcl_pow(i * m_Spacing, (double)j);
          }
        }
      else // odd
        {
        for ( j = 1, k = (int)( m_Order - 1 ) / 2; j < (int)polyCoeffs.size(); j += 2, k-- )
          {
          sum += polyCoeffs[j] * vcl_pow(m_Variance, (double)k) * vcl_pow(i * m_Spacing, (double)j);
          }
        }
      sum *= norm / vcl_pow( m_Variance, static_cast< int >( m_Order ) );
      ( *it ) *= sum;
      ++it;
      }
    }

  else // m_UseDerivativeOperator = true
    {
    GaussianOperatorType gaussOp;
    gaussOp.SetDirection( this->GetDirection() );
    gaussOp.SetMaximumKernelWidth(m_MaximumKernelWidth);
    gaussOp.SetMaximumError(m_MaximumError);
    gaussOp.SetVariance( m_Variance / ( m_Spacing * m_Spacing ) );
    gaussOp.CreateDirectional();

    DerivativeOperatorType derivOp;
    derivOp.SetDirection( this->GetDirection() );
    derivOp.SetOrder(m_Order);
    derivOp.ScaleCoefficients(1.0 / m_Spacing);
    derivOp.CreateDirectional();

    // Now perform convolution between both operators

    int    i, j, k;
    double conv;

    for ( i = 0; i < (int)gaussOp.Size(); ++i ) // current index in gaussian op
      {
      conv = 0.0;
      for ( j = 0; j < (int)derivOp.Size(); ++j ) // current index in derivative
                                                  // op
        {
        k = i + j - derivOp.Size() / 2;
        if ( k >= 0 && k < (int)gaussOp.Size() )
          {
          conv += gaussOp[k] * derivOp[derivOp.Size() - 1 - j];
          }
        }
      coeff.push_back(norm * conv);
      }
    }

  return coeff;
}

template< class TPixel, unsigned int VDimension, class TAllocator >
double
GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::ModifiedBesselI0(double y)
{
  double d, accumulator;
  double m;

  if ( ( d = vcl_fabs(y) ) < 3.75 )
    {
    m = y / 3.75;
    m *= m;
    accumulator = 1.0 + m * ( 3.5156229 + m * ( 3.0899424 + m * ( 1.2067492
                                                                  + m
                                                                  * ( 0.2659732 + m * ( 0.360768e-1 + m * 0.45813e-2 ) ) ) ) );
    }
  else
    {
    m = 3.5 / d;
    accumulator = ( vcl_exp(d) / vcl_sqrt(d) ) * ( 0.39894228 + m * ( 0.1328592e-1
                                                                      + m
                                                                      * ( 0.225319e-2 + m
                                                                          * ( -0.157565e-2 + m * ( 0.916281e-2
                                                                                                   +
                                                                                                   m
                                                                                                   * ( -0.2057706e-1
                                                                                                       + m * ( 0.2635537e-1 + m * ( -0.1647633e-1
                                                                                                                                    +
                                                                                                                                    m
                                                                                                                                    *
                                                                                                                                    0.392377e-2 ) ) ) ) ) ) ) );
    }
  return accumulator;
}

template< class TPixel, unsigned int VDimension, class TAllocator >
double
GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::ModifiedBesselI1(double y)
{
  double d, accumulator;
  double m;

  if ( ( d = vcl_fabs(y) ) < 3.75 )
    {
    m = y / 3.75;
    m *= m;
    accumulator = d * ( 0.5 + m * ( 0.87890594 + m * ( 0.51498869 + m * ( 0.15084934
                                                                          + m
                                                                          * ( 0.2658733e-1 + m
                                                                              * ( 0.301532e-2 + m * 0.32411e-3 ) ) ) ) ) );
    }
  else
    {
    m = 3.75 / d;
    accumulator = 0.2282967e-1 + m * ( -0.2895312e-1 + m * ( 0.1787654e-1
                                                             - m * 0.420059e-2 ) );
    accumulator = 0.39894228 + m * ( -0.3988024e-1 + m * ( -0.362018e-2
                                                           + m * ( 0.163801e-2 + m * ( -0.1031555e-1 + m * accumulator ) ) ) );

    accumulator *= ( vcl_exp(d) / vcl_sqrt(d) );
    }

  if ( y < 0.0 ) { return -accumulator; }
  else { return accumulator; }
}

template< class TPixel, unsigned int VDimension, class TAllocator >
double
GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::ModifiedBesselI(int n, double y)
{
  const double ACCURACY = 40.0;
  int          j;
  double       qim, qi, qip, toy;
  double       accumulator;

  if ( n < 2 )
    {
    throw ExceptionObject(__FILE__, __LINE__, "Order of modified bessel is > 2.", ITK_LOCATION);  //
                                                                                                  // placeholder
    }
  if ( y == 0.0 ) { return 0.0; }
  else
    {
    toy = 2.0 / vcl_fabs(y);
    qip = accumulator = 0.0;
    qi = 1.0;
    for ( j = 2 * ( n + (int)vcl_sqrt(ACCURACY * n) ); j > 0; j-- )
      {
      qim = qip + j * toy * qi;
      qip = qi;
      qi = qim;
      if ( vcl_fabs(qi) > 1.0e10 )
        {
        accumulator *= 1.0e-10;
        qi *= 1.0e-10;
        qip *= 1.0e-10;
        }
      if ( j == n ) { accumulator = qip; }
      }
    accumulator *= ModifiedBesselI0(y) / qi;
    if ( y < 0.0 && ( n & 1 ) ) { return -accumulator; }
    else { return accumulator; }
    }
}
} // end namespace itk

#endif

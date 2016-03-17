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
#ifndef itkGaussianDerivativeOperator_hxx
#define itkGaussianDerivativeOperator_hxx

#include "itkGaussianDerivativeOperator.h"
#include "itkCompensatedSummation.h"
#include "itkOutputWindow.h"
#include "itkMacro.h"
#include <numeric>

namespace itk
{

template< typename TPixel, unsigned int VDimension, typename TAllocator >
GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::GaussianDerivativeOperator() :
  m_NormalizeAcrossScale(true),
  m_Variance(1.0),
  m_MaximumError(0.005),
  m_MaximumKernelWidth(30),
  m_Order(1),
  m_Spacing(1.0)
{
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::GaussianDerivativeOperator(const Self & other)
  : NeighborhoodOperator< TPixel, VDimension, TAllocator >(other),
  m_NormalizeAcrossScale(other.m_NormalizeAcrossScale),
  m_Variance(other.m_Variance),
  m_MaximumError(other.m_MaximumError),
  m_MaximumKernelWidth(other.m_MaximumKernelWidth),
  m_Order(other.m_Order),
  m_Spacing(other.m_Spacing)
{
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
GaussianDerivativeOperator< TPixel, VDimension, TAllocator > &
GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::operator=(const Self & other)
{
  if(this != &other)
    {
    Superclass::operator=(other);
    m_NormalizeAcrossScale = other.m_NormalizeAcrossScale;
    m_Spacing = other.m_Spacing;
    m_Order = other.m_Order;
    m_Variance = other.m_Variance;
    m_MaximumError = other.m_MaximumError;
    m_MaximumKernelWidth = other.m_MaximumKernelWidth;
    }
  return *this;
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
typename GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::CoefficientVector
GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::GenerateCoefficients()
{

  // compute gaussian kernel of 0-order
  CoefficientVector coeff = this->GenerateGaussianCoefficients();

  if ( m_Order == 0 )
    {
    return coeff;
    }


  // Calculate scale-space normalization factor for derivatives
  double norm;
  if (m_NormalizeAcrossScale && m_Order)
    {
    norm = std::pow(m_Variance, m_Order / 2.0);
    }
  else
    {
    norm = 1.0;
    }

  // additional normalization for spacing
  norm /= std::pow( m_Spacing, static_cast< int >( m_Order ) );

  DerivativeOperatorType derivOp;
  derivOp.SetDirection( this->GetDirection() );
  derivOp.SetOrder( m_Order );
  derivOp.CreateDirectional();

  // The input gaussian kernel needs to be padded with a clamped
  // boundary condition. If N is the radius of the derivative
  // operator, then the output kernel needs to be padded by N-1. For
  // these values to be computed the input kernel needs to be padded
  // by 2N-1 on both sides.
  unsigned int N = ( derivOp.Size() - 1 ) / 2;

  // copy the gaussian operator adding clamped boundary condition
  CoefficientVector paddedCoeff( coeff.size() + 4*N - 2);

  // copy the whole gaussuan operator in coeff to paddedCoef
  // starting after the padding
  std::copy( coeff.begin(), coeff.end(), paddedCoeff.begin() + 2*N - 1);

  // padd paddedCoeff with 2*N-1 number of boundary conditions
  std::fill( paddedCoeff.begin(),  paddedCoeff.begin() + 2*N, coeff.front() );
  std::fill( paddedCoeff.rbegin(), paddedCoeff.rbegin() + 2*N, coeff.back() );

  // clear for output kernel/coeffs
  coeff = CoefficientVector();

  // Now perform convolution between derivative operators and padded gaussian
  for ( unsigned int i = N; i < paddedCoeff.size() - N; ++i )
    {
    CompensatedSummation<double> conv;

    // current index in derivative op
    for ( unsigned int j = 0; j < derivOp.Size(); ++j )
      {
      unsigned int k = i + j - derivOp.Size() / 2;
      conv += paddedCoeff[k] * derivOp[derivOp.Size() - 1 - j];
      }

    // normalize for scale-space and spacing
    coeff.push_back(norm * conv.GetSum());
    }

  return coeff;
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
typename GaussianDerivativeOperator< TPixel, VDimension, TAllocator >::CoefficientVector
GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::GenerateGaussianCoefficients() const
{

  CoefficientVector coeff;

  // Use image spacing to modify variance
  const double pixelVariance = m_Variance / ( m_Spacing * m_Spacing );

  // Now create coefficients as if they were zero order coeffs
  const double et  = std::exp(-pixelVariance);
  const double cap = 1.0 - m_MaximumError;
  CompensatedSummation<double> sum;

  // Create the kernel coefficients as a std::vector
  coeff.push_back( et * ModifiedBesselI0(pixelVariance) );
  sum += coeff[0];
  coeff.push_back( et * ModifiedBesselI1(pixelVariance) );
  sum += coeff[1] * 2.0;

  for ( int i = 2; sum.GetSum() < cap; i++ )
    {
    coeff.push_back( et * ModifiedBesselI(i, pixelVariance) );
    sum += coeff[i] * 2.0;
    if ( coeff[i] < sum.GetSum()*NumericTraits<double>::epsilon() )
      {
      // if the coeff is less then this value then the value of cap
      // will not change, and it's will not contribute to the operator
      itkWarningMacro( "Kernel failed to accumulate to approximately one with current remainder "
                       << cap-sum.GetSum() << " and current coefficient " << coeff[i] << "." );

      break;
      }
    if ( coeff.size() > m_MaximumKernelWidth )
      {
      itkWarningMacro("Kernel size has exceeded the specified maximum width of "
                      << m_MaximumKernelWidth << " and has been truncated to "
                      << static_cast< unsigned long >( coeff.size() ) << " elements.  You can raise "
                      "the maximum width using the SetMaximumKernelWidth method.");
      break;
      }
    }

  // re-accumulate from smallest number to largest for maximum precision
  sum = std::accumulate( coeff.rbegin(), coeff.rend() - 1, 0.0 );
  sum *= 2.0;
  sum += coeff[0]; // the first is only needed once

  // Normalize the coefficients so they sum one
  for ( typename CoefficientVector::iterator it = coeff.begin(); it != coeff.end(); ++it )
    {
    *it /= sum.GetSum();
    }

  // Make symmetric
  size_t s = coeff.size() - 1;
  coeff.insert(coeff.begin(), s, 0);
  std::copy( coeff.rbegin(), coeff.rbegin() + s, coeff.begin() );

  return coeff;
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
double
GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::ModifiedBesselI0(double y)
{
  double d, accumulator;
  double m;

  if ( ( d = std::fabs(y) ) < 3.75 )
    {
    m = y / 3.75;
    m *= m;
    accumulator = 1.0 + m * ( 3.5156229 + m * ( 3.0899424 + m * ( 1.2067492
                                                                  + m
                                                                  * ( 0.2659732 + m * ( 0.360768e-1 + m * 0.45813e-2 ) ) ) ) );
    }
  else
    {
    m = 3.75 / d;
    accumulator = ( std::exp(d) / std::sqrt(d) ) * ( 0.39894228 + m * ( 0.1328592e-1
                                                                      + m
                                                                      * ( 0.225319e-2 + m
                                                                          * ( -0.157565e-2 + m * ( 0.916281e-2
                                                                                                   +
                                                                                                   m
                                                                                                   * ( -0.2057706e-1
                                                                                                       + m *
                                                                                                       ( 0.2635537e-1 +
                                                                                                         m *
                                                                                                         ( -0.1647633e-1
       +
       m
       *
       0.392377e-2 ) ) ) ) ) ) ) );
    }
  return accumulator;
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
double
GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::ModifiedBesselI1(double y)
{
  double d, accumulator;
  double m;

  if ( ( d = std::fabs(y) ) < 3.75 )
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

    accumulator *= ( std::exp(d) / std::sqrt(d) );
    }

  if ( y < 0.0 ) { return -accumulator; }
  else { return accumulator; }
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
double
GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::ModifiedBesselI(int n, double y)
{
  const double DIGITS = 10.0;
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
    toy = 2.0 / std::fabs(y);
    qip = accumulator = 0.0;
    qi = 1.0;
    for ( j = 2 * ( n + (int)(DIGITS*std::sqrt((double)n) ) ); j > 0; j-- )
      {
      qim = qip + j * toy * qi;
      qip = qi;
      qi = qim;
      if ( std::fabs(qi) > 1.0e10 )
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

template< typename TPixel, unsigned int VDimension, typename TAllocator >
void
GaussianDerivativeOperator< TPixel, VDimension, TAllocator >
::PrintSelf(std::ostream & os, Indent i) const
{
  os << i << "GaussianDerivativeOperator { this=" << this
     << ", m_NormalizeAcrossScale = " << m_NormalizeAcrossScale
     << ", m_Order = " << m_Order
     << ", m_Spacing = " << m_Spacing
     << ", m_Variance = " << m_Variance
     << ", m_MaximumError = " << m_MaximumError
     << ", m_MaximumKernelWidth = " << m_MaximumKernelWidth
     << "} "  << std::endl;
  Superclass::PrintSelf( os, i.GetNextIndent() );
}

} // end namespace itk

#endif

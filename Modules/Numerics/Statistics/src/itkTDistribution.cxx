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
#include "itkTDistribution.h"
#include "itkGaussianDistribution.h"
#include "vnl/vnl_erf.h"
#include "itkMath.h"

extern "C" double dbetai_(double *x, double *pin, double *qin);

extern "C" double dgamma_(double *x);

namespace itk
{
namespace Statistics
{
TDistribution
::TDistribution()
{
  m_Parameters = ParametersType(1);
  m_Parameters[0] = 1.0;
}

void
TDistribution
::SetDegreesOfFreedom(SizeValueType dof)
{
  bool modified = false;

  if ( m_Parameters.GetSize() > 0 )
    {
    if ( Math::NotExactlyEquals(m_Parameters[0], static_cast< double >( dof )) )
      {
      modified = true;
      }
    }

  if ( m_Parameters.GetSize() != 1 )
    {
    m_Parameters = ParametersType(1);
    }

  m_Parameters[0] = static_cast< double >( dof );

  if ( modified )
    {
    this->Modified();
    }
}

SizeValueType
TDistribution
::GetDegreesOfFreedom() const
{
  if ( m_Parameters.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << m_Parameters.size()
      << " parameters.");
    }
  return static_cast< SizeValueType >( m_Parameters[0] );
}

double
TDistribution
::PDF(double x, SizeValueType degreesOfFreedom)
{
  double dof = static_cast< double >( degreesOfFreedom );
  double dofplusoneon2 = 0.5 * ( dof + 1.0 );
  double dofon2 = 0.5 * dof;
  double pdf;

  pdf = ( dgamma_(&dofplusoneon2) / dgamma_(&dofon2) )
        / ( std::sqrt(dof * itk::Math::pi) * std::pow(1.0 + ( ( x * x ) / dof ), dofplusoneon2) );

  return pdf;
}

double
TDistribution
::PDF(double x, const ParametersType & p)
{
  if ( p.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << p.size()
      << " parameters.");
    }
  return TDistribution::PDF( x, static_cast< SizeValueType >( p[0] ) );
}

double
TDistribution
::CDF(double x, SizeValueType degreesOfFreedom)
{
  double bx;
  double pin, qin;
  double dof;

  // Based on Abramowitz and Stegun 26.7.1, which gives the probability
  // that the absolute value of a random variable with a Student-t
  // distribution is less than or equal to a specified t.
  //
  // P[|x| <= t] = 1 - Ix(v/2, 1/2)
  //
  // where Ix is the incomplete beta function and v is the number of
  // degrees of freedom in the Student-t distribution and x is
  // v / (v + t^2).
  //
  // To calculate the cdf of the Student-t we need to convert
  // this formula.  For an x >= 0,
  //
  // P[|x| <= t] =   \int_{-t}^{t} p(x) dx
  //             = 2 \int_0^t p(x) dx
  //
  // The cdf of the Student-t is
  //
  // P[x <= t] = \int_{-\inf}^t p(x) dx
  //           = 0.5 + \int_0^t p(x) dx           (for x >= 0)
  //           = 0.5 + 0.5 * P[|x| < t]           (from above)
  //           = 0.5 + 0.5 * (1 - Ix(v/2. 1/2))
  //           = 1 - 0.5 * Ix(v/2, 1/2)
  //
  dof = static_cast< double >( degreesOfFreedom );
  bx = dof / ( dof + ( x * x ) );
  pin = dof / 2.0;
  qin = 0.5;

  if ( x >= 0.0 )
    {
    return 1.0 - 0.5 * dbetai_(&bx, &pin, &qin);
    }
  else
    {
    return 0.5 * dbetai_(&bx, &pin, &qin);
    }
}

double
TDistribution
::CDF(double x, const ParametersType & p)
{
  if ( p.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << p.size()
      << " parameters.");
    }
  return TDistribution::CDF( x, static_cast< SizeValueType >( p[0] ) );
}

double
TDistribution
::InverseCDF(double p, SizeValueType degreesOfFreedom)
{
  if ( p <= 0.0 )
    {
    return itk::NumericTraits< double >::NonpositiveMin();
    }
  else if ( p >= 1.0 )
    {
    return itk::NumericTraits< double >::max();
    }

  double x;
  double dof, dof2, dof3, dof4;
  double gaussX, gaussX3, gaussX5, gaussX7, gaussX9;

  // Based on Abramowitz and Stegun 26.7.5
  dof = static_cast< double >( degreesOfFreedom );
  dof2 = dof * dof;
  dof3 = dof * dof2;
  dof4 = dof * dof3;

  gaussX = GaussianDistribution::InverseCDF(p);
  gaussX3 = std::pow(gaussX, 3.0);
  gaussX5 = std::pow(gaussX, 5.0);
  gaussX7 = std::pow(gaussX, 7.0);
  gaussX9 = std::pow(gaussX, 9.0);

  x = gaussX
      + ( gaussX3 + gaussX ) / ( 4.0 * dof )
      + ( 5.0 * gaussX5 + 16.0 * gaussX3 + 3 * gaussX ) / ( 96.0 * dof2 )
      + ( 3.0 * gaussX7 + 19.0 * gaussX5 + 17.0 * gaussX3 - 15.0 * gaussX ) / ( 384.0 * dof3 )
      + ( 79.0 * gaussX9
          + 776.0 * gaussX7
          + 1482.0 * gaussX5
          - 1920.0 * gaussX3
          - 945.0 * gaussX ) / ( 92160.0 * dof4 );

  // The polynomial approximation above is only accurate for large degrees
  // of freedom.  We'll improve the approximation by a few Newton
  // iterations.
  //
  //   0 iterations, error = 1      at 1 degree of freedom
  //   3 iterations, error = 10^-10 at 1 degree of freedom
  // 100 iterations, erorr = 10^-12 at 1 degree of freedom
  //
  //   0 iterations, error = 10^-2  at 11 degrees of freedom
  //   3 iterations, error = 10^-11 at 11 degrees of freedom
  // 100 iterations, erorr = 10^-12 at 11 degrees of freedom
  //
  //
  // We are trying to find the zero of
  //
  // f(x) = p - tcdf(x) = 0;
  //
  // So,
  //
  // x(n+1) = x(n) - f(x(n)) / f'(x(n))
  //        = x(n) + (p - tcdf(x)) / tpdf(x)
  //
  // Note that f'(x) = - tpdf(x)
  //
  double delta;
  for ( unsigned int newt = 0; newt < 3; ++newt )
    {
    delta = ( p - TDistribution::CDF(x, degreesOfFreedom) )
            / TDistribution::PDF(x, degreesOfFreedom);
    x += delta;
    }

  return x;
}

double
TDistribution
::InverseCDF(double p, const ParametersType & params)
{
  if ( params.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << params.size()
      << " parameters.");
    }
  return TDistribution::InverseCDF( p, static_cast< SizeValueType >( params[0] ) );
}

double
TDistribution
::EvaluatePDF(double x) const
{
  if ( m_Parameters.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << m_Parameters.size()
      << " parameters.");
    }
  return TDistribution::PDF( x, static_cast< SizeValueType >( m_Parameters[0] ) );
}

double
TDistribution
::EvaluatePDF(double x, const ParametersType & p) const
{
  if ( p.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << p.size()
      << " parameters.");
    }
  return TDistribution::PDF( x, static_cast< SizeValueType >( p[0] ) );
}

double
TDistribution
::EvaluatePDF(double x, SizeValueType degreesOfFreedom) const
{
  return TDistribution::PDF(x, degreesOfFreedom);
}

double
TDistribution
::EvaluateCDF(double x) const
{
  if ( m_Parameters.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << m_Parameters.size()
      << " parameters.");
    }
  return TDistribution::CDF( x, static_cast< SizeValueType >( m_Parameters[0] ) );
}

double
TDistribution
::EvaluateCDF(double x, const ParametersType & p) const
{
  if ( p.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << p.size()
      << " parameters.");
    }
  return TDistribution::CDF( x, static_cast< SizeValueType >( p[0] ) );
}

double
TDistribution
::EvaluateCDF(double x, SizeValueType degreesOfFreedom) const
{
  return TDistribution::CDF(x, degreesOfFreedom);
}

double
TDistribution
::EvaluateInverseCDF(double p) const
{
  if ( m_Parameters.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << m_Parameters.size()
      << " parameters.");
    }
  return TDistribution::InverseCDF( p, static_cast< SizeValueType >( m_Parameters[0] ) );
}

double
TDistribution
::EvaluateInverseCDF(double p, const ParametersType & params) const
{
  if ( params.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << params.size()
      << " parameters.");
    }
  return TDistribution::InverseCDF( p, static_cast< SizeValueType >( params[0] ) );
}

double
TDistribution
::EvaluateInverseCDF(double p, SizeValueType degreesOfFreedom) const
{
  return TDistribution::InverseCDF(p, degreesOfFreedom);
}

bool
TDistribution
::HasVariance() const
{
  if ( m_Parameters.GetSize() == 1 )
    {
    if ( m_Parameters[0] > 2 )
      {
      return true;
      }
    }
  else
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << m_Parameters.size()
      << " parameters.");
    }

  return false;
}

double
TDistribution
::GetMean() const
{
  return 0.0;
}

double
TDistribution
::GetVariance() const
{
  if ( m_Parameters.GetSize() == 1 )
    {
    if ( m_Parameters[0] > 2 )
      {
      double dof = static_cast< double >( m_Parameters[0] );

      return dof / ( dof - 2.0 );
      }
    else
      {
      return NumericTraits< double >::quiet_NaN();
      }
    }
  else
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << m_Parameters.size()
      << " parameters.");
    }
}

void
TDistribution
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if ( m_Parameters.GetSize() > 0 )
    {
    os << indent << "Degrees of freedom: "
       << static_cast< SizeValueType >( m_Parameters[0] ) << std::endl;
    }
  else
    {
    os << indent << "Degrees of freedom: (unknown)"
       << std::endl;
    }
}
} // end of namespace Statistics
} // end namespace itk

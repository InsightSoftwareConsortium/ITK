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
#include "itkChiSquareDistribution.h"
#include "itkGaussianDistribution.h"
#include "itkMath.h"

extern "C" double dgami_(double *a, double *x);

extern "C" double dgamma_(double *x);

namespace itk
{
namespace Statistics
{
ChiSquareDistribution
::ChiSquareDistribution()
{
  m_Parameters = ParametersType(1);
  m_Parameters[0] = 1.0;
}

void
ChiSquareDistribution
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
ChiSquareDistribution
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
ChiSquareDistribution
::PDF(double x, SizeValueType degreesOfFreedom)
{
  double dof = static_cast< double >( degreesOfFreedom );
  double dofon2 = 0.5 * dof;
  double pdf = 0.0;

  if ( x >= 0.0 )
    {
    pdf = std::exp(-0.5 * x) * std::pow(x, dofon2 - 1.0)
          / ( std::pow(2.0, dofon2) * dgamma_(&dofon2) );
    }

  return pdf;
}

double
ChiSquareDistribution
::PDF(double x, const ParametersType & p)
{
  if ( p.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << p.size()
      << " parameters.");
    }
  return ChiSquareDistribution::PDF( x, static_cast< SizeValueType >( p[0] ) );
}

double
ChiSquareDistribution
::CDF(double x, SizeValueType degreesOfFreedom)
{
  // Based on Abramowitz and Stegun 26.4.19 which relates the
  // cumulative of the chi-square to incomplete (and complete) gamma
  // function.
  if ( x < 0 )
    {
    return 0.0;
    }

  double dofon2 = 0.5 * degreesOfFreedom;
  double xon2 = 0.5 * x;

  return dgami_(&dofon2, &xon2) / dgamma_(&dofon2);
}

double
ChiSquareDistribution
::CDF(double x, const ParametersType & p)
{
  if ( p.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << p.size()
      << " parameters.");
    }
  return ChiSquareDistribution::CDF(x, (SizeValueType)p[0]);
}

double
ChiSquareDistribution
::InverseCDF(double p, SizeValueType degreesOfFreedom)
{
  if ( p <= 0.0 )
    {
    return itk::NumericTraits< double >::ZeroValue();
    }
  else if ( p >= 1.0 )
    {
    return itk::NumericTraits< double >::max();
    }

  double x;
  double dof;
  double nx;

  // Based on Abramowitz and Stegun 26.4.17
  dof = static_cast< double >( degreesOfFreedom );
  nx = GaussianDistribution::InverseCDF(p);

  double f = 2.0 / ( 9.0 * dof );
  x = dof * std::pow(1.0 - f + nx * std::sqrt(f), 3.0);

  // The approximation above is only accurate for large degrees of
  // freedom. We'll improve the approximation by a few Newton iterations.
  //
  //   0 iterations, error = 10^-1  at 1 degree of freedom
  //   3 iterations, error = 10^-11 at 1 degree of freedom
  //  10 iterations, erorr = 10^-13 at 1 degree of freedom
  //  20 iterations, erorr = 10^-13 at 1 degree of freedom
  //
  //   0 iterations, error = 10^-1  at 11 degrees of freedom
  //   3 iterations, error = 10^-8  at 11 degrees of freedom
  //  10 iterations, erorr = 10^-13 at 11 degrees of freedom
  //  20 iterations, erorr = 10^-13 at 11 degrees of freedom
  //
  //   0 iterations, error = 10^-1  at 100 degrees of freedom
  //   3 iterations, error = 10^-9  at 100 degrees of freedom
  //  10 iterations, erorr = 10^-10 at 100 degrees of freedom
  //  20 iterations, erorr = 10^-9  at 100 degrees of freedom

  // We are trying to find the zero of
  //
  // f(x) = p - chisquarecdf(x) = 0;
  //
  // So,
  //
  // x(n+1) = x(n) - f(x(n)) / f'(x(n))
  //        = x(n) + (p - chisquarecdf(x)) / chisquarepdf(x)
  //
  // Note that f'(x) = - chisquarepdf(x)
  //
  double delta;
  for ( unsigned int newt = 0; newt < 10; ++newt )
    {
    delta = ( p - ChiSquareDistribution::CDF(x, degreesOfFreedom) )
            / ChiSquareDistribution::PDF(x, degreesOfFreedom);
    x += delta;
    }

  return x;
}

double
ChiSquareDistribution
::InverseCDF(double p, const ParametersType & params)
{
  if ( params.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << params.size()
      << " parameters.");
    }
  return ChiSquareDistribution::InverseCDF( p, static_cast< SizeValueType >( params[0] ) );
}

double
ChiSquareDistribution
::EvaluatePDF(double x) const
{
  if ( m_Parameters.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << m_Parameters.size()
      << " parameters.");
    }
  return ChiSquareDistribution::PDF( x, static_cast< SizeValueType >( m_Parameters[0] ) );
}

double
ChiSquareDistribution
::EvaluatePDF(double x, const ParametersType & p) const
{
  if ( p.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << p.size()
      << " parameters.");
    }
  return ChiSquareDistribution::PDF( x, static_cast< SizeValueType >( p[0] ) );
}

double
ChiSquareDistribution
::EvaluatePDF(double x, SizeValueType degreesOfFreedom) const
{
  return ChiSquareDistribution::PDF(x, degreesOfFreedom);
}

double
ChiSquareDistribution
::EvaluateCDF(double x) const
{
  if ( m_Parameters.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << m_Parameters.size()
      << " parameters.");
    }
  return ChiSquareDistribution::CDF( x, static_cast< SizeValueType >( m_Parameters[0] ) );
}

double
ChiSquareDistribution
::EvaluateCDF(double x, const ParametersType & p) const
{
  if ( p.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << p.size()
      << " parameters.");
    }
  return ChiSquareDistribution::CDF( x, static_cast< SizeValueType >( p[0] ) );
}

double
ChiSquareDistribution
::EvaluateCDF(double x, SizeValueType degreesOfFreedom) const
{
  return ChiSquareDistribution::CDF(x, degreesOfFreedom);
}

double
ChiSquareDistribution
::EvaluateInverseCDF(double p) const
{
  if ( m_Parameters.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << m_Parameters.size()
      << " parameters.");
    }
  return ChiSquareDistribution::InverseCDF( p, static_cast< SizeValueType >( m_Parameters[0] ) );
}

double
ChiSquareDistribution
::EvaluateInverseCDF(double p, const ParametersType & params) const
{
  if ( params.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << params.size()
      << " parameters.");
    }
  return ChiSquareDistribution::InverseCDF( p, static_cast< SizeValueType >( params[0] ) );
}

double
ChiSquareDistribution
::EvaluateInverseCDF(double p, SizeValueType degreesOfFreedom) const
{
  return ChiSquareDistribution::InverseCDF(p, degreesOfFreedom);
}

double
ChiSquareDistribution
::GetMean() const
{
  if ( m_Parameters.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << m_Parameters.size()
      << " parameters.");
    }
  return m_Parameters[0];
}

double
ChiSquareDistribution
::GetVariance() const
{
  if ( m_Parameters.GetSize() != 1 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 1 parameter, but got "
      << m_Parameters.size()
      << " parameters.");
    }
  return 2.0 * m_Parameters[0];
}

void
ChiSquareDistribution
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

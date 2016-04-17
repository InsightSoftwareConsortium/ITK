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
#include "itkGaussianDistribution.h"
#include "vnl/vnl_erf.h"
#include "itkMath.h"

namespace itk
{
namespace Statistics
{
GaussianDistribution
::GaussianDistribution()
{
  m_Parameters = ParametersType(2);
  m_Parameters[0] = 0.0;
  m_Parameters[1] = 1.0;
}

double
GaussianDistribution
::GetMean() const
{
  if ( m_Parameters.GetSize() != 2 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 2 parameters, but got "
      << m_Parameters.size()
      << " parameters.");
    }
  return m_Parameters[0];
}

void
GaussianDistribution
::SetMean(double mean)
{
  bool modified = false;

  if ( m_Parameters.GetSize() > 0 )
    {
    if ( Math::NotExactlyEquals(m_Parameters[0], mean) )
      {
      modified = true;
      }
    }

  if ( m_Parameters.GetSize() != 2 )
    {
    bool   cache = false;
    double t = 1.0;

    // cache current variance if there is one
    if ( m_Parameters.GetSize() > 1 )
      {
      t = m_Parameters[1];
      cache = true;
      }

    // create a parameters array of the right length
    m_Parameters = ParametersType(2);

    // reapply variance if there was one, otherwise use a default value
    if ( cache )
      {
      m_Parameters[1] = t;
      }
    else
      {
      m_Parameters[1] = 1.0;
      }

    modified = true;
    }

  m_Parameters[0] = mean;

  if ( modified )
    {
    this->Modified();
    }
}

double
GaussianDistribution
::GetVariance() const
{
  if ( m_Parameters.GetSize() != 2 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 2 parameters, but got "
      << m_Parameters.size()
      << " parameters.");
    }
  return m_Parameters[1];
}

void
GaussianDistribution
::SetVariance(double variance)
{
  bool modified = false;

  if ( m_Parameters.GetSize() > 1 )
    {
    if ( Math::NotExactlyEquals(m_Parameters[1], variance) )
      {
      modified = true;
      }
    }

  if ( m_Parameters.GetSize() != 2 )
    {
    bool   cache = false;
    double t = 0.0;

    // cache current mean if there is one
    if ( m_Parameters.GetSize() > 0 )
      {
      t = m_Parameters[0];
      cache = true;
      }

    // create a parameters array of the right length
    m_Parameters = ParametersType(2);

    // reapply mean if there was one, otherwise use a default value
    if ( cache )
      {
      m_Parameters[0] = t;
      }
    else
      {
      m_Parameters[0] = 0.0;
      }

    modified = true;
    }

  m_Parameters[1] = variance;

  if ( modified )
    {
    this->Modified();
    }
}

double
GaussianDistribution
::PDF(double x)
{
  return itk::Math::one_over_sqrt2pi * std::exp(-0.5 * x * x);
}

double
GaussianDistribution
::PDF(double x, double mean, double variance)
{
  double xminusmean = x - mean;

  return ( itk::Math::one_over_sqrt2pi / std::sqrt(variance) )
         * std::exp(-0.5 * xminusmean * xminusmean / variance);
}

double
GaussianDistribution
::PDF(double x, const ParametersType & p)
{
  // verify the parameter vector length
  if ( p.GetSize() == 2 )
    {
    return PDF(x, p[0], p[1]);
    }
  else
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 2 parameters, but got "
      << p.size()
      << " parameters.");
    }
}

double
GaussianDistribution
::CDF(double x)
{
  return 0.5 * ( vnl_erf(itk::Math::sqrt1_2 * x) + 1.0 );
}

double
GaussianDistribution
::CDF(double x, double mean, double variance)
{
  // convert to zero mean unit variance
  double u = ( x - mean ) / std::sqrt(variance);

  return 0.5 * ( vnl_erf(itk::Math::sqrt1_2 * u) + 1.0 );
}

double
GaussianDistribution
::CDF(double x, const ParametersType & p)
{
  if ( p.GetSize() != 2 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 2 parameters, but got "
      << p.size()
      << " parameters.");
    }
  return GaussianDistribution::CDF(x, p[0], p[1]);
}

double
GaussianDistribution
::InverseCDF(double p)
{
  double dp, dx, dt, ddq, dq;
  int    newt;

  dp = ( p <= 0.5 ) ? ( p ) : ( 1.0 - p );   /* make between 0 and 0.5 */

  // if original value is invalid, return +infinity or -infinity
  // changed from original code to reflect the fact that the
  // the inverse of P(x) not Q(x) is desired.
  //
  // Original line: used for inverse of Q(x)
  // if( dp <= 0.0 ){ dx = 13.0;  return ( (p <= 0.5) ? (dx) : (-dx) ); }

  // replaced with this if construct for the inverse of P(x)
  if ( p <= 0.0 )
    {
    return itk::NumericTraits< double >::NonpositiveMin();
    }
  else if ( p >= 1.0 )
    {
    return itk::NumericTraits< double >::max();
    }

  /**  Step 1:  use 26.2.23 from Abramowitz and Stegun */

  dt = std::sqrt( -2.0 * std::log(dp) );
  dx = dt
       - ( ( .010328e+0 * dt + .802853e+0 ) * dt + 2.515517e+0 )
       / ( ( ( .001308e+0 * dt + .189269e+0 ) * dt + 1.432788e+0 ) * dt + 1.e+0 );

  /**  Step 2:  do 3 Newton steps to improve this */

  for ( newt = 0; newt < 3; newt++ )
    {
    dq  = 0.5e+0 * vnl_erfc(dx * itk::Math::sqrt1_2) - dp;
    ddq = std::exp(-0.5e+0 * dx * dx) / 2.506628274631000e+0;
    dx  = dx + dq / ddq;
    }

  // original line when computing the inverse of Q(x)
  // return ( (p <= 0.5) ? (dx) : (-dx) );  /* return with correct sign */
  //
  // Note that P(-x) = Q(x), so whatever x was calculated for Q(x) = p,
  // we simply need to return the negative of x to get P(xp) = p.
  //
  dx = ( ( p <= 0.5 ) ? ( -dx ) : ( dx ) ); // return with correct sign

  return dx;
}

double
GaussianDistribution
::InverseCDF(double p, double mean, double variance)
{
  double x = GaussianDistribution::InverseCDF(p);

  if ( Math::ExactlyEquals(x, itk::NumericTraits< double >::NonpositiveMin()) )
    {
    return x;
    }
  else if ( Math::ExactlyEquals(x, itk::NumericTraits< double >::max()) )
    {
    return x;
    }
  else
    {
    // apply the mean and variance to provide the value for the
    // prescribed Gaussian
    x = x * std::sqrt(variance) + mean;
    return x;
    }
}

double
GaussianDistribution
::InverseCDF(double p, const ParametersType & params)
{
  if ( params.GetSize() != 2 )
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 2 parameters, but got "
      << params.size()
      << " parameters.");
    }
  return GaussianDistribution::InverseCDF(p, params[0], params[1]);
}

double
GaussianDistribution
::EvaluatePDF(double x) const
{
  if ( m_Parameters.GetSize() == 2 )
    {
    if ( m_Parameters[0] == 0.0 && m_Parameters[1] == 1.0 )
      {
      return GaussianDistribution::PDF(x);
      }

    return GaussianDistribution::PDF(x, m_Parameters[0], m_Parameters[1]);
    }
  else
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 2 parameters, but got "
      << m_Parameters.size()
      << " parameters.");
    }
}

double
GaussianDistribution
::EvaluatePDF(double x, const ParametersType & p) const
{
  if ( p.GetSize() == 2 )
    {
    if ( p[0] == 0.0 && p[1] == 1.0 )
      {
      return GaussianDistribution::PDF(x);
      }

    return GaussianDistribution::PDF(x, p[0], p[1]);
    }
  else
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 2 parameters, but got "
      << p.size()
      << " parameters.");
    }
}

double
GaussianDistribution
::EvaluatePDF(double x, double mean, double variance) const
{
  if ( mean == 0.0 && variance == 1.0 )
    {
    return GaussianDistribution::PDF(x);
    }

  return GaussianDistribution::PDF(x, mean, variance);
}

double
GaussianDistribution
::EvaluateCDF(double x) const
{
  if ( m_Parameters.GetSize() == 2 )
    {
    if ( m_Parameters[0] == 0.0 && m_Parameters[1] == 1.0 )
      {
      return GaussianDistribution::CDF(x);
      }

    return GaussianDistribution::CDF(x, m_Parameters[0], m_Parameters[1]);
    }
  else
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 2 parameters, but got "
      << m_Parameters.size()
      << " parameters.");
    }
}

double
GaussianDistribution
::EvaluateCDF(double x, const ParametersType & p) const
{
  if ( p.GetSize() == 2 )
    {
    if ( p[0] == 0.0 && p[1] == 1.0 )
      {
      return GaussianDistribution::CDF(x);
      }

    return GaussianDistribution::CDF(x, p[0], p[1]);
    }
  else
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 2 parameters, but got "
      << p.size()
      << " parameters.");
    }
}

double
GaussianDistribution
::EvaluateCDF(double x, double mean, double variance) const
{
  if ( mean == 0.0 && variance == 1.0 )
    {
    return GaussianDistribution::CDF(x);
    }

  return GaussianDistribution::CDF(x, mean, variance);
}

double
GaussianDistribution
::EvaluateInverseCDF(double p) const
{
  if ( m_Parameters.GetSize() == 2 )
    {
    if ( m_Parameters[0] == 0.0 && m_Parameters[1] == 1.0 )
      {
      return GaussianDistribution::InverseCDF(p);
      }

    return GaussianDistribution::InverseCDF(p, m_Parameters[0], m_Parameters[1]);
    }
  else
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 2 parameters, but got "
      << m_Parameters.size()
      << " parameters.");
    }
}

double
GaussianDistribution
::EvaluateInverseCDF(double p, const ParametersType & params) const
{
  if ( params.GetSize() == 2 )
    {
    if ( params[0] == 0.0 && params[1] == 1.0 )
      {
      return GaussianDistribution::InverseCDF(p);
      }

    return GaussianDistribution::InverseCDF(p, params[0], params[1]);
    }
  else
    {
    itkGenericExceptionMacro(
      "Invalid number of parameters to describe distribution. Expected 2 parameters, but got "
      << params.size()
      << " parameters.");
    }
}

double
GaussianDistribution
::EvaluateInverseCDF(double p, double mean, double variance) const
{
  if ( mean == 0.0 && variance == 1.0 )
    {
    return GaussianDistribution::InverseCDF(p);
    }

  return GaussianDistribution::InverseCDF(p, mean, variance);
}

void
GaussianDistribution
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if ( m_Parameters.GetSize() > 0 )
    {
    os << indent << "Mean: "  << m_Parameters[0] << std::endl;
    }
  else
    {
    os << indent << "Mean: (unknown)" << std::endl;
    }

  if ( m_Parameters.GetSize() > 1 )
    {
    os << indent << "Variance: "  << m_Parameters[1] << std::endl;
    }
  else
    {
    os << indent << "Variance: (unknown)" << std::endl;
    }
}
} // end of namespace Statistics
} // end namespace itk

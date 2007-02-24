/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianDistribution.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkGaussianDistribution.h"
#include "itkNumericTraits.h"
#include "vnl/vnl_math.h"
#include "vnl/vnl_erf.h"

namespace itk { 
namespace Statistics {

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
  if (m_Parameters.GetSize() == 2)
    {
    return m_Parameters[0];
    }
  else
    {
    InvalidArgumentError exp(__FILE__, __LINE__);
    ::itk::OStringStream message;
    message << "itk::ERROR: " << this->GetNameOfClass() 
            << "(" << this << "): "
            << "Invalid number of parameters to describe distribution.";
    exp.SetDescription(message.str());
    exp.SetLocation(ITK_LOCATION);
    throw exp;
    return 0.0;
    }
}

void
GaussianDistribution
::SetMean(double mean)
{
  bool modified=false;
  
  if (m_Parameters.GetSize() > 0)
    {
    if (m_Parameters[0] != mean)
      {
      modified = true;
      }
    }
  
  if (m_Parameters.GetSize() != 2)
    {
    bool cache = false;
    double t = 1.0;

    // cache current variance if there is one
    if (m_Parameters.GetSize() > 1)
      {
      t = m_Parameters[1];
      cache = true;
      }

    // create a parameters array of the right length
    m_Parameters = ParametersType(2);

    // reapply variance if there was one, otherwise use a default value
    if (cache)
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

  if (modified)
    {
    this->Modified();
    }
}

double
GaussianDistribution
::GetVariance() const
{
  if (m_Parameters.GetSize() == 2)
    {
    return m_Parameters[1];
    }
  else
    {
    InvalidArgumentError exp(__FILE__, __LINE__);
    ::itk::OStringStream message;
    message << "itk::ERROR: " << this->GetNameOfClass() 
            << "(" << this << "): "
            << "Invalid number of parameters to describe distribution.";
    exp.SetDescription(message.str());
    exp.SetLocation(ITK_LOCATION);
    throw exp;
    return 0.0;
    }
}

void
GaussianDistribution
::SetVariance(double variance)
{
  bool modified = false;
  
  if (m_Parameters.GetSize() > 1)
    {
    if (m_Parameters[1] != variance)
      {
      modified = true;
      }
    }

  if (m_Parameters.GetSize() != 2)
    {
    bool cache = false;
    double t = 0.0;

    // cache current mean if there is one
    if (m_Parameters.GetSize() > 0)
      {
      t = m_Parameters[0];
      cache = true;
      }

    // create a parameters array of the right length
    m_Parameters = ParametersType(2);

    // reapply mean if there was one, otherwise use a default value
    if (cache)
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

  if (modified)
    {
    this->Modified();
    }
}


double
GaussianDistribution
::PDF(double x)
{
  static const double oneonsqrttwopi = 1.0 / sqrt( 2.0 * vnl_math::pi );

  return oneonsqrttwopi * exp(-0.5*x*x);
}

double
GaussianDistribution
::PDF(double x, double mean, double variance)
{
  static const double oneonsqrttwopi = 1.0 / sqrt( 2.0 * vnl_math::pi );

  double xminusmean = x - mean;
  
  return (oneonsqrttwopi / sqrt(variance))
    * exp(-0.5*xminusmean*xminusmean / variance);
}

double
GaussianDistribution
::PDF(double x, const ParametersType& p)
{
  // verify the parameter vector length
  if (p.GetSize() == 2)
    {
    return PDF(x, p[0], p[1]);
    }
  else
    {
    InvalidArgumentError exp(__FILE__, __LINE__);
    ::itk::OStringStream message;
    message << "itk::ERROR: " 
            << "GaussianDistribution: "
            << "Invalid number of parameters to describe distribution.";
    exp.SetDescription(message.str());
    exp.SetLocation(ITK_LOCATION);
    throw exp;
    return 0.0;
    }
}

double
GaussianDistribution
::CDF(double x)
{
  return 0.5 * (vnl_erf(vnl_math::sqrt1_2 * x) + 1.0);
}


double
GaussianDistribution
::CDF(double x, double mean, double variance)
{
  // convert to zero mean unit variance
  double u = (x - mean) / sqrt(variance);
  
  return 0.5 * (vnl_erf(vnl_math::sqrt1_2 * u) + 1.0);
}


double
GaussianDistribution
::CDF(double x, const ParametersType& p)
{
  // verify the parameter vector length
  if (p.GetSize() == 2)
    {
    return GaussianDistribution::CDF(x, p[0], p[1]);
    }
  else
    {
    InvalidArgumentError exp(__FILE__, __LINE__);
    ::itk::OStringStream message;
    message << "itk::ERROR: " 
            << "GaussianDistribution: "
            << "Invalid number of parameters to describe distribution.";
    exp.SetDescription(message.str());
    exp.SetLocation(ITK_LOCATION);
    throw exp;
    return 0.0;
    }
}

double
GaussianDistribution
::InverseCDF(double p)
{
  double dp , dx , dt , ddq , dq;
  int    newt;

  dp = (p <= 0.5) ? (p) : (1.0-p);   /* make between 0 and 0.5 */
  
  // if original value is invalid, return +infinity or -infinity
  // changed from original code to reflect the fact that the
  // the inverse of P(x) not Q(x) is desired.
  //
  // Original line: used for inverse of Q(x)
  // if( dp <= 0.0 ){ dx = 13.0;  return ( (p <= 0.5) ? (dx) : (-dx) ); }
  
  // replaced with this if construct for the inverse of P(x)
  if (p <= 0.0)
    {
    return itk::NumericTraits<double>::NonpositiveMin();
    }
  else if (p >= 1.0)
    {
    return itk::NumericTraits<double>::max();
    }
  
  
  /**  Step 1:  use 26.2.23 from Abramowitz and Stegun **/
  
  dt = sqrt( -2.0 * log(dp) );
  dx = dt
    - ((.010328e+0*dt + .802853e+0)*dt + 2.515517e+0)
    /(((.001308e+0*dt + .189269e+0)*dt + 1.432788e+0)*dt + 1.e+0);
  
  /**  Step 2:  do 3 Newton steps to improve this **/
  
  for( newt=0; newt < 3; newt++ )
    {
    dq  = 0.5e+0 * vnl_erfc( dx / 1.414213562373095e+0 ) - dp;
    ddq = exp( -0.5e+0 * dx * dx ) / 2.506628274631000e+0;
    dx  = dx + dq / ddq;
    }

  // original line when computing the inverse of Q(x)
  // return ( (p <= 0.5) ? (dx) : (-dx) );  /* return with correct sign */
  //
  // Note that P(-x) = Q(x), so whatever x was calculated for Q(x) = p,
  // we simply need to return the negative of x to get P(xp) = p.
  //
  dx = ( (p <= 0.5) ? (-dx) : (dx) ); // return with correct sign

  return dx;
}


double
GaussianDistribution
::InverseCDF(double p, double mean, double variance)
{
  double x = GaussianDistribution::InverseCDF(p);

  // apply the mean and variance to provide the value for the
  // prescribed Gaussian
  x = x*sqrt(variance) + mean;
  
  return x;
}

double
GaussianDistribution
::InverseCDF(double p, const ParametersType& params)
{
  // verify the parameter vector length
  if (params.GetSize() == 2)
    {
    return GaussianDistribution::InverseCDF(p, params[0], params[1]);
    }
  else
    {
    InvalidArgumentError exp(__FILE__, __LINE__);
    ::itk::OStringStream message;
    message << "itk::ERROR: " 
            << "GaussianDistribution: "
            << "Invalid number of parameters to describe distribution.";
    exp.SetDescription(message.str());
    exp.SetLocation(ITK_LOCATION);
    throw exp;
    return 0.0;
    }
}

double
GaussianDistribution
::EvaluatePDF(double x) const
{
  if (m_Parameters.GetSize() == 2)
    {
    if (m_Parameters[0] == 0.0 && m_Parameters[1] == 1.0)
      {
      return GaussianDistribution::PDF(x);
      }
    
    return GaussianDistribution::PDF(x, m_Parameters[0], m_Parameters[1]);
    }
  else
    {
    InvalidArgumentError exp(__FILE__, __LINE__);
    ::itk::OStringStream message;
    message << "itk::ERROR: " << this->GetNameOfClass() 
            << "(" << this << "): "
            << "Invalid number of parameters to describe distribution.";
    exp.SetDescription(message.str());
    exp.SetLocation(ITK_LOCATION);
    throw exp;
    return 0.0;
    }
}

double
GaussianDistribution
::EvaluatePDF(double x, const ParametersType&p) const
{
  if (p.GetSize() == 2)
    {
    if (p[0] == 0.0 && p[1] == 1.0)
      {
      return GaussianDistribution::PDF(x);
      }
    
    return GaussianDistribution::PDF(x, p[0], p[1]);
    }
  else
    {
    InvalidArgumentError exp(__FILE__, __LINE__);
    ::itk::OStringStream message;
    message << "itk::ERROR: " << this->GetNameOfClass() 
            << "(" << this << "): "
            << "Invalid number of parameters to describe distribution.";
    exp.SetDescription(message.str());
    exp.SetLocation(ITK_LOCATION);
    throw exp;
    return 0.0;
    }
}

double
GaussianDistribution
::EvaluatePDF(double x, double mean, double variance) const
{
  if (mean == 0.0 && variance == 1.0)
    {
    return GaussianDistribution::PDF(x);
    }
    
  return GaussianDistribution::PDF(x, mean, variance);
}


double
GaussianDistribution
::EvaluateCDF(double x) const
{
  if (m_Parameters.GetSize() == 2)
    {
    if (m_Parameters[0] == 0.0 && m_Parameters[1] == 1.0)
      {
      return GaussianDistribution::CDF(x);
      }
    
    return GaussianDistribution::CDF(x, m_Parameters[0], m_Parameters[1]);
    }
  else
    {
    InvalidArgumentError exp(__FILE__, __LINE__);
    ::itk::OStringStream message;
    message << "itk::ERROR: " << this->GetNameOfClass() 
            << "(" << this << "): "
            << "Invalid number of parameters to describe distribution.";
    exp.SetDescription(message.str());
    exp.SetLocation(ITK_LOCATION);
    throw exp;
    return 0.0;
    }
}

double
GaussianDistribution
::EvaluateCDF(double x, const ParametersType& p) const
{
  if (p.GetSize() == 2)
    {
    if (p[0] == 0.0 && p[1] == 1.0)
      {
      return GaussianDistribution::CDF(x);
      }
    
    return GaussianDistribution::CDF(x, p[0], p[1]);
    }
  else
    {
    InvalidArgumentError exp(__FILE__, __LINE__);
    ::itk::OStringStream message;
    message << "itk::ERROR: " << this->GetNameOfClass() 
            << "(" << this << "): "
            << "Invalid number of parameters to describe distribution.";
    exp.SetDescription(message.str());
    exp.SetLocation(ITK_LOCATION);
    throw exp;
    return 0.0;
    }
}

double
GaussianDistribution
::EvaluateCDF(double x, double mean, double variance) const
{
  if (mean == 0.0 && variance == 1.0)
    {
    return GaussianDistribution::CDF(x);
    }
  
  return GaussianDistribution::CDF(x, mean, variance);
}


double
GaussianDistribution
::EvaluateInverseCDF(double p) const
{
  if (m_Parameters.GetSize() == 2)
    {
    if (m_Parameters[0] == 0.0 && m_Parameters[1] == 1.0)
      {
      return GaussianDistribution::InverseCDF(p);
      }

    return GaussianDistribution::InverseCDF(p,m_Parameters[0],m_Parameters[1]);
    }
  else
    {
    InvalidArgumentError exp(__FILE__, __LINE__);
    ::itk::OStringStream message;
    message << "itk::ERROR: " << this->GetNameOfClass() 
            << "(" << this << "): "
            << "Invalid number of parameters to describe distribution.";
    exp.SetDescription(message.str());
    exp.SetLocation(ITK_LOCATION);
    throw exp;
    return 0.0;
    }
}

double
GaussianDistribution
::EvaluateInverseCDF(double p, const ParametersType& params) const
{
  if (params.GetSize() == 2)
    {
    if (params[0] == 0.0 && params[1] == 1.0)
      {
      return GaussianDistribution::InverseCDF(p);
      }

    return GaussianDistribution::InverseCDF(p,params[0],params[1]);
    }
  else
    {
    InvalidArgumentError exp(__FILE__, __LINE__);
    ::itk::OStringStream message;
    message << "itk::ERROR: " << this->GetNameOfClass() 
            << "(" << this << "): "
            << "Invalid number of parameters to describe distribution.";
    exp.SetDescription(message.str());
    exp.SetLocation(ITK_LOCATION);
    throw exp;
    return 0.0;
    }
}

double
GaussianDistribution
::EvaluateInverseCDF(double p, double mean, double variance) const
{
  if (mean == 0.0 && variance == 1.0)
    {
    return GaussianDistribution::InverseCDF(p);
    }

  return GaussianDistribution::InverseCDF(p,mean,variance);
}

void  
GaussianDistribution
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  if (m_Parameters.GetSize() > 0)
    {
    os << indent << "Mean: "  << m_Parameters[0] << std::endl;
    }
  else
    {
    os << indent << "Mean: (unknown)" << std::endl;
    }

  if (m_Parameters.GetSize() > 1)
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

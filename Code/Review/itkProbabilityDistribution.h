/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProbabilityDistribution.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkProbabilityDistribution_h
#define __itkProbabilityDistribution_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkArray.h"

namespace itk{ 
namespace Statistics{

/** \class ProbabilityDistribution
 * \brief ProbabilityDistribution class defines common interface for statistical distributions (pdfs, cdfs, etc.).
 *
 * ProbabilityDistribution defines a common interface for parameteric
 * and non-parametric distributions.  ProbabilityDistribution provides
 * access to the probability density function (pdf), the cumulative
 * distribution function (cdf), and the inverse cumulative
 * distribution function.
 *
 * ProbabilityDistribution also defines an abstract interface for
 * setting parameters of distribution (mean/variance for a Gaussian,
 * degrees of freedom for Student-t, etc.).
 *
 * Note that nonparametric subclasses of ProbabilityDistribution are
 * possible.  For instance, a nonparametric implementation may use a
 * histogram or kernel density function to model the distribution.
 *
 * The EvaluatePDF(), EvaluateCDF, EvaluateInverseCDF() methods are
 * all virtual, allowing algorithms to be written with an abstract
 * interface to a distribution (with said distribution provided to the
 * algorithm at run-time).  Static methods, not requiring an instance
 * of the distribution, are also allowed.  The static methods allow
 * for optimized access to distributions when the distribution is
 * known a priori to the algorithm.
 *
 * ProbabilityDistributions are univariate.  Multivariate versions may
 * be provided under a separate superclass (since the parameters to the
 * pdf and cdf would have to be vectors not scalars). Perhaps this
 * class will be named MultivariateProbabilityDistribution.
 *
 * ProbabilityDistributions can be used for standard statistical
 * tests: Z-scores, t-tests, chi-squared tests, F-tests, etc.
 *
 * \note This work is part of the National Alliance for Medical Image
 * Computing (NAMIC), funded by the National Institutes of Health
 * through the NIH Roadmap for Medical Research, Grant U54 EB005149.
 * Information on the National Centers for Biomedical Computing
 * can be obtained from http://nihroadmap.nih.gov/bioinformatics.  
 */
class ITK_EXPORT ProbabilityDistribution :
    public Object
{
public:
  /** Standard class typedefs */
  typedef ProbabilityDistribution Self;
  typedef Object Superclass ;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Standard macros */
  itkTypeMacro(ProbabilityDistribution, Object);

  /** Type of the parameter vector. */
  typedef  Array< double >           ParametersType;

  /** Return the number of parameters that describe the
   * distribution. For nonparametric distributions, this will be a
   * function of the number of samples. */
  virtual unsigned long GetNumberOfParameters() const = 0;

  /** Get the parameters of the distribution. See concrete subclasses
   * for the order of parameters. Subclasses may provide convenience
   * methods for setting parameters, i.e. SetDegreesOfFreedom(), etc.*/
  itkGetConstReferenceMacro(Parameters, ParametersType);

  /** Set the parameters of the distribution. See concrete subclasses
   * for the order of the parameters. Subclasses may provide convenience
   * methods for setting parameters, i.e. SetDegreesOfFreedom(), etc.*/
  virtual void SetParameters(const ParametersType& params)
    {
      if (params != m_Parameters)
        {
        m_Parameters = params;
        this->Modified();
        }
    }

  /** Evaluate the probability density function (pdf). The parameters
   * of the distribution are  assigned via SetParameters().  */
  virtual double EvaluatePDF(double x) const = 0;

  /** Evaluate the probability density function (pdf). The parameters
   * for the distribution are passed as a parameters vector. See
   * concrete subclasses for the ordering of parameters. */
  virtual double EvaluatePDF(double x, const ParametersType&) const = 0;
  
  /** Evaluate the cumulative distribution function (cdf). The parameters
   * of the distribution are  assigned via SetParameters(). See
   * concrete subclasses for the ordering of parameters.  */
  virtual double EvaluateCDF(double x) const = 0;

  /** Evaluate the cumulative distribution function (cdf). The parameters
   * for the distribution are passed as a parameters vector. See
   * concrete subclasses for the ordering of parameters. */
  virtual double EvaluateCDF(double x, const ParametersType&) const = 0;
  
  /** Evaluate the inverse cumulative distribution function (inverse
   * cdf).  Parameter p must be between 0.0 and 1.0. The parameters
   * of the distribution are  assigned via SetParameters(). See
   * concrete subclasses for the ordering of parameters.  */
  virtual double EvaluateInverseCDF(double p) const = 0;

  /** Evaluate the inverse cumulative distribution function (inverse
   * cdf).  Parameter p must be between 0.0 and 1.0.  The parameters
   * for the distribution are passed as a parameters vector. See
   * concrete subclasses for the ordering of parameters. */
  virtual double EvaluateInverseCDF(double p, const ParametersType&) const = 0;
  
  /** Does this distribution have a mean? */
  virtual bool HasMean() const = 0;

  /** Does this distribution have a variance? */
  virtual bool HasVariance() const = 0;

  /** Get the mean of the distribution.  If the mean does not exist,
   * then quiet_NaN may is returned. */
  virtual double GetMean() const = 0;

  /** Get the variance of the distribution. If the variance does not
   * exist, then quiet_NaN is returned. */
  virtual double GetVariance() const = 0;
  
protected:
  ProbabilityDistribution(void) {}
  virtual ~ProbabilityDistribution(void) {}

  ParametersType m_Parameters;

private:
  ProbabilityDistribution(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

} ; // end of class

} // end of namespace Statistics
} // end namespace itk

#endif






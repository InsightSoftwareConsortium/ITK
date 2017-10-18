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
#ifndef itkGaussianDistribution_h
#define itkGaussianDistribution_h

#include "itkProbabilityDistribution.h"
#include "ITKStatisticsExport.h"

namespace itk
{
namespace Statistics
{
/** \class GaussianDistribution
 * \brief GaussianDistribution class defines the interface for a
 * univariate Gaussian distribution (pdfs, cdfs, etc.).
 *
 * GaussianDistribution provides access to the probability density
 * function (pdf), the cumulative distribution function (cdf), and the
 * inverse cumulative distribution function for a Gaussian distribution.
 *
 * The EvaluatePDF(), EvaluateCDF, EvaluateInverseCDF() methods are
 * all virtual, allowing algorithms to be written with an abstract
 * interface to a distribution (with said distribution provided to the
 * algorithm at run-time).  Static methods, not requiring an instance
 * of the distribution, are also provided.  The static methods allow
 * for optimized access to distributions when the distribution is
 * known a priori to the algorithm.
 *
 * GaussianDistributions are univariate.  Multivariate versions may
 * be provided under a separate superclass (since the parameters to the
 * pdf and cdf would have to be vectors not scalars).
 *
 * GaussianDistributions can be used for Z-score statistical tests.
 *
 * \note This work is part of the National Alliance for Medical Image
 * Computing (NAMIC), funded by the National Institutes of Health
 * through the NIH Roadmap for Medical Research, Grant U54 EB005149.
 * Information on the National Centers for Biomedical Computing
 * can be obtained from http://commonfund.nih.gov/bioinformatics.
 * \ingroup ITKStatistics
 *
 * \wiki
 * \wikiexample{Statistics/GaussianDistribution,Create a Gaussian distribution}
 * \endwiki
 */
class ITKStatistics_EXPORT GaussianDistribution:
  public ProbabilityDistribution
{
public:
  /** Standard class typedefs */
  typedef GaussianDistribution       Self;
  typedef ProbabilityDistribution    Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Strandard macros */
  itkTypeMacro(GaussianDistribution, ProbabilityDistribution);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Return the number of parameters.  For a univariate Gaussian,
   * this is 2 (mean, variance). */
  virtual SizeValueType GetNumberOfParameters() const ITK_OVERRIDE { return 2; }

  /** Evaluate the probability density function (pdf). The parameters
   * of the distribution are  assigned via SetParameters().  */
  virtual double EvaluatePDF(double x) const ITK_OVERRIDE;

  /** Evaluate the probability density function (pdf). The parameters
   * for the distribution are passed as a parameters vector. The
   * ordering of the parameters is (mean, variance). */
  virtual double EvaluatePDF(double x, const ParametersType &) const ITK_OVERRIDE;

  /** Evaluate the probability density function (pdf). The parameters
   * of the distribution are passed as separate parameters. */
  virtual double EvaluatePDF(double x, double mean, double variance) const;

  /** Evaluate the cumulative distribution function (cdf). The parameters
   * of the distribution are  assigned via SetParameters().  */
  virtual double EvaluateCDF(double x) const ITK_OVERRIDE;

  /** Evaluate the cumulative distribution function (cdf). The parameters
   * for the distribution are passed as a parameters vector. The
   * ordering of the parameters is (mean, variance). */
  virtual double EvaluateCDF(double x, const ParametersType &) const ITK_OVERRIDE;

  /** Evaluate the cumulative distribution function (cdf). The parameters
   * of the distribution are passed as separate parameters. */
  virtual double EvaluateCDF(double x, double mean, double variance) const;

  /** Evaluate the inverse cumulative distribution function (inverse
   * cdf).  Parameter p must be between 0.0 and 1.0. The parameters
   * of the distribution are  assigned via SetParameters().  */
  virtual double EvaluateInverseCDF(double p) const ITK_OVERRIDE;

  /** Evaluate the inverse cumulative distribution function (inverse
   * cdf).  Parameter p must be between 0.0 and 1.0.  The parameters
   * for the distribution are passed as a parameters vector. The
   * ordering of the parameters is (mean, variance). */
  virtual double EvaluateInverseCDF(double p, const ParametersType &) const ITK_OVERRIDE;

  /** Evaluate the inverse cumulative distribution function (inverse
   * cdf).  Parameter p must be between 0.0 and 1.0.  The parameters
   * of the distribution are passed as separate parameters. */
  virtual double EvaluateInverseCDF(double p,
                                    double mean,
                                    double variance) const;

  /** Set the mean of the Gaussian distribution. Defaults to 0.0. The
   * mean is stored in position 0 of the parameters vector. */
  virtual void SetMean(double);

  /** Get the mean of the Gaussian distribution. Defaults to 0.0. The
   * mean is stored in position 0 of the parameters vector. */
  virtual double GetMean() const ITK_OVERRIDE;

  /** Does this distribution have a mean? */
  virtual bool HasMean() const ITK_OVERRIDE { return true; }

  /** Set the variance of the Gaussian distribution.  Defaults
   * to 1.0. The variance is stored in position 1 of the parameters
   * vector.  */
  virtual void SetVariance(double);

  /** Get the variance of the Gaussian distribution. Defaults to
   * 1.0. The variance is stored in position 1 of the parameters vector. */
  virtual double GetVariance() const ITK_OVERRIDE;

  /** Does this distribution have a variance? */
  virtual bool HasVariance() const ITK_OVERRIDE { return true; }

  /** Static method to evaluate the probability density function (pdf)
   * of a standardized (mean zero, unit variance) Gaussian. The static
   * method provides optimized access without requiring an instance of
   * the class. */
  static double PDF(double x);

  /** Static method to evaluate the probability density function (pdf)
   * of a Gaussian. The parameters of the distribution are passed as a
   * parameter vector. The ordering of the parameters is (mean,
   * variance). The static method provides optimized access without
   * requiring an instance of the class. */
  static double PDF(double x, const ParametersType &);

  /** Static method to evaluate the probability density function (pdf)
   * of a Gaussian. The parameters of the distribution are passed as
   * separate values. The static method provides optimized access
   * without requiring an instance of the class. */
  static double PDF(double x, double mean, double variance);

  /** Static method to evaluate the cumulative distribution function
   * (cdf) of a standardized (mean zero, unit variance) Gaussian. The
   * static method provides optimized access without requiring an
   * instance of the class. Accuracy is approximately 10^-8. */
  static double CDF(double x);

  /** Static method to evaluate the cumulative distribution function
   * (cdf) of a Gaussian. The parameters of the distribution are passed
   * as a parameter vector. The ordering of the parameters is (mean,
   * variance). The static method provides optimized access
   * without requiring an instance of the class. */
  static double CDF(double x, const ParametersType &);

  /** Static method to evaluate the cumulative distribution function
   * (cdf) of a Gaussian. The parameters of the distribution are
   * passed as separate values. The static method provides optimized access
   * without requiring an instance of the class. */
  static double CDF(double x, double mean, double variance);

  /** Static method to evaluate the inverse cumulative distribution
   * function of a standardized (mean zero, unit variance) Gaussian.
   * The static method provides optimized access without requiring an
   * instance of the class. Parameter p must be between 0.0 and 1.0.
   *
   * THis implementation was provided by Robert W. Cox from the
   * Biophysics Research Institute at the Medical College of
   * Wisconsin.  This function is based off of a rational polynomial
   * approximation to the inverse Gaussian CDF which can be found in
   * M. Abramowitz and I.A. Stegun. Handbook of Mathematical Functions
   * with Formulas, Graphs, and Mathematical Tables.  John Wiley & Sons.
   * New York. Equation 26.2.23. pg. 933. 1972.
   *
   * Since the initial approximation only provides an estimate within
   * 4.5 E-4 of the true value, 3 Newton-Raphson interations are used
   * to refine the approximation. Accuracy is approximately 10^-8.
   *
   * Let,
   * Q(x) = (1/sqrt(2*pi)) Int_{x}^{infinity} e^{-t^2/2} dt
   * = 0.5 * erfc(x/sqrt(2))
   *
   * Given p, this function computes x such that Q(x) = p, for 0 < p < 1
   *
   * Note that the Gaussian CDF is defined as
   * P(x) = (1/sqrt(2*pi)) Int_{-infinity}{x} e^{-t^2/2} dt
   * = 1 - Q(x)
   *
   * This function has been modified to compute the inverse of P(x) instead
   * of Q(x).
   */
  static double InverseCDF(double p);

  /** Static method to evaluate the inverse cumulative distribution
   * function of a Gaussian.  The parameters of the distribution are
   * passed as a parameter vector. The ordering of the parameters is
   * (mean, variance). The static method provides optimized access
   * without requiring an instance of the class. Parameter p must be
   * between 0.0 and 1.0 */
  static double InverseCDF(double p, const ParametersType &);

  /** Static method to evaluate the inverse cumulative distribution
   * function of a Gaussian.  The parameters of the distribution are
   * passed as separate values. The static method provides optimized
   * access without requiring an instance of the class. Parameter p
   * must be between 0.0 and 1.0 */
  static double InverseCDF(double p, double mean, double variance);

protected:
  GaussianDistribution();
  virtual ~GaussianDistribution(void) ITK_OVERRIDE {}

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GaussianDistribution);
};                                    // end of class
} // end of namespace Statistics
} // end namespace itk

#endif

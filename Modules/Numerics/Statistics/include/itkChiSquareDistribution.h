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
#ifndef itkChiSquareDistribution_h
#define itkChiSquareDistribution_h

#include "itkProbabilityDistribution.h"
#include "itkNumericTraits.h"
#include "ITKStatisticsExport.h"

namespace itk
{
namespace Statistics
{
/** \class ChiSquareDistribution
 * \brief ChiSquareDistribution class defines the interface for a
 * univariate Chi-Square distribution (pdfs, cdfs, etc.).
 *
 * ChiSquareDistribution provides access to the probability density
 * function (pdf), the cumulative distribution function (cdf), and the
 * inverse cumulative distribution function for a Chi-Square distribution.
 *
 * The EvaluatePDF(), EvaluateCDF, EvaluateInverseCDF() methods are
 * all virtual, allowing algorithms to be written with an abstract
 * interface to a distribution (with said distribution provided to the
 * algorithm at run-time).  Static methods, not requiring an instance
 * of the distribution, are also provided.  The static methods allow
 * for optimized access to distributions when the distribution is
 * known a priori to the algorithm.
 *
 * ChiSquareDistributions are univariate.  Multivariate versions may
 * be provided under a separate superclass (since the parameters to the
 * pdf and cdf would have to be vectors not scalars).
 *
 * ChiSquareDistributions can be used for Chi-Square tests.
 *
 * \note This work is part of the National Alliance for Medical Image
 * Computing (NAMIC), funded by the National Institutes of Health
 * through the NIH Roadmap for Medical Research, Grant U54 EB005149.
 * Information on the National Centers for Biomedical Computing
 * can be obtained from http://commonfund.nih.gov/bioinformatics.
 * \ingroup ITKStatistics
 */
class ITKStatistics_EXPORT ChiSquareDistribution:
  public ProbabilityDistribution
{
public:
  /** Standard class typedefs */
  typedef ChiSquareDistribution      Self;
  typedef ProbabilityDistribution    Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Strandard macros */
  itkTypeMacro(ChiSquareDistribution, ProbabilityDistribution);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Return the number of parameters.  For a Chi-Square
   * distribution, the number of parameters is 1 (degrees of freedom) */
  virtual SizeValueType GetNumberOfParameters() const ITK_OVERRIDE { return 1; }

  /** Evaluate the probability density function (pdf). The parameters
   * of the distribution are  assigned via SetParameters().  */
  virtual double EvaluatePDF(double x) const ITK_OVERRIDE;

  /** Evaluate the probability density function (pdf). The parameters
   * for the distribution are passed as a parameters vector. The
   * ordering of the parameters is (degrees of freedom). */
  virtual double EvaluatePDF(double x, const ParametersType &) const ITK_OVERRIDE;

  /** Evaluate the probability density function (pdf). The parameters
   * of the distribution are passed as separate parameters. */
  virtual double EvaluatePDF(double x, SizeValueType degreesOfFreedom) const;

  /** Evaluate the cumulative distribution function (cdf). The parameters
   * of the distribution are  assigned via SetParameters().  */
  virtual double EvaluateCDF(double x) const ITK_OVERRIDE;

  /** Evaluate the cumulative distribution function (cdf). The parameters
   * for the distribution are passed as a parameters vector. The
   * ordering of the parameters is (degreesOfFreedom). */
  virtual double EvaluateCDF(double x, const ParametersType &) const ITK_OVERRIDE;

  /** Evaluate the cumulative distribution function (cdf). The parameters
   * of the distribution are passed as separate parameters. */
  virtual double EvaluateCDF(double x, SizeValueType degreesOfFreedom) const;

  /** Evaluate the inverse cumulative distribution function (inverse
   * cdf).  Parameter p must be between 0.0 and 1.0. The parameters
   * of the distribution are  assigned via SetParameters().  */
  virtual double EvaluateInverseCDF(double p) const ITK_OVERRIDE;

  /** Evaluate the inverse cumulative distribution function (inverse
   * cdf).  Parameter p must be between 0.0 and 1.0.  The parameters
   * for the distribution are passed as a parameters vector. The
   * ordering of the parameters is (degrees of freedom). */
  virtual double EvaluateInverseCDF(double p, const ParametersType &) const ITK_OVERRIDE;

  /** Evaluate the inverse cumulative distribution function (inverse
   * cdf).  Parameter p must be between 0.0 and 1.0.  The parameters
   * of the distribution are passed as separate parameters. */
  virtual double EvaluateInverseCDF(double p, SizeValueType degreesOfFreedom) const;

  /** Set the number of degrees of freedom in the Chi-Square distribution.
   * Defaults to 1 */
  virtual void SetDegreesOfFreedom(SizeValueType);

  /** Get the number of degrees of freedom in the t
   * distribution. Defaults to 1 */
  virtual SizeValueType GetDegreesOfFreedom() const;

  /** Does the Chi-Square distribution have a mean? */
  virtual bool HasMean() const ITK_OVERRIDE { return true; }

  /** Get the mean of the distribution. */
  virtual double GetMean() const ITK_OVERRIDE;

  /** Does the Chi-Square distribution have a variance? */
  virtual bool HasVariance() const ITK_OVERRIDE { return true; }

  /** Get the variance of the distribution. */
  virtual double GetVariance() const ITK_OVERRIDE;

  /** Static method to evaluate the probability density function (pdf)
   * of a Chi-Square with a specified number of degrees of freedom. The
   * static method provides optimized access without requiring an
   * instance of the class. The degrees of freedom for the
   * distribution are passed in a parameters vector. */
  static double PDF(double x, const ParametersType &);

  /** Static method to evaluate the probability density function (pdf)
   * of a Chi-Square with a specified number of degrees of freedom. The
   * static method provides optimized access without requiring an
   * instance of the class. */
  static double PDF(double x, SizeValueType degreesOfFreedom);

  /** Static method to evaluate the cumulative distribution function
   * (cdf) of a Chi-Square with a specified number of degrees of
   * freedom. The static method provides optimized access without
   * requiring an instance of the class. The degrees of freedom are
   * passed as a parameters vector.
   *
   * This is based on Abramowitz and Stegun 26.7.1. Accuracy is
   * approximately 10^-14.
   */
  static double CDF(double x, const ParametersType &);

  /** Static method to evaluate the cumulative distribution function
   * (cdf) of a Chi-Square with a specified number of degrees of
   * freedom. The static method provides optimized access without
   * requiring an instance of the class.
   *
   * This is based on Abramowitz and Stegun 26.7.1. Accuracy is
   * approximately 10^-14.
   */
  static double CDF(double x, SizeValueType degreesOfFreedom);

  /** Static method to evaluate the inverse cumulative distribution
   * function of a Chi-Square with a specified number of degrees of
   * freedom.  The static method provides optimized access without
   * requiring an instance of the class. Parameter p must be between
   * 0.0 and 1.0. The degrees of freedom are passed as a parameters vector.
   *
   * This is based on Abramowitz and Stegun 26.7.5 followed by a few
   * Newton iterations to improve the precision at low degrees of
   * freedom. Accuracy is approximately 10^-10.
   **/
  static double InverseCDF(double p, const ParametersType &);

  /** Static method to evaluate the inverse cumulative distribution
   * function of a Chi-Square with a specified number of degrees of
   * freedom.  The static method provides optimized access without
   * requiring an instance of the class. Parameter p must be between
   * 0.0 and 1.0.
   *
   * This is based on Abramowitz and Stegun 26.7.5 followed by a few
   * Newton iterations to improve the precision at low degrees of
   * freedom. Accuracy is approximately 10^-10.
   **/
  static double InverseCDF(double p, SizeValueType degreesOfFreedom);

protected:
  ChiSquareDistribution();
  virtual ~ChiSquareDistribution() ITK_OVERRIDE {}

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ChiSquareDistribution);
};                                     // end of class
} // end of namespace Statistics
} // end namespace itk

#endif

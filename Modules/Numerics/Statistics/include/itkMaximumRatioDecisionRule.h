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
#ifndef itkMaximumRatioDecisionRule_h
#define itkMaximumRatioDecisionRule_h

#include <vector>
#include "vnl/vnl_matrix.h"

#include "itkNumericTraits.h"
#include "itkDecisionRule.h"
#include "ITKStatisticsExport.h"

namespace itk
{
namespace Statistics
{
/** \class MaximumRatioDecisionRule
 *  \brief A decision rule that operates as a frequentist's
 *  approximation to Bayes rule.
 *
 * MaximumRatioDecisionRule returns the class label using a Bayesian
 * style decision rule. The discriminant scores are evaluated in the
 * context of class priors. If the discriminant scores are actual
 * conditional probabilites (likelihoods) and the class priors are
 * actual a priori class probabilities, then this decision rule operates
 * as Bayes rule, returning the class \f$i\f$ if
 * \f$p(x|i) p(i) > p(x|j) p(j)\f$ for all class \f$j\f$. The
 * discriminant scores and priors are not required to be true
 * probabilities.
 *
 * This class is named the MaximumRatioDecisionRule as it can be
 * implemented as returning the class \f$i\f$ if
 * \f$\frac{p(x|i)}{p(x|j)} > \frac{p(j)}{p(i)}\f$ for all class
 * \f$j\f$.
 *
 * A priori values need to be set before calling the Evaluate
 * method. If they are not set, a uniform prior is assumed.
 *
 * \sa MaximumDecisionRule, MinimumDecisionRule
 * \ingroup ITKStatistics
 */

class ITKStatistics_EXPORT MaximumRatioDecisionRule : public DecisionRule
{
public:
  /** Standard class typedefs */
  typedef MaximumRatioDecisionRule  Self;
  typedef DecisionRule              Superclass;
  typedef SmartPointer< Self >      Pointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(MaximumRatioDecisionRule, DecisionRule);

  /** Standard New() method support */
  itkNewMacro(Self);

  /** Types for discriminant values and vectors. */
  typedef Superclass::MembershipValueType  MembershipValueType;
  typedef Superclass::MembershipVectorType MembershipVectorType;

  /** Types for class identifiers. */
  typedef Superclass::ClassIdentifierType ClassIdentifierType;

  /** Types for priors and values */
  typedef MembershipValueType                      PriorProbabilityValueType;
  typedef std::vector< PriorProbabilityValueType > PriorProbabilityVectorType;
  typedef PriorProbabilityVectorType::size_type    PriorProbabilityVectorSizeType;

  /**
   * Evaluate the decision rule \f$p(x|i) p(i) > p(x|j) p(j)\f$. Prior
   * probabilities need to be set before calling Evaluate() using the
   * SetPriorProbabilities() method (otherwise a uniform prior is
   * assumed). Parameter to Evaluate() is the discriminant score in
   * the form of a likelihood \f$p(x|i)\f$.
   */
  virtual ClassIdentifierType Evaluate(const MembershipVectorType & discriminantScores) const ITK_OVERRIDE;

  /** Set the prior probabilities used in evaluating
   * \f$p(x|i) p(i) > p(x|j) p(j)\f$. The likelihoods are set using
   * the Evaluate() method. SetPriorProbabilities needs to be called before
   * Evaluate(). If not set, assumes a uniform prior.  */
  void SetPriorProbabilities(const PriorProbabilityVectorType& p);

  /** Get the prior probabilities. */
  itkGetConstReferenceMacro(PriorProbabilities, PriorProbabilityVectorType);

protected:
  MaximumRatioDecisionRule();
  virtual ~MaximumRatioDecisionRule() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MaximumRatioDecisionRule);

  PriorProbabilityVectorType m_PriorProbabilities;

};  // end of class
} // end of Statistics namespace
} // end of ITK namespace
#endif

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
#ifndef itkMaximumDecisionRule_h
#define itkMaximumDecisionRule_h

#include "itkDecisionRule.h"
#include "ITKStatisticsExport.h"

namespace itk
{
namespace Statistics
{
/** \class MaximumDecisionRule
 *  \brief A decision rule that returns the class label with the
 *  largest discriminant score.
 *
 * MaximumDecisionRule returns the class label with the largest
 * discriminant score. If the discriminant scores are likelihood
 * \f$p(x|i)\f$, then this decision rule is a maximum likelihood
 * decision rule.
 *
 * \ingroup ITKStatistics
 */

class ITKStatistics_EXPORT MaximumDecisionRule:public DecisionRule
{
public:
  /** Standard class typedefs */
  typedef MaximumDecisionRule        Self;
  typedef DecisionRule               Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(MaximumDecisionRule, DecisionRule);

  /** Standard New() method support */
  itkNewMacro(Self);

  /** Types for discriminant values and vectors. */
  typedef Superclass::MembershipValueType  MembershipValueType;
  typedef Superclass::MembershipVectorType MembershipVectorType;

  /** Types for class identifiers. */
  typedef Superclass::ClassIdentifierType ClassIdentifierType;

  /**
   * Evaluate the decision rule, returning the class label associated
   * with the largest discriminant score.
   */
  virtual ClassIdentifierType Evaluate(const MembershipVectorType & discriminantScores) const ITK_OVERRIDE;

protected:
  MaximumDecisionRule() {}
  virtual ~MaximumDecisionRule() ITK_OVERRIDE {}

};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#endif

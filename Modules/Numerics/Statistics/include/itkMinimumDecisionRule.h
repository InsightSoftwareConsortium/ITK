/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkMinimumDecisionRule_h
#define itkMinimumDecisionRule_h

#include "itkDecisionRule.h"
#include "ITKStatisticsExport.h"

namespace itk
{
namespace Statistics
{
/** \class MinimumDecisionRule
 *  \brief A decision rule that returns the class label with the
 *  smallest discriminant score.
 *
 * MinimumDecisionRule returns the class label with the smallest
 * discriminant score.
 *
 * \ingroup ITKStatistics
 */

class ITKStatistics_EXPORT MinimumDecisionRule : public DecisionRule
{
public:
  /** Standard class type aliases */
  using Self = MinimumDecisionRule;
  using Superclass = DecisionRule;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods) */
  itkTypeMacro(MinimumDecisionRule, DecisionRule);

  /** Standard New() method support */
  itkNewMacro(Self);

  /** Types for discriminant values and vectors. */
  using MembershipValueType = Superclass::MembershipValueType;
  using MembershipVectorType = Superclass::MembershipVectorType;

  /** Types for class identifiers. */
  using ClassIdentifierType = Superclass::ClassIdentifierType;

  /**
   * Evaluate the decision rule, returning the class label associated
   * with the smallest discriminant score.
   */
  ClassIdentifierType
  Evaluate(const MembershipVectorType & discriminantScores) const override;

protected:
  MinimumDecisionRule() = default;
  ~MinimumDecisionRule() override = default;

}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#endif

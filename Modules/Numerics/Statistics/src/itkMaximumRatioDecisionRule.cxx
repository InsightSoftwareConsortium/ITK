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
#include "itkMaximumRatioDecisionRule.h"
#include "itkMath.h"

namespace itk
{
namespace Statistics
{
MaximumRatioDecisionRule
::MaximumRatioDecisionRule()
{
}

void
MaximumRatioDecisionRule
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << "Prior probabilities: ";
  PriorProbabilityVectorType::size_type N = 10;
  if (m_PriorProbabilities.size() < N)
    {
    N = m_PriorProbabilities.size();
    }
  os << "[" << std::endl;
  for (PriorProbabilityVectorType::size_type i = 0; i < N; ++i)
    {
    os << m_PriorProbabilities[i];
    if (i+1 < N)
      {
      os << ", ";
      }
    }
  if (m_PriorProbabilities.size() > 0 && m_PriorProbabilities.size() < N)
    {
    os << ", ...";
    }
  os << "]" << std::endl;
}

void
MaximumRatioDecisionRule
::SetPriorProbabilities(const PriorProbabilityVectorType& p)
{
  if (p.size() != m_PriorProbabilities.size())
    {
    m_PriorProbabilities = p;
    this->Modified();
    }
  else
    {
    PriorProbabilityVectorType::const_iterator pit, it;

    for (pit = p.begin(), it = m_PriorProbabilities.begin();
         pit != p.end(); ++pit, ++it)
      {
      if ( fabs( *pit - *it ) > itk::Math::eps )
        {
        break;
        }
      }
    if (pit != p.end())
      {
      m_PriorProbabilities = p;
      this->Modified();
      }
    }
}

MaximumRatioDecisionRule::ClassIdentifierType
MaximumRatioDecisionRule
::Evaluate(const MembershipVectorType & discriminantScores) const
{
  bool uniformPrior = false;
  if (discriminantScores.size() != m_PriorProbabilities.size())
    {
    itkWarningMacro("Size mismatch between discriminant scores (" << discriminantScores.size() << ") and priors (" << m_PriorProbabilities.size() << "). Reverting to a uniform prior.");
    uniformPrior = true;
    }

  if (uniformPrior)
    {
    // find the maximum discriminant score. if list is empty, return 0.
    ClassIdentifierType i, besti = 0;
    MembershipValueType best = NumericTraits<MembershipValueType>::NonpositiveMin();
    for (i=0; i < discriminantScores.size(); ++i)
      {
      if (discriminantScores[i] > best)
        {
        best = discriminantScores[i];
        besti = i;
        }
      }
    return besti;
    }

  // Non-uniform prior case
  // find the maximum p(x|i)*p(i)
  ClassIdentifierType i, besti = 0;
  MembershipValueType best = NumericTraits<MembershipValueType>::NonpositiveMin();
  MembershipValueType temp = NumericTraits<MembershipValueType>::NonpositiveMin();

  for (i=0; i < discriminantScores.size(); ++i)
    {
    temp = discriminantScores[i] * m_PriorProbabilities[i];
    if (temp > best)
      {
      best = temp;
      besti = i;
      }
    }
  return besti;
}
} // end of Statistics namespace
} // end of ITK namespace

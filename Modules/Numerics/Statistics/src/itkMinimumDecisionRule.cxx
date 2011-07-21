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
#include "itkMinimumDecisionRule.h"

namespace itk
{
namespace Statistics
{
MinimumDecisionRule::ClassIdentifierType
MinimumDecisionRule
::Evaluate(const MembershipVectorType & discriminantScores) const
{
  ClassIdentifierType minIndex = 0;

  if (discriminantScores.size() > 0)
    {
    MembershipValueType  min = discriminantScores[0];
    ClassIdentifierType i;

    for ( i = 1; i < discriminantScores.size(); i++ )
      {
      if ( discriminantScores[i] < min )
        {
        min = discriminantScores[i];
        minIndex = i;
        }
      }
    }
  return minIndex;
}
} // end of namespace Statistics
} // end of namespace itk

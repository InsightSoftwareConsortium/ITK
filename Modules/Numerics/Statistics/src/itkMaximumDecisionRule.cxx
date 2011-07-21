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
#include "itkMaximumDecisionRule.h"

namespace itk
{
namespace Statistics
{
MaximumDecisionRule::ClassIdentifierType
MaximumDecisionRule
::Evaluate(const MembershipVectorType & discriminantScores) const
{
  ClassIdentifierType maxIndex = 0;

  if (discriminantScores.size() > 0)
    {
    MembershipValueType  max = discriminantScores[0];
    ClassIdentifierType i;

    for ( i = 1; i < discriminantScores.size(); i++ )
      {
      if ( discriminantScores[i] > max )
        {
        max = discriminantScores[i];
        maxIndex = i;
        }
      }
    }
  return maxIndex;
}
} // end of namespace Statistics
} // end of namespace itk

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
#include "itkMinimumDecisionRule2.h"

namespace itk
{
namespace Statistics
{
void
MinimumDecisionRule2::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

unsigned int
MinimumDecisionRule2::Evaluate(const MembershipVectorType & discriminantScores) const
{
  double       min = discriminantScores[0];
  unsigned int minIndex = 0;
  unsigned int i;

  for ( i = 1; i < discriminantScores.size(); i++ )
    {
    if ( discriminantScores[i] < min )
      {
      min = discriminantScores[i];
      minIndex = i;
      }
    }
  return minIndex;
}
} // end of namespace itk
} // end of namespace Statistics

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
void
MaximumDecisionRule::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

unsigned int
MaximumDecisionRule::Evaluate(const VectorType & discriminantScores) const
{
  double       max = discriminantScores[0];
  unsigned int maxIndex = 0;
  unsigned int i;

  for ( i = 1; i < discriminantScores.size(); i++ )
    {
    if ( discriminantScores[i] > max )
      {
      max = discriminantScores[i];
      maxIndex = i;
      }
    }
  return maxIndex;
}

unsigned int
MaximumDecisionRule::Evaluate(const ArrayType & discriminantScores) const
{
  double       max = discriminantScores[0];
  unsigned int maxIndex = 0;
  unsigned int i;

  for ( i = 1; i < discriminantScores.Size(); i++ )
    {
    if ( discriminantScores[i] > max )
      {
      max = discriminantScores[i];
      maxIndex = i;
      }
    }
  return maxIndex;
}

unsigned int
MaximumDecisionRule::Evaluate(const VariableLengthVectorType & discriminantScores) const
{
  double       max = discriminantScores[0];
  unsigned int maxIndex = 0;
  unsigned int i;

  for ( i = 1; i < discriminantScores.Size(); i++ )
    {
    if ( discriminantScores[i] > max )
      {
      max = discriminantScores[i];
      maxIndex = i;
      }
    }
  return maxIndex;
}
} // end of namespace itk

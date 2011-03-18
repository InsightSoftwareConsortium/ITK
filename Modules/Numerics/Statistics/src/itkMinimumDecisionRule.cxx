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
MinimumDecisionRule
::MinimumDecisionRule()
{}

unsigned int
MinimumDecisionRule
::Evaluate(const VectorType & discriminantScores) const
{
  double       minimumDistance      = discriminantScores[0];
  unsigned int classifiedPixelIndex = 0;
  unsigned int numberOfClasses      = static_cast< unsigned int >( discriminantScores.size() );

  //Loop through the probabilities to get the best index
  for ( unsigned int classIndex = 1; classIndex < numberOfClasses; classIndex++ )
    {
    if ( discriminantScores[classIndex] < minimumDistance )
      {
      minimumDistance      = discriminantScores[classIndex];
      classifiedPixelIndex = classIndex;
      }
    } // end for

  return classifiedPixelIndex;
}

unsigned int
MinimumDecisionRule
::Evaluate(const ArrayType & discriminantScores) const
{
  double       minimumDistance      = discriminantScores[0];
  unsigned int classifiedPixelIndex = 0;
  unsigned int numberOfClasses      = static_cast< unsigned int >( discriminantScores.Size() );

  //Loop through the probabilities to get the best index
  for ( unsigned int classIndex = 1; classIndex < numberOfClasses; classIndex++ )
    {
    if ( discriminantScores[classIndex] < minimumDistance )
      {
      minimumDistance      = discriminantScores[classIndex];
      classifiedPixelIndex = classIndex;
      }
    } // end for

  return classifiedPixelIndex;
}
} // namespace itk

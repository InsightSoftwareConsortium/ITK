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

#ifndef itkLevelSetEvolutionNumberOfIterationsStoppingCriterion_hxx
#define itkLevelSetEvolutionNumberOfIterationsStoppingCriterion_hxx

#include "itkLevelSetEvolutionNumberOfIterationsStoppingCriterion.h"

namespace itk
{
template< typename TLevelSetContainer >
LevelSetEvolutionNumberOfIterationsStoppingCriterion< TLevelSetContainer >::
LevelSetEvolutionNumberOfIterationsStoppingCriterion() {}

template< typename TLevelSetContainer >
LevelSetEvolutionNumberOfIterationsStoppingCriterion< TLevelSetContainer >::
~LevelSetEvolutionNumberOfIterationsStoppingCriterion() {}


template< typename TLevelSetContainer >
bool
LevelSetEvolutionNumberOfIterationsStoppingCriterion< TLevelSetContainer >::
IsSatisfied() const
{
  return ( this->m_CurrentIteration >= this->m_NumberOfIterations );
}

template< typename TLevelSetContainer >
std::string
LevelSetEvolutionNumberOfIterationsStoppingCriterion< TLevelSetContainer >::
GetDescription() const
{
  return "Current Iteration Number >= Number Of Iterations";
}

}
 #endif

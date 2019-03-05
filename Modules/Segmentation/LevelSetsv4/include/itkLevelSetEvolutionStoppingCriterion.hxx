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

#ifndef itkLevelSetEvolutionStoppingCriterion_hxx
#define itkLevelSetEvolutionStoppingCriterion_hxx

#include "itkLevelSetEvolutionStoppingCriterion.h"

namespace itk
{
template< typename TLevelSetContainer >
LevelSetEvolutionStoppingCriterion< TLevelSetContainer >
::LevelSetEvolutionStoppingCriterion()
{
  this->m_RMSChangeAccumulator = NumericTraits< OutputRealType >::ZeroValue();
  this->m_NumberOfIterations = NumericTraits< IterationIdType >::ZeroValue();
  this->m_CurrentIteration = NumericTraits< IterationIdType >::ZeroValue();
}
}
#endif

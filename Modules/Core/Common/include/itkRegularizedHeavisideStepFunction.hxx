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
#ifndef itkRegularizedHeavisideStepFunction_hxx
#define itkRegularizedHeavisideStepFunction_hxx

#include "itkRegularizedHeavisideStepFunction.h"

namespace itk
{
template< typename TInput, typename TOutput >
RegularizedHeavisideStepFunction< TInput, TOutput >
::RegularizedHeavisideStepFunction() : Superclass(),
  m_Epsilon( NumericTraits< RealType >::OneValue() ),
  m_OneOverEpsilon( NumericTraits< RealType >::OneValue() )
{}

template< typename TInput, typename TOutput >
RegularizedHeavisideStepFunction< TInput, TOutput >
::~RegularizedHeavisideStepFunction()
{}

template< typename TInput, typename TOutput >
void
RegularizedHeavisideStepFunction< TInput, TOutput >
::SetEpsilon(const RealType & ieps)
{
  if ( ieps > NumericTraits< RealType >::epsilon() )
    {
    this->m_Epsilon = ieps;
    m_OneOverEpsilon = 1.0 / ieps;
    }
  else
    {
    itkGenericExceptionMacro("ERROR: Epsilon needs to be greater than "
                               << NumericTraits< RealType >::epsilon() );
    }
}
}

#endif

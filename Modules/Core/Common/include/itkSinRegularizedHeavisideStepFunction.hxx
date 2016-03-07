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
#ifndef itkSinRegularizedHeavisideStepFunction_hxx
#define itkSinRegularizedHeavisideStepFunction_hxx

#include "itkSinRegularizedHeavisideStepFunction.h"
#include "itkMath.h"

namespace itk
{
template< typename TInput, typename TOutput >
SinRegularizedHeavisideStepFunction< TInput, TOutput >
::SinRegularizedHeavisideStepFunction() : Superclass()
{}

template< typename TInput, typename TOutput >
SinRegularizedHeavisideStepFunction< TInput, TOutput >
::~SinRegularizedHeavisideStepFunction()
{}


template< typename TInput, typename TOutput >
typename SinRegularizedHeavisideStepFunction< TInput, TOutput >::OutputType
SinRegularizedHeavisideStepFunction< TInput, TOutput >
::Evaluate( const InputType& input ) const
{
  if ( static_cast< RealType >( input ) >= this->GetEpsilon() )
    {
    return NumericTraits< OutputType >::OneValue();
    }
  else
    {
    if ( static_cast< RealType >( input ) <= -this->GetEpsilon() )
      {
      return NumericTraits< OutputType >::ZeroValue();
      }
    else
      {
      const RealType angleFactor = 0.5 * itk::Math::pi * this->GetOneOverEpsilon();
      const RealType angle = input * angleFactor;

      return static_cast< OutputType >( 0.5 * ( 1.0 + std::sin( angle ) ) );
      }
    }
}

template< typename TInput, typename TOutput >
typename SinRegularizedHeavisideStepFunction< TInput, TOutput >::OutputType
SinRegularizedHeavisideStepFunction< TInput, TOutput >
::EvaluateDerivative(const InputType & input) const
{
  if ( itk::Math::abs( static_cast< RealType >( input ) ) >= this->GetEpsilon() )
    {
    return NumericTraits< OutputType >::ZeroValue();
    }
  else
    {
    const RealType angleFactor = 0.5 * itk::Math::pi * this->GetOneOverEpsilon();
    const RealType angle = input * angleFactor;

    return static_cast< OutputType >( 0.5 * angleFactor * std::cos(angle) );
    }
}

}

#endif

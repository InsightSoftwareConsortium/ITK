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
#ifndef itkAtanRegularizedHeavisideStepFunction_hxx
#define itkAtanRegularizedHeavisideStepFunction_hxx

#include "itkAtanRegularizedHeavisideStepFunction.h"
#include "itkMath.h"

namespace itk
{
template< typename TInput, typename TOutput >
AtanRegularizedHeavisideStepFunction< TInput, TOutput >::
AtanRegularizedHeavisideStepFunction() : Superclass()
{}

template< typename TInput, typename TOutput >
AtanRegularizedHeavisideStepFunction< TInput, TOutput >::
~AtanRegularizedHeavisideStepFunction()
{}

template< typename TInput, typename TOutput >
typename AtanRegularizedHeavisideStepFunction< TInput, TOutput >::OutputType
AtanRegularizedHeavisideStepFunction< TInput, TOutput >
::Evaluate(const InputType & input) const
{
  const RealType t = static_cast< RealType >( input ) * this->GetOneOverEpsilon();
  return 0.5 + static_cast< OutputType >( itk::Math::one_over_pi * std::atan( t ) );
}

/** Evaluate the derivative at the specified input position */
template< typename TInput, typename TOutput >
typename AtanRegularizedHeavisideStepFunction< TInput, TOutput >::OutputType
AtanRegularizedHeavisideStepFunction< TInput, TOutput >
::EvaluateDerivative(const InputType & input) const
{
  const RealType oneOverEpsilon = this->GetOneOverEpsilon();
  const RealType t = static_cast< RealType >( input ) * oneOverEpsilon;

  return static_cast< OutputType >( Math::one_over_pi * oneOverEpsilon / ( 1.0 + t * t ) );
}

} // namespace itk

#endif

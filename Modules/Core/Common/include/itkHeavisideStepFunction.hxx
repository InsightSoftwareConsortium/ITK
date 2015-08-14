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
#ifndef itkHeavisideStepFunction_hxx
#define itkHeavisideStepFunction_hxx

#include "itkHeavisideStepFunction.h"
#include "itkMath.h"

namespace itk
{
template< typename TInput, typename TOutput >
HeavisideStepFunction< TInput, TOutput >
::HeavisideStepFunction() : Superclass()
{}

template< typename TInput, typename TOutput >
HeavisideStepFunction< TInput, TOutput >
::~HeavisideStepFunction()
{}

template< typename TInput, typename TOutput >
typename HeavisideStepFunction< TInput, TOutput >::OutputType
HeavisideStepFunction< TInput, TOutput >
::Evaluate(const InputType & input) const
{
  return ( input >= NumericTraits< InputType >::ZeroValue() ) ?
          NumericTraits< OutputType >::OneValue() : NumericTraits< OutputType >::ZeroValue();
}

template< typename TInput, typename TOutput >
typename HeavisideStepFunction< TInput, TOutput >::OutputType
HeavisideStepFunction< TInput, TOutput >
::EvaluateDerivative(const InputType & input) const
{
  return ( Math::ExactlyEquals(input, NumericTraits< InputType >::ZeroValue()) ) ?
    NumericTraits< OutputType >::OneValue() : NumericTraits< OutputType >::ZeroValue();
}

}

#endif // itkHeavisideStepFunction_hxx

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
#ifndef itkLogSigmoidTransferFunction_hxx
#define itkLogSigmoidTransferFunction_hxx

#include "itkLogSigmoidTransferFunction.h"

namespace itk
{
namespace Statistics
{

template<typename TScalar>
LogSigmoidTransferFunction<TScalar>
::LogSigmoidTransferFunction()
{
}

template<typename TScalar>
LogSigmoidTransferFunction<TScalar>
::~LogSigmoidTransferFunction()
{
}

template<typename TScalar>
TScalar
LogSigmoidTransferFunction<TScalar>
::Evaluate(const ScalarType& input)  const
{
  const ScalarType v = static_cast< ScalarType >( 1.0 /
    ( 1.0 + std::exp( -1*static_cast< typename NumericTraits< ScalarType >::RealType >(input)) ) );
  return v;
}

template<typename TScalar>
TScalar
LogSigmoidTransferFunction<TScalar>
::EvaluateDerivative(const ScalarType& input)  const
{
  ScalarType f = Evaluate(input);
  return f * (1 - f);
}

template<typename TScalar>
void
LogSigmoidTransferFunction<TScalar>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "LogSigmoidTransferFunction(" << this << ")" << std::endl;
}

} // end namespace Statistics
} // end namespace itk

#endif

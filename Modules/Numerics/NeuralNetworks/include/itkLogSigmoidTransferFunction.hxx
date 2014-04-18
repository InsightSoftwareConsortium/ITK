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
#ifndef __itkLogSigmoidTransferFunction_hxx
#define __itkLogSigmoidTransferFunction_hxx

#include "itkLogSigmoidTransferFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<typename ScalarType>
LogSigmoidTransferFunction<ScalarType>
::LogSigmoidTransferFunction()
{
}

/** Destructor */
template<typename ScalarType>
LogSigmoidTransferFunction<ScalarType>
::~LogSigmoidTransferFunction()
{
}

/** Evaluate */
template<typename ScalarType>
ScalarType
LogSigmoidTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  typedef typename NumericTraits< ScalarType >::RealType RealType;
  const ScalarType v = static_cast< ScalarType >( 1.0 /
    ( 1.0 + std::exp( static_cast< typename NumericTraits< ScalarType >::RealType >(-input)) ) );
  return v;
}

/** Evaluate derivatives */
template<typename ScalarType>
ScalarType
LogSigmoidTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  ScalarType f = Evaluate(input);
  return f * (1 - f);
}

/** Print the object */
template<typename ScalarType>
void
LogSigmoidTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "LogSigmoidTransferFunction(" << this << ")" << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif

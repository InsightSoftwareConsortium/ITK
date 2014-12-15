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
#ifndef itkSigmoidTransferFunction_hxx
#define itkSigmoidTransferFunction_hxx

#include "itkSigmoidTransferFunction.h"

namespace itk
{
namespace Statistics
{


template<typename TScalar>
SigmoidTransferFunction<TScalar>
::SigmoidTransferFunction()
{
  m_Alpha = 1.0;
  m_Beta = 0.0;
  m_OutputMinimum = NumericTraits<ScalarType>::min();
  m_OutputMaximum = NumericTraits<ScalarType>::max();
}


template<typename TScalar>
SigmoidTransferFunction<TScalar>
::~SigmoidTransferFunction()
{
}


template<typename TScalar>
TScalar
SigmoidTransferFunction<TScalar>
::Evaluate(const ScalarType& input)  const
{
  const RealType x = static_cast< RealType >( input - m_Beta ) / m_Alpha;
  const RealType e = 1.0 / (1.0 + std::exp( static_cast< typename NumericTraits< ScalarType >::RealType >(-x)));
  const ScalarType v = static_cast< ScalarType >( (m_OutputMaximum - m_OutputMinimum) * e )
                     + m_OutputMinimum;
  return v;
}


template<typename TScalar>
TScalar
SigmoidTransferFunction<TScalar>
::EvaluateDerivative(const ScalarType& input)  const
{
  ScalarType f = Evaluate(input);
  return f * (1 - f);
}


template<typename TScalar>
void
SigmoidTransferFunction<TScalar>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "SigmoidTransferFunction(" << this << ")" << std::endl;
  os << indent << "m_Alpha = " << m_Alpha << std::endl;
  os << indent << "m_Beta = " << m_Beta << std::endl;
  os << indent << "m_OutputMinimum = " << m_OutputMinimum << std::endl;
  os << indent << "m_OutputMaximum = " << m_OutputMaximum << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif

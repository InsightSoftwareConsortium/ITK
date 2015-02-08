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
#ifndef itkSymmetricSigmoidTransferFunction_hxx
#define itkSymmetricSigmoidTransferFunction_hxx

#include "itkSymmetricSigmoidTransferFunction.h"

namespace itk
{
namespace Statistics
{

template<typename ScalarType>
SymmetricSigmoidTransferFunction<ScalarType>
::SymmetricSigmoidTransferFunction()
{
  m_Range = 15.0;
  m_Offset = 0.1;
}

template<typename ScalarType>
SymmetricSigmoidTransferFunction<ScalarType>
::~SymmetricSigmoidTransferFunction()
{
}

template<typename ScalarType>
ScalarType
SymmetricSigmoidTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  ScalarType val;
  if(input<-m_Range)
    {
    val=-0.5;
    }
  else if(input>m_Range)
    {
    val=0.5;
    }
  else
    {
    val= (ScalarType)1.0/(1.0+std::exp(-input))-0.5;
    }
  return val;
}

/** Evaluate derivatives */
template<typename ScalarType>
ScalarType
SymmetricSigmoidTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  ScalarType f = Evaluate(input);
  return (m_Offset+(0.25-f*f));
}


/** Print the object */
template<typename ScalarType>
void
SymmetricSigmoidTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "SumInputFunction(" << this << ")" << std::endl;
  os << indent << "m_Range = " << m_Range << std::endl;
  os << indent << "m_Offset = " << m_Offset << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk


#endif

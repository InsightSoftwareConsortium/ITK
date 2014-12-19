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
#ifndef itkMultiquadricRadialBasisFunction_hxx
#define itkMultiquadricRadialBasisFunction_hxx

#include "itkMultiquadricRadialBasisFunction.h"
#include <cmath>

namespace itk
{
namespace Statistics
{

template<typename ScalarType>
MultiquadricRadialBasisFunction<ScalarType>
::MultiquadricRadialBasisFunction()
{
  m_Center=0.0;
  m_Radius=1;
}

template<typename ScalarType>
MultiquadricRadialBasisFunction<ScalarType>
::~MultiquadricRadialBasisFunction()
{
}

template<typename ScalarType>
ScalarType
MultiquadricRadialBasisFunction<ScalarType>
::Evaluate(const ScalarType& input) const
{
  const ScalarType val = std::pow((input*input)+(m_Radius*m_Radius),0.5);
  return val;
}

/** Print the object */
template<typename ScalarType>
void
MultiquadricRadialBasisFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "MultiquadricRadialBasisFunction(" << this << ")" << std::endl;
  os << indent << "m_Center = " << m_Center << std::endl;
  os << indent << "m_Radius = " << m_Radius << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif

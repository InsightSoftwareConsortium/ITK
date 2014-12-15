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
#ifndef itkHardLimitTransferFunction_hxx
#define itkHardLimitTransferFunction_hxx

#include "itkHardLimitTransferFunction.h"

namespace itk
{
namespace Statistics
{

template<typename ScalarType>
HardLimitTransferFunction< ScalarType>
::HardLimitTransferFunction()
{
}

template<typename ScalarType>
HardLimitTransferFunction<ScalarType>
::~HardLimitTransferFunction()
{
}

template<typename ScalarType>
ScalarType
HardLimitTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  return (input >= 0);
}

template<typename ScalarType>
ScalarType
HardLimitTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& )  const
{
  return 0;
}

/** Print the object */
template<typename ScalarType>
void
HardLimitTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "HardLimitTransferFunction(" << this << ")" << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk


#endif

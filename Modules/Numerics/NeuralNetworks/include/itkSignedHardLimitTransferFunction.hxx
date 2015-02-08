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
#ifndef itkSignedHardLimitTransferFunction_hxx
#define itkSignedHardLimitTransferFunction_hxx

#include "itkSignedHardLimitTransferFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<typename ScalarType>
SignedHardLimitTransferFunction<ScalarType>
::SignedHardLimitTransferFunction()
{
}

/** Destructor */
template<typename ScalarType>
SignedHardLimitTransferFunction<ScalarType>
::~SignedHardLimitTransferFunction()
{
}

/** Evaluate */
template<typename ScalarType>
ScalarType
SignedHardLimitTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  return 2 * (input >= 0) - 1;
}

/** Evaluate derivative */
template<typename ScalarType>
ScalarType
SignedHardLimitTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& )  const
{
  return 0;
}

/** Print the object */
template<typename ScalarType>
void
SignedHardLimitTransferFunction< ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "SignedHardLimitTransferFunction(" << this << ")" << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif

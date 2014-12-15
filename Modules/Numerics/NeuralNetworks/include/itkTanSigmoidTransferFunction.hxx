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
#ifndef itkTanSigmoidTransferFunction_hxx
#define itkTanSigmoidTransferFunction_hxx

#include "itkTanSigmoidTransferFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<typename ScalarType>
TanSigmoidTransferFunction<ScalarType>
::TanSigmoidTransferFunction()
{
}

/** Destructor */
template<typename ScalarType>
TanSigmoidTransferFunction<ScalarType>
::~TanSigmoidTransferFunction()
{
}

/** Evaluate */
template<typename ScalarType>
ScalarType
TanSigmoidTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  return static_cast<ScalarType>((2
                                / (1 + std::exp(-2 * static_cast<ScalarType>(input))))
                               - 1);
}

/** Evaluate derivatives */
template<typename ScalarType>
ScalarType
TanSigmoidTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  ScalarType f = Evaluate(input);
  return 1 - (f * f);
}

/** Print the object */
template<typename ScalarType>
void
TanSigmoidTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "TanSigmoidTransferFunction(" << this << ")" << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif

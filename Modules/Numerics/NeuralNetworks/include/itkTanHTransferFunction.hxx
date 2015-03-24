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
#ifndef itkTanHTransferFunction_hxx
#define itkTanHTransferFunction_hxx

#include "itkTanHTransferFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<typename ScalarType>
TanHTransferFunction<ScalarType>
::TanHTransferFunction()
{
}

/** Destructor */
template<typename ScalarType>
TanHTransferFunction<ScalarType>
::~TanHTransferFunction()
{
}

/** Evaluate */
template<typename ScalarType>
ScalarType
TanHTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  ScalarType x = std::exp(input);
  ScalarType y = std::exp(-input);
  return static_cast<ScalarType>((float) (x - y) / (x + y));
}

/** Evaluate derivatives */
template<typename ScalarType>
ScalarType
TanHTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  ScalarType f = Evaluate(input);
  return 1 - std::pow(f, 2);
}

/** Print the object */
template<typename ScalarType>
void
TanHTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "TanHTransferFunction(" << this << ")" << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif

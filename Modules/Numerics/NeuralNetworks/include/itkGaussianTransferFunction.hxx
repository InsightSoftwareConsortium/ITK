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
#ifndef itkGaussianTransferFunction_hxx
#define itkGaussianTransferFunction_hxx

#include "itkGaussianTransferFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<typename ScalarType>
GaussianTransferFunction<ScalarType>
::GaussianTransferFunction()
{
}

/** Destructor */
template<typename ScalarType>
GaussianTransferFunction<ScalarType>
::~GaussianTransferFunction()
{
}

/** Evaluate function */
template<typename ScalarType>
ScalarType
GaussianTransferFunction<ScalarType>
::Evaluate(const ScalarType& input)  const
{
  return static_cast<ScalarType>((std::exp(-1 * input * input)));
}

/** Evaluate derivatives function */
template<typename ScalarType>
ScalarType
GaussianTransferFunction<ScalarType>
::EvaluateDerivative(const ScalarType& input)  const
{
  return static_cast<ScalarType>(-2 * Evaluate(input) * input);
}

/** Print the object */
template<typename ScalarType>
void
GaussianTransferFunction<ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "GaussianTransferFunction(" << this << ")" << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif

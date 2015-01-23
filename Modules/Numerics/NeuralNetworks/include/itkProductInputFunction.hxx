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
#ifndef itkProductInputFunction_hxx
#define itkProductInputFunction_hxx

#include "itkProductInputFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<typename TMeasurementVector, typename ScalarType>
ProductInputFunction<TMeasurementVector,ScalarType>
::ProductInputFunction()
{
}

/** Destructor */
template<typename TMeasurementVector, typename ScalarType>
ProductInputFunction <TMeasurementVector,ScalarType>
::~ProductInputFunction()
{
}

/** Evaluate */
template<typename TMeasurementVector, typename ScalarType>
ScalarType
ProductInputFunction <TMeasurementVector,ScalarType>
::Evaluate(const TMeasurementVector& input)  const
{
  vnl_vector<ScalarType> temp(input);
  ScalarType product = temp[0];
  for (unsigned int i = 1; i < input.Size(); i++)
    {
    product *= temp[i];
    }
  return product;
}

/** Print the object */
template<typename TMeasurementVector, typename ScalarType>
void
ProductInputFunction <TMeasurementVector,ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "ProductInputFunction(" << this << ")" << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif

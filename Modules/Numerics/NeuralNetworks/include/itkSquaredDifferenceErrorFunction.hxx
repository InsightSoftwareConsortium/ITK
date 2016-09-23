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
#ifndef itkSquaredDifferenceErrorFunction_hxx
#define itkSquaredDifferenceErrorFunction_hxx

#include "itkSquaredDifferenceErrorFunction.h"

namespace itk
{
namespace Statistics
{

template<typename TMeasurementVector, typename ScalarType>
SquaredDifferenceErrorFunction<TMeasurementVector,ScalarType>
::SquaredDifferenceErrorFunction()
{
}

template<typename TMeasurementVector, typename ScalarType>
SquaredDifferenceErrorFunction <TMeasurementVector,ScalarType>
::~SquaredDifferenceErrorFunction()
{
}

template<typename TMeasurementVector, typename ScalarType>
ScalarType
SquaredDifferenceErrorFunction <TMeasurementVector,ScalarType>
::Evaluate(const TMeasurementVector& errors) const
{
  vnl_vector <ScalarType> temp;
  temp.set_size( errors.Size() );
  for( unsigned int i = 0; i < errors.Size(); i++ )
    {
    temp[i] = errors[i];
    }

  return (temp.squared_magnitude() / 2);
}

template<typename TMeasurementVector, typename ScalarType>
typename SquaredDifferenceErrorFunction <TMeasurementVector,ScalarType>
::InternalVectorType
SquaredDifferenceErrorFunction <TMeasurementVector,ScalarType>
::EvaluateDerivative(const TMeasurementVector& errors)  const
{
  InternalVectorType diff;
  diff.SetSize( errors.Size() );
  for( unsigned int i = 0; i < errors.Size(); i++ )
    {
    if ((-0.1 < errors[i]) && (errors[i] < 0.1))
      {
      diff[i] = 0;
      }
    else
      {
      diff[i] = errors[i];
      }
    }
  return diff;
}

template<typename TMeasurementVector, typename ScalarType>
void
SquaredDifferenceErrorFunction <TMeasurementVector,ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "SquaredDifferenceErrorFunction(" << this << ")" << std::endl;
}

} // end namespace Statistics
} // end namespace itk

#endif

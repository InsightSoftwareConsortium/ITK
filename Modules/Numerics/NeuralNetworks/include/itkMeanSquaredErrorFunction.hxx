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
#ifndef itkMeanSquaredErrorFunction_hxx
#define itkMeanSquaredErrorFunction_hxx

#include "itkMeanSquaredErrorFunction.h"

namespace itk
{
namespace Statistics
{

/** Constructor */
template<typename TMeasurementVector, typename ScalarType>
MeanSquaredErrorFunction<TMeasurementVector,ScalarType>
::MeanSquaredErrorFunction()
{
}

/** Destructor */
template<typename TMeasurementVector, typename ScalarType>
MeanSquaredErrorFunction <TMeasurementVector,ScalarType>
::~MeanSquaredErrorFunction()
{
}

/** Evaluate */
template<typename TMeasurementVector, typename ScalarType>
ScalarType
MeanSquaredErrorFunction <TMeasurementVector,ScalarType>
::Evaluate(const TMeasurementVector& errors)  const
{
  vnl_vector <ScalarType> temp(errors.Size());
  for(unsigned int i=0; i<errors.Size(); i++)
    {
    temp[i]=errors[i];
    }
  return (temp.squared_magnitude() / temp.size());
}

/** Evaluate derivatives */
template<typename TMeasurementVector, typename ScalarType>
typename MeanSquaredErrorFunction <TMeasurementVector,ScalarType>
::InternalVectorType
MeanSquaredErrorFunction <TMeasurementVector,ScalarType>
::EvaluateDerivative(const TMeasurementVector& errors)  const
{
  ScalarType m = static_cast<ScalarType>(2) / errors.Size();
  InternalVectorType temp(errors.Size());
  for(unsigned int i=0; i<errors.Size(); i++)
  {
     temp[i]=errors[i]*m;
  }
  return temp;
}

/** Print the object */
template<typename TMeasurementVector, typename ScalarType>
void
MeanSquaredErrorFunction <TMeasurementVector,ScalarType>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "MeanSquaredErrorFunction(" << this << ")" << std::endl;
  Superclass::PrintSelf( os, indent );
}

} // end namespace Statistics
} // end namespace itk

#endif

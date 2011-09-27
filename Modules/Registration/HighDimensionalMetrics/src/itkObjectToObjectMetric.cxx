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
#include "itkObjectToObjectMetric.h"

namespace itk
{

//-------------------------------------------------------------------
ObjectToObjectMetric
::ObjectToObjectMetric()
{
  // Don't call SetGradientSource, to avoid valgrind warning.
  this->m_GradientSource = this->GRADIENT_SOURCE_MOVING;
}

//-------------------------------------------------------------------
ObjectToObjectMetric
::~ObjectToObjectMetric()
{}

//-------------------------------------------------------------------
void
ObjectToObjectMetric
::GetDerivative(DerivativeType & derivative)
{
  MeasureType value;
  this->GetValueAndDerivative(value, derivative);
}

//-------------------------------------------------------------------
void
ObjectToObjectMetric
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "TODO...";
}

//-------------------------------------------------------------------
ObjectToObjectMetric::MeasureType
ObjectToObjectMetric
::GetValue( const ParametersType& ) const
{
  itkExceptionMacro("Not implemented. Use GetValue(void).");
}

//-------------------------------------------------------------------
void
ObjectToObjectMetric
::GetDerivative( const ParametersType &, DerivativeType &) const
{
  itkExceptionMacro("Not implemented. Use GetDerivative(DerivativeType&).");
}

//-------------------------------------------------------------------
void
ObjectToObjectMetric
::GetValueAndDerivative (const ParametersType &,
                              MeasureType &,
                              DerivativeType &) const
{
  itkExceptionMacro("Not implemented. Use GetValueAndDerivative( "
                      "MeasureType & value, DerivativeType & derivative).");
}

}//namespace itk

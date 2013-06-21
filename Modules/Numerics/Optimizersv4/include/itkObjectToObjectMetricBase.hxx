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
#ifndef __itkObjectToObjectMetricBase_hxx
#define __itkObjectToObjectMetricBase_hxx

#include "itkObjectToObjectMetricBase.h"

namespace itk
{

//-------------------------------------------------------------------
template<class TInternalComputationValueType>
ObjectToObjectMetricBaseTemplate<TInternalComputationValueType>
::ObjectToObjectMetricBaseTemplate()
{
  // Don't call SetGradientSource, to avoid valgrind warning.
  this->m_GradientSource = this->GRADIENT_SOURCE_MOVING;
  this->m_Value = NumericTraits<MeasureType>::Zero;
}

//-------------------------------------------------------------------
template<class TInternalComputationValueType>
ObjectToObjectMetricBaseTemplate<TInternalComputationValueType>
::~ObjectToObjectMetricBaseTemplate()
{}

//-------------------------------------------------------------------
template<class TInternalComputationValueType>
bool ObjectToObjectMetricBaseTemplate<TInternalComputationValueType>
::GetGradientSourceIncludesFixed() const
{
  return m_GradientSource == GRADIENT_SOURCE_FIXED ||
  m_GradientSource == GRADIENT_SOURCE_BOTH;
}

//-------------------------------------------------------------------
template<class TInternalComputationValueType>
bool ObjectToObjectMetricBaseTemplate<TInternalComputationValueType>
::GetGradientSourceIncludesMoving() const
{
  return m_GradientSource == GRADIENT_SOURCE_MOVING ||
  m_GradientSource == GRADIENT_SOURCE_BOTH;
}

//-------------------------------------------------------------------
template<class TInternalComputationValueType>
typename ObjectToObjectMetricBaseTemplate<TInternalComputationValueType>::MeasureType
ObjectToObjectMetricBaseTemplate<TInternalComputationValueType>
::GetCurrentValue() const
{
  return m_Value;
}

//-------------------------------------------------------------------
template<class TInternalComputationValueType>
void
ObjectToObjectMetricBaseTemplate<TInternalComputationValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
Superclass::PrintSelf(os, indent);
os << indent << "Value: " << m_Value << std::endl;
os << indent << "GradientSourceType: ";
switch( m_GradientSource )
  {
    case GRADIENT_SOURCE_FIXED:
    os << "GRADIENT_SOURCE_FIXED";
    break;
    case GRADIENT_SOURCE_MOVING:
    os << "GRADIENT_SOURCE_MOVING";
    break;
    case GRADIENT_SOURCE_BOTH:
    os << "GRADIENT_SOURCE_BOTH";
    break;
    default:
    itkExceptionMacro(<< "Unknown GradientSource.");
  }
os << std::endl;
}

}//namespace itk

#endif

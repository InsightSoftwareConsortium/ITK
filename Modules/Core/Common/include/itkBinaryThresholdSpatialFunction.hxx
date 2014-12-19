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
#ifndef itkBinaryThresholdSpatialFunction_hxx
#define itkBinaryThresholdSpatialFunction_hxx

#include "itkBinaryThresholdSpatialFunction.h"

namespace itk
{
template< typename TFunction >
BinaryThresholdSpatialFunction< TFunction >
::BinaryThresholdSpatialFunction()
{
  m_LowerThreshold = NumericTraits< FunctionOutputType >::NonpositiveMin();
  m_UpperThreshold = NumericTraits< FunctionOutputType >::max();
  m_Function = ITK_NULLPTR;
}

template< typename TFunction >
BinaryThresholdSpatialFunction< TFunction >
::~BinaryThresholdSpatialFunction()
{}

template< typename TFunction >
void
BinaryThresholdSpatialFunction< TFunction >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << " m_LowerThreshold: " << m_LowerThreshold << std::endl;
  os << indent << " m_UpperThreshold: " << m_UpperThreshold << std::endl;
  os << indent << " m_Function: " << m_Function.GetPointer() << std::endl;
}

template< typename TFunction >
typename BinaryThresholdSpatialFunction< TFunction >
::OutputType
BinaryThresholdSpatialFunction< TFunction >
::Evaluate(const InputType & point) const
{
  FunctionOutputType value = m_Function->Evaluate(point);

  if ( m_LowerThreshold <= value && value <= m_UpperThreshold )
    {
    return true;
    }
  return false;
}
} // end namespace itk

#endif

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
#ifndef _itkMultipleValuedNonLinearOptimizer_hxx
#define _itkMultipleValuedNonLinearOptimizer_hxx

#include "itkMultipleValuedNonLinearOptimizer.h"

namespace itk
{
MultipleValuedNonLinearOptimizer
::MultipleValuedNonLinearOptimizer()
{
  m_CostFunction = ITK_NULLPTR;
}

/**
 * Connect a Cost Function
 */
void
MultipleValuedNonLinearOptimizer
::SetCostFunction(CostFunctionType *costFunction)
{
  if ( m_CostFunction.GetPointer() == costFunction )
    {
    return;
    }

  itkDebugMacro("setting CostFunction  to " <<  costFunction);

  m_CostFunction = costFunction;

  if ( !m_ScalesInitialized )
    {
    const unsigned int numberOfParameters =
      m_CostFunction->GetNumberOfParameters();

    ScalesType scales(numberOfParameters);
    scales.Fill(1.0f);
    SetScales(scales);
    m_ScalesInitialized = true;
    }

  this->Modified();
}

void
MultipleValuedNonLinearOptimizer
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  if ( m_CostFunction )
    {
    os << indent << "Cost Function: " << m_CostFunction.GetPointer() << std::endl;
    }
}
} // namespace itk

#endif

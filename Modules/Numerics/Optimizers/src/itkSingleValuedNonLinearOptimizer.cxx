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
#ifndef _itkSingleValuedNonLinearOptimizer_hxx
#define _itkSingleValuedNonLinearOptimizer_hxx

#include "itkSingleValuedNonLinearOptimizer.h"

namespace itk
{
SingleValuedNonLinearOptimizer
::SingleValuedNonLinearOptimizer()
{
  m_CostFunction = ITK_NULLPTR;
}

/**
 * Connect a Cost Function
 */
void
SingleValuedNonLinearOptimizer
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

/**
 * Get the cost function value at the given parameters
 */
SingleValuedNonLinearOptimizer::MeasureType
SingleValuedNonLinearOptimizer
::GetValue(const ParametersType & parameters) const
{
  itkDebugMacro("Computing CostFunction value at " <<  parameters);

  if ( !m_CostFunction )
    {
    ExceptionObject ex;
    ex.SetLocation(__FILE__);
    ex.SetDescription("The costfunction must be set prior to calling GetValue");
    throw ex;
    }

  return this->GetCostFunction()->GetValue(parameters);
}

void
SingleValuedNonLinearOptimizer
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

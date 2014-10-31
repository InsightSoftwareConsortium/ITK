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
#ifndef _itkOptimizer_hxx
#define _itkOptimizer_hxx

#include "itkOptimizer.h"

namespace itk
{
/**
 * Constructor
 */

Optimizer
::Optimizer() :
  m_ScalesInitialized( false )
{
}

/**
 * Set scaling as an array of factors
 */
void
Optimizer
::SetScales(const ScalesType & scales)
{
  itkDebugMacro("setting scales to " <<  scales);
  m_Scales = scales;
  //Nearly every optimizer computes the inverse at each iteration.
  //provides 1 commone place where the inverse can be computes
  //and validated.
  m_InverseScales.SetSize(m_Scales.GetSize());

  for( unsigned int i = 0; i < m_Scales.size(); ++i )
    {
    if ( m_Scales[i] < NumericTraits<double>::epsilon() )
      {
      itkExceptionMacro("ERROR: Scales must have value greater than epsilon! Scale[" << i << "] = " << m_Scales[i] );
      }
    m_InverseScales[i] = NumericTraits<double>::OneValue() / m_Scales[i];
    }
  m_ScalesInitialized = true;
  this->Modified();
}

/**
 * Set the initial position
 */
void
Optimizer
::SetInitialPosition(const ParametersType & param)
{
  m_InitialPosition = param;
  this->Modified();
}

/**
 * Set the current position
 */
void
Optimizer
::SetCurrentPosition(const ParametersType &  param)
{
  m_CurrentPosition = param;
  this->Modified();
}

const std::string
Optimizer
::GetStopConditionDescription() const
{
  std::ostringstream description;

  description << this->GetNameOfClass() << ": "
              << "Optimizer did not provide a stop condition description";
  return description.str();
}

/**
 * Print Self method
 */
void
Optimizer
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "InitialPosition: "
     << m_InitialPosition << std::endl;
  os << indent << "CurrentPosition: "
     << m_CurrentPosition << std::endl;

  if ( m_ScalesInitialized )
    {
    os << indent << "Scales: " << m_Scales << std::endl;
    os << indent << "InverseScales: " << m_InverseScales << std::endl;
    }
  else
    {
    os << indent << "Scales: not defined (default 1)" << std::endl;
    }

  os << indent << "StopConditionDescription: "
     << this->GetStopConditionDescription() << std::endl;
}
} // end namespace itk

#endif

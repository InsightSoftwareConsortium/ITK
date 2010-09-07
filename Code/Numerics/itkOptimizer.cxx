/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkOptimizer.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkOptimizer_txx
#define _itkOptimizer_txx

#include "itkOptimizer.h"

namespace itk
{
/**
 * Constructor
 */

Optimizer
::Optimizer()
{
  m_ScalesInitialized = false;
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
    os << indent << "Scales: "
       << m_Scales << std::endl;
    }
  else
    {
    os << indent << "Scales: not defined (default 1)"
       << std::endl;
    }

  os << indent << "StopConditionDescription: "
     << this->GetStopConditionDescription() << std::endl;
}
} // end namespace itk

#endif

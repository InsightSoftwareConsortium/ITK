/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaborKernelFunction.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkGaborKernelFunction.h"

namespace itk
{
GaborKernelFunction::GaborKernelFunction()
{
  this->m_CalculateImaginaryPart = false;
  this->m_Sigma = 1.0;
  this->m_Frequency = 0.4;
  this->m_PhaseOffset = 0.0;
}

GaborKernelFunction::~GaborKernelFunction()
{}

void GaborKernelFunction::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Sigma: " << this->GetSigma() << std::endl;
  os << indent << "Frequency: " << this->GetFrequency() << std::endl;
  os << indent << "PhaseOffset: " << this->GetPhaseOffset() << std::endl;
  os << indent << "CalculateImaginaryPart: " << this->GetCalculateImaginaryPart() << std::endl;
}
} // namespace itk

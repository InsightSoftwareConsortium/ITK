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

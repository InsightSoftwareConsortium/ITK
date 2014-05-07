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
#include "itkKLMSegmentationBorder.h"

namespace itk
{
KLMSegmentationBorder
::KLMSegmentationBorder(void)
{
  m_Lambda = 0.0;
  m_Region1 = ITK_NULLPTR;
  m_Region2 = ITK_NULLPTR;
}

KLMSegmentationBorder
::~KLMSegmentationBorder()
{}

/**
 * PrintSelf
 */
void
KLMSegmentationBorder
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Lambda  = " << m_Lambda << std::endl;
  os << indent << "Region1 = " << m_Region1 << std::endl;
  os << indent << "Region2 = " << m_Region2 << std::endl;
} // end PrintSelf

void
KLMSegmentationBorder
::SetRegion1(KLMSegmentationRegion *Region1)
{
  m_Region1 = Region1;
} // end SetRegion1

KLMSegmentationRegion *
KLMSegmentationBorder
::GetRegion1()
{
  return m_Region1;
} // end GetRegion2

void
KLMSegmentationBorder
::SetRegion2(KLMSegmentationRegion *Region2)
{
  m_Region2 = Region2;
} // end SetRegion2

KLMSegmentationRegion *
KLMSegmentationBorder
::GetRegion2()
{
  return m_Region2;
} // end GetRegion2

void
KLMSegmentationBorder
::EvaluateLambda()
{
  m_Lambda = m_Region1->EnergyFunctional(m_Region2) / this->GetBorderLength();
} // end EvaluateLambda()

void
KLMSegmentationBorder
::PrintBorderInfo()
{
  itkDebugMacro(<< "------------------------------");
  itkDebugMacro(<< "Location      : " << this);
  itkDebugMacro(<< "Lambda        : " << m_Lambda);
  itkDebugMacro( << "Region1       : " << this->GetRegion1() );
  itkDebugMacro( << "Region 1 Label: " << ( this->GetRegion1()->GetRegionLabel() ) );
  itkDebugMacro( << "Region2       : " << this->GetRegion2() );
  itkDebugMacro( << "Region 2 Label: " << ( this->GetRegion2()->GetRegionLabel() ) );
  itkDebugMacro(<< "++++++++++++++++++++++++++++++");
  itkDebugMacro(<< "------------------------------");
  itkDebugMacro(<< "------------------------------");

  std::cout << "Location      : " << this << std::endl;
  std::cout << "Lambda        : " << m_Lambda << std::endl;
}
} // namespace itk

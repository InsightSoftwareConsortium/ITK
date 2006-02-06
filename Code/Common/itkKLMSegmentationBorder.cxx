/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKLMSegmentationBorder.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkKLMSegmentationBorder.h"

namespace itk
{


KLMSegmentationBorder
::KLMSegmentationBorder(void)
{
  m_Lambda = 0.0;
  m_Region1 = 0;
  m_Region2 = 0;
}


KLMSegmentationBorder
::~KLMSegmentationBorder()
{

}

/**
 * PrintSelf
 */
void
KLMSegmentationBorder
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
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

  m_Lambda = m_Region1->EnergyFunctional( m_Region2 ) / this->GetBorderLength();

} // end EvaluateLambda()


void
KLMSegmentationBorder
::PrintBorderInfo()
{
  itkDebugMacro(<< "------------------------------");
  itkDebugMacro(<< "Location      : " << this);
  itkDebugMacro(<< "Lambda        : " << m_Lambda);
  itkDebugMacro(<< "Region1       : " << this->GetRegion1());
  itkDebugMacro(<< "Region 1 Label: " << (this->GetRegion1()->GetRegionLabel()));
  itkDebugMacro(<< "Region2       : " << this->GetRegion2());
  itkDebugMacro(<< "Region 2 Label: " << (this->GetRegion2()->GetRegionLabel()));
  itkDebugMacro(<< "++++++++++++++++++++++++++++++" );
  itkDebugMacro(<< "------------------------------" );
  itkDebugMacro(<< "------------------------------" );

  std::cout << "Location      : " << this << std::endl;
  std::cout << "Lambda        : " << m_Lambda << std::endl;
}


} // namespace itk


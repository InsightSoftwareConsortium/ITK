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
#ifndef _itkKLMSegmentationBorder_cxx
#define _itkKLMSegmentationBorder_cxx

#include "itkKLMSegmentationBorder.h"

namespace itk
{


KLMSegmentationBorder
::KLMSegmentationBorder(void)
{

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
  os << indent << "Region border KLM object" << std::endl;

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
  // Get the regions corresponding to the border in question
  KLMSegmentationRegion *preg1 = this->GetRegion1();
  KLMSegmentationRegion *preg2 = this->GetRegion2();

  MeanRegionIntensityType region1Mean = preg1->GetMeanRegionIntensity();
  MeanRegionIntensityType region2Mean = preg2->GetMeanRegionIntensity();
  MeanRegionIntensityType region1_2MeanDiff = region1Mean - region2Mean;

  // Assuming equal weights to all the channels
  // FIXME: For different channel weights modify this part of the code.

  m_Lambda = region1_2MeanDiff.squared_magnitude();

  double region1Area = preg1->GetRegionArea();
  double region2Area = preg2->GetRegionArea();

  double scaleArea = ( region1Area * region2Area ) /
                     ( region1Area + region2Area );

  m_Lambda *= scaleArea / this->GetBorderLength();

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
} // end PrintBorderResults


} // namespace itk


#endif

/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKLMSegmentationBorder.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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

}// end PrintSelf


void
KLMSegmentationBorder
::SetRegion1(KLMSegmentationRegion *Region1)
{

  m_Region1 = Region1; 

}// end SetRegion1


KLMSegmentationRegion *
KLMSegmentationBorder
::GetRegion1()
{

  return m_Region1; 

}// end GetRegion2


void
KLMSegmentationBorder
::SetRegion2(KLMSegmentationRegion *Region2)
{

  m_Region2 = Region2; 

}// end SetRegion2


KLMSegmentationRegion *
KLMSegmentationBorder
::GetRegion2()
{

  return m_Region2; 

}// end GetRegion2


void
KLMSegmentationBorder
::EvaluateLambda()
{
  // Get the regions corresponding to the border in question
  KLMSegmentationRegion *preg1 = this->GetRegion1();
  KLMSegmentationRegion *preg2 = this->GetRegion2();

  VecDblType region1Mean = preg1->GetMeanRegionIntensity();
  VecDblType region2Mean = preg2->GetMeanRegionIntensity();
  VecDblType region1_2MeanDiff = region1Mean - region2Mean;

  // Eventhough this implementation uses the definition of a 
  // Matrix in reality it is a vector, hence number of colums
  // is 1.
  int numRows = region1_2MeanDiff.rows();
  for( int i = 0; i < numRows; i++ )
    region1_2MeanDiff[i][0] *= region1_2MeanDiff[i][0];

  int region1Area = preg1->GetRegionArea();
  int region2Area = preg2->GetRegionArea();
 
  double scaleArea = (( double )( region1Area * region2Area ) /
                      ( double )( region1Area + region2Area ) );

  VecDblType LambdaMat = 
    (scaleArea / this->GetBorderLength() ) * region1_2MeanDiff;
                         

  // Assuming equal weights to all the channels
  // FIXME: For different channel weights modify this part of the
  // code.
  m_Lambda = 0.0;
  for( int i = 0; i < numRows; i++ ) m_Lambda += LambdaMat[i][0];

}//end EvaluateLambda()


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
  
  
  std::cout << "------------------------------"<< std::endl;
  std::cout << "Location      : " << this << std::endl;
  std::cout << "Lambda        : " << m_Lambda << std::endl;
  std::cout << "Neighbor regions: " << (this->GetRegion1()->GetRegionLabel()) << 
    " - " << (this->GetRegion2()->GetRegionLabel()) << std::endl;
  std::cout << "Total neighbor area: " << 
    ( this->GetRegion1()->GetRegionArea() + this->GetRegion2()->GetRegionArea() ) <<
    std::endl;
  std::cout << "++++++++++++++++++++++++++++++"  << std::endl;
  std::cout << "                             "  << std::endl;


}//end PrintBorderResults


} // namespace itk











#endif

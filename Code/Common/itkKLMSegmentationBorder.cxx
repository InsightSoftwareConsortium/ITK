/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkKLMSegmentationBorder.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

}//end PrintBorderResults


} // namespace itk











#endif

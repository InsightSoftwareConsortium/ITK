/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTimeProbe.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkTimeProbe.h"

namespace itk
{

TimeProbe
::TimeProbe()
{
  m_TotalTicks    = 0;
  m_Start         = 0;
  m_NumberOfCalls = 0;
}


TimeProbe
::~TimeProbe()
{
}



void 
TimeProbe
::Start(void)
{
  m_NumberOfCalls++;
  m_Start = clock();
}
 


void 
TimeProbe
::Stop(void)
{
  m_TotalTicks += clock() - m_Start;
}


    

TimeProbe::CountType
TimeProbe
::GetNumberOfCalls(void) const
{
  return m_NumberOfCalls;
}



double
TimeProbe
::GetMeanTime(void) const
{
  const double seconds =  
        static_cast<double>(m_TotalTicks) /
        static_cast<double>(CLOCKS_PER_SEC);

  const double meanTime = seconds / m_NumberOfCalls;
  return meanTime;
}



} // end namespace itk



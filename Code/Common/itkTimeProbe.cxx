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
  m_TotalTicks      = 0;
  m_Start           = 0;
  m_NumberOfStarts  = 0;
  m_NumberOfStops   = 0;
}


TimeProbe
::~TimeProbe()
{
}



void 
TimeProbe
::Start(void)
{
  m_NumberOfStarts++;
  m_Start = clock();
}
 


void 
TimeProbe
::Stop(void)
{
  m_TotalTicks += clock() - m_Start;
  m_NumberOfStops++;
}


    

TimeProbe::CountType
TimeProbe
::GetNumberOfStarts(void) const
{
  return m_NumberOfStarts;
}

    

TimeProbe::CountType
TimeProbe
::GetNumberOfStops(void) const
{
  return m_NumberOfStops;
}



double
TimeProbe
::GetMeanTime(void) const
{
  const double seconds =  
    static_cast<double>(m_TotalTicks) /
    static_cast<double>(CLOCKS_PER_SEC);

  double meanTime = 0.0f;

  if( m_NumberOfStops )
    {
    meanTime = seconds / m_NumberOfStops;
    }

  return meanTime;

}



} // end namespace itk



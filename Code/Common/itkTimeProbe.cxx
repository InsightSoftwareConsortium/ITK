/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTimeProbe.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkTimeProbe.h"
#include "itkNumericTraits.h"

namespace itk
{

TimeProbe
::TimeProbe()
{
  m_TotalTime       = NumericTraits< TimeStampType >::ZeroValue();
  m_Start           = NumericTraits< TimeStampType >::ZeroValue();
  m_NumberOfStarts  = NumericTraits< CountType >::ZeroValue();
  m_NumberOfStops   = NumericTraits< CountType >::ZeroValue();
  m_RealTimeClock   = RealTimeClock::New();
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
  m_Start = m_RealTimeClock->GetTimeStamp();
}
 


void 
TimeProbe
::Stop(void)
{
  m_TotalTime += m_RealTimeClock->GetTimeStamp() - m_Start;
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



TimeProbe::TimeStampType
TimeProbe
::GetMeanTime(void) const
{
  TimeStampType meanTime = 0.0f;

  if( m_NumberOfStops )
    {
    meanTime = m_TotalTime / m_NumberOfStops;
    }

  return meanTime;

}



} // end namespace itk



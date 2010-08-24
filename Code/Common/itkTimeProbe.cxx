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

namespace itk
{
TimeProbe
::TimeProbe():ResourceProbe< TimeStampType, TimeStampType >("Time", "s")
{
  m_RealTimeClock   = RealTimeClock::New();
}

TimeProbe
::~TimeProbe()
{}

TimeProbe::TimeStampType
TimeProbe
::GetMeanTime(void) const
{
  return this->GetMean();
}

TimeProbe::TimeStampType
TimeProbe
::GetInstantValue(void) const
{
  return m_RealTimeClock->GetTimeStamp();
}
} // end namespace itk

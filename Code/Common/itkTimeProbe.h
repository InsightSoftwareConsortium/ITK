/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTimeProbe.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTimeProbe_H
#define __itkTimeProbe_H


#include <time.h>
#include "itkWin32Header.h"


namespace itk 
{
 
class ITKCommon_EXPORT TimeProbe
{

public:
  typedef unsigned long CountType;
  typedef clock_t       ClickType;

public:
  TimeProbe();
  ~TimeProbe();

  void Start(void);
  void Stop(void);

  CountType GetNumberOfStarts(void) const;
  CountType GetNumberOfStops(void) const;
  double    GetMeanTime(void) const;

private:

    ClickType   m_Start;
    ClickType   m_TotalTicks;
    CountType   m_NumberOfStarts;
    CountType   m_NumberOfStops;

};


}

#endif

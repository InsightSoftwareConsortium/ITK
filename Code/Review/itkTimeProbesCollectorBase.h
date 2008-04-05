/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTimeProbesCollectorBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTimeProbesCollectorBase_h_Review
#define __itkTimeProbesCollectorBase_h_Review


#include "itkMacro.h"
#include "itkTimeProbe.h"
#include "itkProbesCollectorBase.h"

namespace itk
{

/** \class TimeProbesCollectorBase
 *  \brief Class for aggregating a set of time probes. 
 *
 *  This class defines a set of TimeProbes and assign names to them.
 *  The user can start and stop each one of the probes by addressing them by name.
 *  
 *  \sa TimeProbe
 *  \sa RealTimeClock
 *
 */
class ITK_EXPORT TimeProbesCollectorBase :public ProbesCollectorBase<TimeProbe>
{
public:
  /** destructor */ 
  virtual ~TimeProbesCollectorBase(){}
};

}

#endif //__itkTimeProbesCollectorBase_h_Review

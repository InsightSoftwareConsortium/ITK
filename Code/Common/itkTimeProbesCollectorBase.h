/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTimeProbesCollectorBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef TimeProbesCollectorBase_h
#define TimeProbesCollectorBase_h


#include "itkMacro.h"
#include "itkTimeProbe.h"
#include <map>
#include <string>

namespace itk
{


class TimeProbesCollectorBase 
{

public:

  typedef std::string                IdType;
  typedef std::map<IdType,TimeProbe> MapType;

  TimeProbesCollectorBase();
  ~TimeProbesCollectorBase();

  virtual void Start(const char *);
  virtual void Stop(const char *);
  virtual void Report(void) const;
  virtual void Clear(void);

protected:

  MapType   m_Probes;


};

}

#endif

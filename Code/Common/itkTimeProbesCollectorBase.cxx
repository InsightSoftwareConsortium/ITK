/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTimeProbesCollectorBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#include "itkTimeProbesCollectorBase.h"
#include <iostream>

namespace itk
{

TimeProbesCollectorBase
::TimeProbesCollectorBase()
{
}



TimeProbesCollectorBase
::~TimeProbesCollectorBase()
{
}




void 
TimeProbesCollectorBase
::Start(const char * id)
{
  IdType tid = id;
  MapType::iterator  pos = m_Probes.find( tid );
  if ( pos == m_Probes.end() )
    {
    m_Probes[ tid ] = TimeProbe();
    }
  m_Probes[ tid ].Start(); 
}





void 
TimeProbesCollectorBase
::Stop(const char * id)
{
  IdType tid = id;
  MapType::iterator  pos = m_Probes.find( tid );
  if ( pos == m_Probes.end() )
    {
    return;
    }
  pos->second.Stop(); 
}



void 
TimeProbesCollectorBase
::Report(void) const
{
  MapType::const_iterator probe = m_Probes.begin();
  MapType::const_iterator end   = m_Probes.end();
  while( probe != end )
  {
    std::cout << "Probe: " << probe->first << std::endl;
    std::cout << "Calls: " << probe->second.GetNumberOfCalls() << std::endl;
    std::cout << "Time:  " << probe->second.GetMeanTime() << std::endl;
    probe++;
  }
}
    
   


void 
TimeProbesCollectorBase
::Clear(void) 
{
  m_Probes.clear();
}


} // end namespace itk



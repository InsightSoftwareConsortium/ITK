/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProbesCollectorBase.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkProbesCollectorBase_txx
#define __itkProbesCollectorBase_txx

#include "itkProbesCollectorBase.h"
#include <iostream>

namespace itk
{

template <class TProbe>
ProbesCollectorBase<TProbe>
::~ProbesCollectorBase()
{
}

template <class TProbe>
void 
ProbesCollectorBase<TProbe>
::Start(const char * id)
{
  // if the probe does not exist yet, it is created.
  m_Probes[ id ].Start(); 
}

template <class TProbe>
void 
ProbesCollectorBase<TProbe>
::Stop(const char * id)
{
  IdType tid = id;
  typename MapType::iterator  pos = m_Probes.find( tid );
  if ( pos == m_Probes.end() )
    {
    itkGenericExceptionMacro( << "The probe \""<< id<< "\" does not exist. It can not be stopped." );
    return;
    }
  pos->second.Stop();
}


template <class TProbe>
void 
ProbesCollectorBase<TProbe>
::Report( std::ostream & os ) const
{
  typename MapType::const_iterator probe = m_Probes.begin();
  typename MapType::const_iterator end   = m_Probes.end();

  if ( probe == end )
    return;

  os.width(20);
  os <<  " Probe Tag ";
  os.width(10);
  os <<  " Starts ";
  os.width(10);
  os <<  " Stops  ";
  os.width(15);
  os <<  probe->second.GetType() << " (" << probe->second.GetUnit() << ")";
  os << std::endl;
  
  while( probe != end )
    {
    os.width(20);
    os <<  probe->first << "  ";
    os.width(10);
    os <<  probe->second.GetNumberOfStarts() <<  "   ";
    os.width(10);
    os <<  probe->second.GetNumberOfStops() <<  "   ";
    os.width(15);
    os <<  probe->second.GetMean();
    os << std::endl;
    probe++;
    }

}
    
   

template <class TProbe>
void 
ProbesCollectorBase<TProbe>
::Clear(void) 
{
  m_Probes.clear();
}


} // end namespace itk

#endif //__itkProbesCollectorBase_txx

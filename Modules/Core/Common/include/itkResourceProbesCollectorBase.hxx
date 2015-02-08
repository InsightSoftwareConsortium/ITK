/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkResourceProbesCollectorBase_hxx
#define itkResourceProbesCollectorBase_hxx

#include "itkResourceProbesCollectorBase.h"
#include <iostream>

namespace itk
{

template< typename TProbe >
ResourceProbesCollectorBase< TProbe >
::~ResourceProbesCollectorBase()
{}

template< typename TProbe >
void
ResourceProbesCollectorBase< TProbe >
::Start(const char *id)
{
  // if the probe does not exist yet, it is created.
  this->m_Probes[id].Start();
}

template< typename TProbe >
void
ResourceProbesCollectorBase< TProbe >
::Stop(const char *id)
{
  IdType tid = id;

  typename MapType::iterator pos = this->m_Probes.find(tid);
  if ( pos == this->m_Probes.end() )
    {
    itkGenericExceptionMacro(<< "The probe \"" << id << "\" does not exist. It can not be stopped.");
    return;
    }
  pos->second.Stop();
}

template< typename TProbe >
void
ResourceProbesCollectorBase< TProbe >
::Report(std::ostream & os) const
{
  typename MapType::const_iterator probe = this->m_Probes.begin();
  typename MapType::const_iterator end   = this->m_Probes.end();

  if ( probe == end )
    {
    os << "No probes have been created" << std::endl;
    return;
    }

  os.width(20);
  os <<  " Probe Tag ";
  os.width(10);
  os <<  " Starts ";
  os.width(10);
  os <<  " Stops  ";
  os.width(15);
  os <<  probe->second.GetType() << " (" << probe->second.GetUnit() << ")";
  os << std::endl;

  while ( probe != end )
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

template< typename TProbe >
void
ResourceProbesCollectorBase< TProbe >
::Clear(void)
{
  this->m_Probes.clear();
}

} // end namespace itk

#endif //itkResourceProbesCollectorBase_hxx

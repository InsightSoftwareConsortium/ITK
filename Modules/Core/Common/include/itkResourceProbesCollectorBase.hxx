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
  this->m_Probes[id].SetNameOfProbe(id);
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
const TProbe &
ResourceProbesCollectorBase< TProbe >
::GetProbe(const char *id) const
{
  IdType tid = id;

  typename MapType::const_iterator pos = this->m_Probes.find(tid);
  if ( pos == this->m_Probes.end() )
    {
    itkGenericExceptionMacro(<< "The probe \"" << id << "\" does not exist.");
    }
  return pos->second;
}


template< typename TProbe >
void
ResourceProbesCollectorBase< TProbe >
::Report(std::ostream & os, bool printSystemInfo, bool printReportHead, bool useTabs)
{
  typename MapType::iterator probe = this->m_Probes.begin();
  typename MapType::const_iterator end   = this->m_Probes.end();

  if ( probe == end )
    {
    os << "No probes have been created" << std::endl;
    return;
    }

  bool firstProbe = true;
  while ( probe != end )
    {
    if(firstProbe)
      {
      probe->second.Report(os, printSystemInfo, printReportHead, useTabs);
      firstProbe = false;
      }
    else
      {
      probe->second.Report(os, false, false, useTabs);
      }

    ++probe;
    }
}


template< typename TProbe >
void
ResourceProbesCollectorBase< TProbe >
::Report(const char *name, std::ostream & os, bool printSystemInfo, bool printReportHead, bool useTabs)
{
  const IdType tid = name;

  typename MapType::iterator pos = this->m_Probes.find(tid);
  if ( pos == this->m_Probes.end() )
    {
    os << "The probe \"" << name << "\" does not exist. It's report is not available" <<std::endl;
    return;
    }

  pos->second.Report(os, printSystemInfo, printReportHead, useTabs);
}


template< typename TProbe >
void
ResourceProbesCollectorBase< TProbe >
::ExpandedReport(std::ostream & os, bool printSystemInfo, bool printReportHead, bool useTabs)
{
  typename MapType::iterator probe = this->m_Probes.begin();
  typename MapType::const_iterator end = this->m_Probes.end();

  if ( probe == end )
    {
    os << "No probes have been created" << std::endl;
    return;
    }

  bool firstProbe = true;
  while ( probe != end )
    {
    if(firstProbe)
      {
      probe->second.ExpandedReport(os, printSystemInfo, printReportHead, useTabs);
      firstProbe = false;
      }
    else
      {
      probe->second.ExpandedReport(os, false, false, useTabs);
      }

    ++probe;
    }
}


template< typename TProbe >
void
ResourceProbesCollectorBase< TProbe >
::ExpandedReport(const char *name, std::ostream & os, bool printSystemInfo, bool printReportHead, bool useTabs)
{
  const IdType tid = name;

  typename MapType::iterator pos = this->m_Probes.find(tid);
  if ( pos == this->m_Probes.end() )
    {
    os << "The probe \"" << name << "\" does not exist. It's report is not available" <<std::endl;
    return;
    }

  pos->second.ExpandedReport(os, printSystemInfo, printReportHead, useTabs);
}


template< typename TProbe >
void
ResourceProbesCollectorBase< TProbe >
::JSONReport(std::ostream & os, bool printSystemInfo)
{
  typename MapType::iterator probe = this->m_Probes.begin();
  typename MapType::const_iterator end = this->m_Probes.end();

  if ( probe == end )
    {
    os << "{ \"Status\": \"No probes have been created\" }" << std::endl;
    return;
    }

  os << "{\n";
  if (printSystemInfo)
    {
    os << "  \"SystemInformation\": ";
    probe->second.PrintJSONSystemInformation(os);
    os << ",\n";
    }
  os << "  \"Probes\": [\n";
  bool firstProbe = true;
  while ( probe != end )
    {
    if(firstProbe)
      {
      probe->second.JSONReport(os);
      firstProbe = false;
      }
    else
      {
      os << ",\n";
      probe->second.JSONReport(os);
      }

    ++probe;
    }
  os << "\n  ]\n}" << std::endl;
}


template< typename TProbe >
void
ResourceProbesCollectorBase< TProbe >
::JSONReport(const char *name, std::ostream & os)
{
  const IdType tid = name;

  typename MapType::iterator pos = this->m_Probes.find(tid);
  if ( pos == this->m_Probes.end() )
    {
    os << "  { \"ProbeName\": \"" << name << "\", \"Status\": \"Does not exist!\" }" <<std::endl;
    return;
    }

  pos->second.JSONReport(os);
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

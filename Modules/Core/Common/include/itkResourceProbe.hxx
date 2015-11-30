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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkResourceProbe_hxx
#define itkResourceProbe_hxx

#include <numeric>
#include <iomanip>
#include <sstream>
#include <algorithm>
#include <functional>

#include "itkResourceProbe.h"
#include "itkNumericTraits.h"
#include "itkMultiThreader.h"
#include "itksys/SystemInformation.hxx"

namespace itk
{

template< typename ValueType, typename MeanType >
ResourceProbe< ValueType, MeanType >
::ResourceProbe(const std::string & type, const std::string & unit):
  m_TypeString(type), m_UnitString(unit)
{
  this->RetrieveSystemInformation();
  this->Reset();
}


template< typename ValueType, typename MeanType >
ResourceProbe< ValueType, MeanType >
::~ResourceProbe()
{}


template< typename ValueType, typename MeanType >
void
ResourceProbe< ValueType, MeanType >
::Reset(void)
{
  this->m_TotalValue        = NumericTraits< ValueType >::ZeroValue();
  this->m_StartValue        = NumericTraits< ValueType >::ZeroValue();
  this->m_MinimumValue      = NumericTraits< ValueType >::max();
  this->m_MaximumValue      = NumericTraits< ValueType >::min();
  this->m_MeanValue         = NumericTraits< MeanType >::ZeroValue();
  this->m_StandardDeviation = NumericTraits< ValueType >::ZeroValue();

  this->m_NumberOfStarts    = NumericTraits< CountType >::ZeroValue();
  this->m_NumberOfStops     = NumericTraits< CountType >::ZeroValue();
  this->m_NumberOfIteration = NumericTraits< CountType >::ZeroValue();

  this->m_ProbeValueList.clear();
}


template< typename ValueType, typename MeanType >
std::string
ResourceProbe< ValueType, MeanType >
::GetType(void) const
{
  return this->m_TypeString;
}


template< typename ValueType, typename MeanType >
std::string
ResourceProbe< ValueType, MeanType >
::GetUnit(void) const
{
  return this->m_UnitString;
}


template< typename ValueType, typename MeanType >
void
ResourceProbe< ValueType, MeanType >
::Start(void)
{
  this->m_NumberOfStarts++;
  this->m_StartValue = this->GetInstantValue();
}


template< typename ValueType, typename MeanType >
void
ResourceProbe< ValueType, MeanType >
::Stop(void)
{
  ValueType probevalue = this->GetInstantValue() - this->m_StartValue;
  if ( this->m_NumberOfStops == this->m_NumberOfStarts )
    {
    return;
    }

  this->UpdateMinimumMaximumMeasuredValue(probevalue);
  this->m_TotalValue += probevalue;
  this->m_ProbeValueList.push_back(probevalue);
  this->m_NumberOfStops++;
  this->m_NumberOfIteration = static_cast<CountType>(this->m_ProbeValueList.size());
}


template< typename ValueType, typename MeanType >
typename ResourceProbe< ValueType, MeanType >::CountType
ResourceProbe< ValueType, MeanType >
::GetNumberOfStarts(void) const
{
  return this->m_NumberOfStarts;
}


template< typename ValueType, typename MeanType >
typename ResourceProbe< ValueType, MeanType >::CountType
ResourceProbe< ValueType, MeanType >
::GetNumberOfStops(void) const
{
  return this->m_NumberOfStops;
}


template< typename ValueType, typename MeanType >
typename ResourceProbe< ValueType, MeanType >::CountType
ResourceProbe< ValueType, MeanType >
::GetNumberOfIteration(void) const
{
  return this->m_NumberOfIteration;
}


template< typename ValueType, typename MeanType >
ValueType
ResourceProbe< ValueType, MeanType >
::GetTotal(void) const
{
  return this->m_TotalValue;
}


template< typename ValueType, typename MeanType >
MeanType
ResourceProbe< ValueType, MeanType >
::GetMean(void) const
{
  MeanType meanValue = NumericTraits< MeanType >::ZeroValue();

  if ( this->m_NumberOfStops )
    {
    meanValue = static_cast< MeanType >( this->m_TotalValue ) / static_cast< MeanType >( this->m_NumberOfStops );
    }

  return meanValue;
}


template< typename ValueType, typename MeanType >
ValueType
ResourceProbe< ValueType, MeanType >
::GetMinimum() const
{
  return this->m_MinimumValue;
}


template< typename ValueType, typename MeanType >
ValueType
ResourceProbe< ValueType, MeanType >
::GetMaximum() const
{
  return this->m_MaximumValue;
}


template< typename ValueType, typename MeanType >
ValueType
ResourceProbe< ValueType, MeanType >
::GetStandardDeviation()
{
  this->m_MeanValue = this->GetMean();
  std::vector<ValueType> diff(this->m_ProbeValueList.size());
  std::transform(this->m_ProbeValueList.begin(),
                 this->m_ProbeValueList.end(),
                 diff.begin(),
                 std::bind2nd(std::minus<ValueType>(),
                              static_cast<ValueType>(this->m_MeanValue ) ));
  ValueType sqsum =
    std::inner_product(diff.begin(),diff.end(),
                       diff.begin(),
                       NumericTraits< ValueType >::ZeroValue());

  int sz = static_cast<int>(this->m_ProbeValueList.size())-1;
  if (sz <=0)
    {
    this->m_StandardDeviation = NumericTraits< ValueType >::ZeroValue();
    }
  else
    {
    this->m_StandardDeviation =
      static_cast<ValueType>(std::sqrt(static_cast<double>(sqsum /(static_cast<ValueType>(sz)))));
    }
  return this->m_StandardDeviation;
}


template< typename ValueType, typename MeanType >
void
ResourceProbe< ValueType, MeanType >
::SetNameOfProbe(const char* nameOfProbe)
{
  this->m_NameOfProbe = nameOfProbe;
}


template< typename ValueType, typename MeanType >
std::string
ResourceProbe< ValueType, MeanType >
::GetNameOfProbe() const
{
  return this->m_NameOfProbe;
}

template< typename ValueType, typename MeanType >
void
ResourceProbe< ValueType, MeanType >
::PrintSystemInformation(std::ostream & os)
{
  os << "System:              " << m_ITKSystemInfomation.SystemName << std::endl;
  os << "Processor:           " << m_ITKSystemInfomation.ProcessorName << std::endl;
  os << "    Cache:           " << m_ITKSystemInfomation.ProcessorCacheSize << std::endl;
  os << "    Clock:           " << m_ITKSystemInfomation.ProcessorClockFrequency << std::endl;
  os << "    Cores:           " << m_ITKSystemInfomation.NumberOfPhysicalCPU
     << " cpus x " << m_ITKSystemInfomation.NumberOfLogicalCPU
     << " Cores = "<< m_ITKSystemInfomation.NumberOfAvailableCore << std::endl;
  // Retrieve memory information in megabyte.
  os << "    Virtual Memory:  Total: "
     << m_ITKSystemInfomation.TotalVirtualMemory
     <<" Available: "<< m_ITKSystemInfomation.AvailableVirtualMemory << std::endl;
  os << "    Physical Memory: Total:"
     << m_ITKSystemInfomation.TotalPhysicalMemory
     <<" Available: "<< m_ITKSystemInfomation.AvailablePhysicalMemory << std::endl;

  os << "OSName:              "<< m_ITKSystemInfomation.OSName << std::endl;
  os << "    Release:         "<< m_ITKSystemInfomation.OSRelease << std::endl;
  os << "    Version:         "<< m_ITKSystemInfomation.OSVersion << std::endl;
  os << "    Platform:        "<< m_ITKSystemInfomation.OSPlatform << std::endl;

  os << "    Operating System is "
     << (m_ITKSystemInfomation.Is64Bits?"64 bit":"32 bit") << std::endl;

  os << "ITK Version: "
     << m_ITKSystemInfomation.ITKVersion << std::endl;
}


template< typename ValueType, typename MeanType >
void
ResourceProbe< ValueType, MeanType >
::Report(std::ostream & os, bool printSystemInfo, bool printReportHead )
{
  if(printSystemInfo)
    {
    this->PrintSystemInformation(os);
    }

  if(printReportHead)
    {
    this->PrintReportHead(os);
    }

  std::stringstream ss;
  ss << std::left << std::setw( tabwidth *2 ) << this->m_NameOfProbe
     << std::left << std::setw( tabwidth    ) << this->m_NumberOfIteration
     << std::left << std::setw( tabwidth    ) << this->GetTotal()
     << std::left << std::setw( tabwidth    ) << this->GetMinimum()
     << std::left << std::setw( tabwidth    ) << this->GetMean()
     << std::left << std::setw( tabwidth    ) << this->GetMaximum()
     << std::left << std::setw( tabwidth    ) << this->GetStandardDeviation();
  os << ss.str() << std::endl;
}


template< typename ValueType, typename MeanType >
void
ResourceProbe< ValueType, MeanType >
::ExpandedReport(std::ostream & os, bool printSystemInfo, bool printReportHead )
{
  if(printSystemInfo)
    {
    this->PrintSystemInformation(os);
    }

  if(printReportHead)
    {
    this->PrintExpandedReportHead(os);
    }

  std::stringstream ss;
  ss << std::left << std::setw( tabwidth *2 ) << this->m_NameOfProbe
     << std::left << std::setw( tabwidth    ) << this->m_NumberOfIteration
     << std::left << std::setw( tabwidth    ) << this->GetTotal()
     << std::left << std::setw( tabwidth    ) << this->GetMinimum()
     << std::left << std::setw( tabwidth    ) << this->GetMean() - this->GetMinimum()
     << std::left << std::setw( tabwidth    ) << (this->GetMean()/this->GetMinimum())*100
     << std::left << std::setw( tabwidth    ) << this->GetMean()
     << std::left << std::setw( tabwidth    ) << this->GetMaximum() - this->GetMean()
     << std::left << std::setw( tabwidth    ) << (this->GetMaximum()/this->GetMean())*100
     << std::left << std::setw( tabwidth    ) << this->GetMaximum()
     << std::left << std::setw( tabwidth    ) << this->GetMaximum() - this->GetMinimum()
     << std::left << std::setw( tabwidth    ) << this->GetStandardDeviation();
  os << ss.str() << std::endl;
}

template< typename ValueType, typename MeanType >
const ITKSystemInfomation&
ResourceProbe< ValueType, MeanType >
::GetSystemInformation() const
{
  return m_ITKSystemInfomation;
}

template< typename ValueType, typename MeanType >
void
ResourceProbe< ValueType, MeanType >
::UpdateMinimumMaximumMeasuredValue(ValueType value)
{
  if(this->m_MinimumValue > value)
    {
    this->m_MinimumValue = value;
    }

  if(this->m_MaximumValue < value)
    {
    this->m_MaximumValue = value;
    }
}


template< typename ValueType, typename MeanType >
void
ResourceProbe< ValueType, MeanType >
::PrintReportHead(std::ostream & os)
{
  std::stringstream ss;
  ss << std::left << std::setw( tabwidth *2 ) << std::string("Name Of Probe(")+this->m_TypeString + std::string(")")
     << std::left << std::setw( tabwidth    ) << "Iteration"
     << std::left << std::setw( tabwidth    ) << std::string("Total(") + this->m_UnitString + std::string(")")
     << std::left << std::setw( tabwidth    ) << std::string("Min(") + this->m_UnitString + std::string(")")
     << std::left << std::setw( tabwidth    ) << std::string("Mean(") + this->m_UnitString + std::string(")")
     << std::left << std::setw( tabwidth    ) << std::string("Max(") + this->m_UnitString + std::string(")")
     << std::left << std::setw( tabwidth    ) << std::string("Std(") + this->m_UnitString + std::string(")");

  os << ss.str() << std::endl;
}


template< typename ValueType, typename MeanType >
void
ResourceProbe< ValueType, MeanType >
::PrintExpandedReportHead(std::ostream & os)
{
  std::stringstream ss;
  ss << std::left << std::setw( tabwidth *2 ) << std::string("Name Of Probe(") + this->m_TypeString + std::string(")")
     << std::left << std::setw( tabwidth    ) << "Iteration"
     << std::left << std::setw( tabwidth    ) << std::string("Total(") + this->m_UnitString + std::string(")")
     << std::left << std::setw( tabwidth    ) << std::string("Min(") + this->m_UnitString + std::string(")")
     << std::left << std::setw( tabwidth    ) << "Mean-Min(diff)"
     << std::left << std::setw( tabwidth    ) << "Mean/Min(%)"
     << std::left << std::setw( tabwidth    ) << std::string("Mean(") + this->m_UnitString + std::string(")")
     << std::left << std::setw( tabwidth    ) << "Max-Mean(diff)"
     << std::left << std::setw( tabwidth    ) << "Max/Mean(%)"
     << std::left << std::setw( tabwidth    ) << std::string("Max(") + this->m_UnitString + std::string(")")
     << std::left << std::setw( tabwidth    ) << std::string("Total Diff(") + this->m_UnitString + std::string(")")
     << std::left << std::setw( tabwidth    ) << std::string("Std(") + this->m_UnitString + std::string(")");

  os << ss.str() << std::endl;
}


template< typename ValueType, typename MeanType >
void
ResourceProbe< ValueType, MeanType >
::RetrieveSystemInformation()
{
  itksys::SystemInformation systeminfo;
  systeminfo.RunCPUCheck();
  systeminfo.RunMemoryCheck();
  systeminfo.RunOSCheck();

  m_ITKSystemInfomation.SystemName              = systeminfo.GetHostname();
  m_ITKSystemInfomation.ProcessorName           = systeminfo.GetExtendedProcessorName();
  m_ITKSystemInfomation.ProcessorCacheSize      = static_cast<SizeValueType>(systeminfo.GetProcessorCacheSize());
  m_ITKSystemInfomation.ProcessorClockFrequency = systeminfo.GetProcessorClockFrequency();
  m_ITKSystemInfomation.NumberOfPhysicalCPU     = systeminfo.GetNumberOfPhysicalCPU();
  m_ITKSystemInfomation.NumberOfLogicalCPU      = systeminfo.GetNumberOfLogicalCPU();
  m_ITKSystemInfomation.NumberOfAvailableCore   = m_ITKSystemInfomation.NumberOfPhysicalCPU*m_ITKSystemInfomation.NumberOfLogicalCPU;

  m_ITKSystemInfomation.OSName                  = systeminfo.GetOSName();
  m_ITKSystemInfomation.OSRelease               = systeminfo.GetOSRelease();
  m_ITKSystemInfomation.OSVersion               = systeminfo.GetOSVersion();
  m_ITKSystemInfomation.OSPlatform              = systeminfo.GetOSPlatform();

  m_ITKSystemInfomation.Is64Bits                = systeminfo.Is64Bits();
  std::ostringstream        itkversion;
  itkversion << ITK_VERSION_MAJOR << "." << ITK_VERSION_MINOR << "." << ITK_VERSION_PATCH;
  m_ITKSystemInfomation.ITKVersion              = itkversion.str();

 // Retrieve memory information in megabyte.
  m_ITKSystemInfomation.TotalVirtualMemory      = systeminfo.GetTotalVirtualMemory();
  m_ITKSystemInfomation.AvailableVirtualMemory  = systeminfo.GetAvailableVirtualMemory();
  m_ITKSystemInfomation.TotalPhysicalMemory     = systeminfo.GetTotalPhysicalMemory();
  m_ITKSystemInfomation.AvailablePhysicalMemory = systeminfo.GetAvailablePhysicalMemory();
}

} // end namespace itk

#endif

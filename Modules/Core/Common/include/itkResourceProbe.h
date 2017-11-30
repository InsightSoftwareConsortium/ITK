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
#ifndef itkResourceProbe_h
#define itkResourceProbe_h

#include "itkMacro.h"
#include "itkIntTypes.h"

#include <string>
#include <vector>

#include "ITKCommonExport.h"

namespace itk
{
/** \class ResourceProbe
  *  \brief Computes the change of a value between two points in code.
  *
  *   This class is the base class of all the probes (time, memory, etc.)
  *   between the execution of two pieces of code. It can be started and
  *   stopped in order to evaluate the execution over multiple passes.
  *
  *   \sa TimeResourceProbe, MemoryResourceProbe
  *
  * \ingroup ITKCommon
  */
template< typename ValueType, typename MeanType >
class ITK_TEMPLATE_EXPORT ResourceProbe
{
public:

  /** Type for counting how many times the probe has been started and stopped.
    */
  typedef  SizeValueType  CountType;

public:

  /** Constructor */
  ResourceProbe(const std::string & type, const std::string & unit);

  /** Destructor */
  virtual ~ResourceProbe();

  /** Returns the type probed value */
  std::string GetType() const;

  /** Returns the unit probed value */
  std::string GetUnit() const;

  /** Start counting the change of value */
  virtual void Start();

  /** Stop counting the change of value.
   *
   * If a matching Start() has not been called before, there is no
   * effect.
   **/
  virtual void Stop();

  /** Returns the number of times that the probe has been started */
  CountType   GetNumberOfStarts() const;

  /** Returns the number of times that the probe has been stopped */
  CountType   GetNumberOfStops() const;

  /** Returns the number of iteration of the probe */
  CountType   GetNumberOfIteration() const;

  /** Returns the instant value of the probed system.
   */
  virtual ValueType   GetInstantValue(void) const = 0;

  /** Returns the accumulated value changes between the starts and stops
   *  of the probe */
  virtual ValueType    GetTotal() const;

  /** Returns the average value changes between the starts and stops
   *  of the probe. Stop() has to be called at least once, returns 0 otherwise.
   */
  virtual MeanType     GetMean() const;

  /** Reset the probe */
  virtual void Reset();

  /** Returns the min value changes between the starts and stops
   *  of the probe */
  virtual ValueType GetMinimum() const;

  /** Returns the max value changes between the starts and stops
   *  of the probe */
  virtual ValueType GetMaximum() const;

  /** Returns the standard deviation value changes between the starts and stops
   *  of the probe. */
  virtual ValueType GetStandardDeviation();

  /** Returns the standard deviation value changes between the starts and stops
   *  of the probe. */
  virtual ValueType GetStandardError();

  /** Set name of probe */
  virtual void SetNameOfProbe(const char* nameOfProbe);

  /** Set name of probe */
  virtual std::string GetNameOfProbe() const;

  /** Print System information */
  virtual void PrintSystemInformation(std::ostream & os = std::cout);

  /** Print Probe Results. */
  virtual void Report(std::ostream & os = std::cout, bool printSystemInfo = true,
                      bool printReportHead = true, bool useTabs = false);

  /** Print Probe Results. */
  virtual void ExpandedReport(std::ostream & os = std::cout, bool printSystemInfo = true,
                              bool printReportHead = true, bool useTabs = false);

  /** Print Probe Results. */
  virtual void JSONReport(std::ostream & os = std::cout);

  /** Print Probe Results. */
  virtual void PrintJSONSystemInformation(std::ostream & os = std::cout);

protected:
  /** Update the Min and Max values with an input value */
  virtual void UpdateMinimumMaximumMeasuredValue(ValueType value);

  /** Print Probe Results. */
  virtual void PrintReportHead(std::ostream & os = std::cout, bool useTabs = false);

  /** Print Probe Results. */
  virtual void PrintExpandedReportHead(std::ostream & os = std::cout, bool useTabs = false);

  /** Prints a varName: varValue pair. */
  template<typename T>
  void PrintJSONvar(std::ostream & os, const char* varName, T varValue,
      unsigned indent = 4, bool comma = true);

  /** Get System information */
  virtual void GetSystemInformation();

private:

  ValueType                  m_StartValue;
  ValueType                  m_TotalValue;
  ValueType                  m_MinimumValue;
  ValueType                  m_MaximumValue;
  MeanType                   m_MeanValue;
  ValueType                  m_StandardDeviation;
  ValueType                  m_StandardError;

  CountType                  m_NumberOfStarts;
  CountType                  m_NumberOfStops;
  CountType                  m_NumberOfIteration;

  std::vector<ValueType>     m_ProbeValueList;

  std::string                m_NameOfProbe;
  std::string                m_TypeString;
  std::string                m_UnitString;

  std::string                m_SystemName;
  std::string                m_ProcessorName;
  int                        m_ProcessorCacheSize;
  float                      m_ProcessorClockFrequency;
  unsigned int               m_NumberOfPhysicalCPU;
  unsigned int               m_NumberOfLogicalCPU;
  std::string                m_OSName;
  std::string                m_OSRelease;
  std::string                m_OSVersion;
  std::string                m_OSPlatform;
  bool                       m_Is64Bits;
  std::string                m_ITKVersion;
  size_t                     m_TotalVirtualMemory;
  size_t                     m_AvailableVirtualMemory;
  size_t                     m_TotalPhysicalMemory;
  size_t                     m_AvailablePhysicalMemory;

  static ITK_CONSTEXPR_VAR unsigned int  tabwidth  = 15;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkResourceProbe.hxx"
#endif

#endif //itkResourceProbe_h

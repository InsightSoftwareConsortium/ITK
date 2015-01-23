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
class ResourceProbe
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
  void        Start();

  /** Stop counting the change of value.
   *
   * If a matching Start() has not been called before, there is no
   * effect.
   **/
  void        Stop();

  /** Returns the number of times that the probe has been started */
  CountType   GetNumberOfStarts() const;

  /** Returns the number of times that the probe has been stopped */
  CountType   GetNumberOfStops() const;

  /** Returns the instant value of the probed system.
   */
  virtual ValueType   GetInstantValue(void) const = 0;

  /** Returns the accumulated value changes between the starts and stops
   *  of the probe */
  ValueType    GetTotal() const;

  /** Returns the average value changes between the starts and stops
   *  of the probe. Stop() has to be called at least once, returns 0 otherwise.
   */
  MeanType    GetMean() const;

  /** Reset the probe */
  void        Reset();

private:

  ValueType m_StartValue;
  ValueType m_TotalValue;

  CountType m_NumberOfStarts;
  CountType m_NumberOfStops;

  std::string m_TypeString;
  std::string m_UnitString;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkResourceProbe.hxx"
#endif

#endif //itkResourceProbe_h

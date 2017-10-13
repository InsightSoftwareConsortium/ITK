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
#ifndef itkTimeProbe_h
#define itkTimeProbe_h

#include "itkConfigure.h"
#include "itkResourceProbe.h"
#include "itkRealTimeClock.h"

namespace itk
{
/** \class TimeProbe
 *
 *  \brief Computes the time passed between two points in code.
 *
 *   This class allows the user to trace the time passed between the execution
 *   of two pieces of code.  It can be started and stopped in order to evaluate
 *   the execution over multiple passes.  The values of time are taken from the
 *   RealTimeClock.
 *
 *   \sa RealTimeClock
 *
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Utilities/TimeProbe,Time probe}
 * \endwiki
 */
class ITKCommon_EXPORT TimeProbe:public
  ResourceProbe< RealTimeClock::TimeStampType, RealTimeClock::TimeStampType >
{
public:

  /** Type for counting how many times the probe has been started and stopped.
    */
  typedef unsigned long CountType;

  /** Type for measuring time. See the RealTimeClock class for details on the
   * precision and units of this clock signal */
  typedef RealTimeClock::TimeStampType TimeStampType;

public:

  /** Constructor */
  TimeProbe();

  /** Destructor */
  virtual ~TimeProbe() ITK_OVERRIDE;

  /** Get the current time.
   *  Warning: the returned value is not the elapsed time since the last Start() call.
   */
  virtual TimeStampType GetInstantValue(void) const ITK_OVERRIDE;

  /** Returns the average times passed between the starts and stops of the
   *  probe. See the RealTimeClock for details on the precision and units of
   *  this time value. Obsolete method kept for backward compatibility,
   *  use Probe::GetMean() instead.
   *  \deprecated
   */
  itkLegacyMacro(TimeStampType GetMeanTime(void) const);

  /** Get a handle to m_RealTimeClock. */
  itkGetConstObjectMacro( RealTimeClock, RealTimeClock );

private:
  RealTimeClock::Pointer m_RealTimeClock;
};
} // end namespace itk

#endif //itkTimeProbe_h

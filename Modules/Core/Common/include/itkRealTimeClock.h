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
#ifndef itkRealTimeClock_h
#define itkRealTimeClock_h

#include "itkMacro.h"
#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkRealTimeStamp.h"

namespace itk
{
/** \class RealTimeClock
 * \brief Provides a timestamp from a real-time clock.
 *
 * This class represents a real-time clock object
 * and provides a timestamp in platform-independent format.
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT RealTimeClock:public Object
{
public:
  using Self = RealTimeClock;
  using Superclass = Object;
  using Pointer = SmartPointer< Self >;
  using ConstPointer = SmartPointer< const Self >;

  /** Method for defining the name of the class */
  itkTypeMacro(RealTimeClock, Object);

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Define the type for the timestap */
  using TimeStampType = double;

  /** Define the type for the frequency of the clock */
  using FrequencyType = double;

  /** Returns a timestamp in seconds, e.g. 52.341243 seconds */
  TimeStampType GetTimeInSeconds() const;

  /** Returns the frequency of a clock */
  itkGetConstMacro(Frequency, FrequencyType);

  /** Get the time as a RealTimeStamp type. */
  RealTimeStamp GetRealTimeStamp() const;

protected:

  /** Constructor. */
  RealTimeClock();

  /** Destructor. */
  ~RealTimeClock() override;

  void PrintSelf(std::ostream & os, Indent indent) const override;

private:
  FrequencyType m_Frequency{1};
  TimeStampType m_Difference;
  TimeStampType m_Origin;

  // Returns a timestamp in a TimeStamp data structure.
  // We hide this method in the private section, because it returns the
  // modified time of the itk::Object. That modified time is ambiguous with
  // the role of the RealTimeStamp.
  const TimeStamp & GetTimeStamp() const override;
};
} // end of namespace itk

#endif  // itkRealTimeClock_h

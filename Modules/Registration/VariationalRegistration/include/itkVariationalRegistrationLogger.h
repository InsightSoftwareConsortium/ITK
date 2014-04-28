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
#ifndef __itkVariationalRegistrationLogger_h
#define __itkVariationalRegistrationLogger_h

#include "itkCommand.h"
#include "itkEventObject.h"

#include "itkVariationalRegistrationFilter.h"
#include "itkMultiResolutionVariationalRegistration.h"

#include <time.h>
#include <vector>

namespace itk
{

/** \class itk::VariationalRegistrationLogger
 *
 * TODO class documentation
 *
 *  \sa VariationalRegistrationFilter
 *
 *  \ingroup VariationalRegistration
 */
template <class TRegistrationFilter, class TMRFilter>
class ITK_EXPORT VariationalRegistrationLogger : public Command
{
public:
  /** Standard class typedefs. */
  typedef VariationalRegistrationLogger Self;
  typedef Command                       Superclass;
  typedef SmartPointer<Self>            Pointer;
  typedef SmartPointer<const Self>      ConstPointer;

  /** Registration and MR filter types */
  typedef TRegistrationFilter RegistrationFilterType;
  typedef TMRFilter           MRFilterType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  // TODO implement platform independent time measurement
  //  /** Write to log file. */
  //  itkSetMacro( WriteToLogFile, bool );
  //  itkGetMacro( WriteToLogFile, bool );
  //  itkBooleanMacro( WriteToLogFile );
  //
  //  /** Write to standard output. */
  //  itkSetMacro( WriteToStdOut, bool );
  //  itkGetMacro( WriteToStdOut, bool );
  //  itkBooleanMacro( WriteToStdOut );
  //
  //  /** \brief Method to start time measurement. */
  //  void InitializeTimeMeasurement();

  void
  Execute(itk::Object * caller, const itk::EventObject & event)
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * caller, const itk::EventObject & event);

protected:
  VariationalRegistrationLogger();
  ~VariationalRegistrationLogger();
  void
  PrintSelf(std::ostream & os, Indent indent) const;

  /** \brief Method to get time since initialization as a string. */
  char *
  GetMonotonicTime();

  /** \brief Method to get process time since initialization as a string. */
  char *
  GetProcessTime();

private:
  VariationalRegistrationLogger(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  //  bool   m_TimeIsInitialized;
  //
  //  /** \brief The starting time of the program. */
  //  timespec m_MonotonicStartTime;
  //
  //  /** \brief The current time of the program. */
  //  timespec m_MonotonicCurrentTime;
  //
  //  /** \brief The starting process time of the program. */
  //  timespec m_ProzessStartTime;
  //
  //  /** \brief The current process time of the program. */
  //  timespec m_ProcessCurrentTime;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVariationalRegistrationLogger.hxx"
#endif

#endif

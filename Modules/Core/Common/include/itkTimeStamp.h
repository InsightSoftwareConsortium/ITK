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
#ifndef itkTimeStamp_h
#define itkTimeStamp_h

#include "itkMacro.h"
#include "itkIntTypes.h"
#include "itkAtomicInt.h"

namespace itk
{
/** \class TimeStamp
 * \brief Generate a unique, increasing time value.
 *
 * TimeStamp records a unique time when the method Modified() is
 * executed. This time is guaranteed to be monotonically increasing.
 * Classes use this object to record modified and/or execution time.
 * There is built in support for the binary < and > comparison
 * operators between two TimeStamp objects.
 *
 * \warning On most platforms, this class uses a lock-free incremental
 * counter. The Modified function can safely  be called simultaneously
 * by multiple threads on different instances of the class. However,
 * calling the Modified function by different threads on the same
 * instance of the class can lead to some unexpected behavior. The
 * global counter will always be correct but the local m_ModifiedTime
 * might not (see
 * https://www.itk.org/mailman/private/insight-developers/2009-February/011732.html
 * for more detail).
 *
 * \ingroup ITKSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT TimeStamp
{
public:
  /** Standard class typedefs. */
  typedef TimeStamp Self;

  typedef AtomicInt< ModifiedTimeType > GlobalTimeStampType;

  /** Create an instance of this class. We don't want to use reference
   * counting. */
  static Self * New();

  /** Constructor must remain public because classes instantiate
   * TimeStamps implicitly in their construction.  */
  TimeStamp()
  { m_ModifiedTime = 0; }

  /** Destoy this instance. */
  void Delete()
  { delete this; }

  /** The class name as a string.  */
  static const char * GetNameOfClass()
  { return "TimeStamp"; }

  /** Set this objects time to the current time. The current time is just a
   * monotonically increasing unsigned long integer. It is possible for this
   * number to wrap around back to zero.  This should only happen for
   * processes that have been running for a very long time, while constantly
   * changing objects within the program. When this does occur, the typical
   * consequence should be that some filters will update themselves when
   * really they don't need to.   */
  void Modified();

  /** Return this object's Modified time.  */
  ModifiedTimeType GetMTime() const
  { return m_ModifiedTime; }

  /** Support comparisons of time stamp objects directly.  */
  bool operator>(TimeStamp & ts)
  { return ( m_ModifiedTime > ts.m_ModifiedTime ); }
  bool operator<(TimeStamp & ts)
  { return ( m_ModifiedTime < ts.m_ModifiedTime ); }

  /** Allow for typcasting to unsigned long.  */
  operator ModifiedTimeType() const
        { return m_ModifiedTime; }

  /** Assignment operator, allows to initialize one time stamp by copying from
   * another. */
  const Self & operator=( const Self & other );

  /** Set/Get the pointer to GlobalTimeStamp.
   * Note that SetGlobalTimeStamp is not concurrent thread safe. */
  static GlobalTimeStampType * GetGlobalTimeStamp();
  static void SetGlobalTimeStamp( GlobalTimeStampType * timeStamp );

private:
  ModifiedTimeType m_ModifiedTime;

  /** The static GlobalTimeStamp. This is initialized to NULL as the first
   * stage of static initialization. It is then populated on the first call to
   * itk::TimeStamp::Modified() but it can be overridden with SetGlobalTimeStamp().
   * */
  static GlobalTimeStampType * m_GlobalTimeStamp;
};
} // end namespace itk

#endif

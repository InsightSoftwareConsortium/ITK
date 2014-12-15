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
#ifndef itkIterationReporter_h
#define itkIterationReporter_h

#include "itkProcessObject.h"

namespace itk
{
/** \class IterationReporter
 * \brief Implements iterations tracking for a filter.
 *
 * This is a utility class for use by filter implementations in
 * GenerateData() and ThreadedGenerateData().
 *
 * This class is intended to be used in iterative filter for which
 * a progress cannot be stablished. These filters run until an stopping
 * criterion is reached and it is not possible to anticipate how long
 * it will take to get to the stopping point.
 *
 * This class is constructed before entering the iteration loop in the
 * filter. The CompletedStep() method should be called at every iteration.
 * The reporter will count the number of calls and will invoke an
 * IterationEvent every certain number of calls. The default period
 * is 100.
 *
 * Example usage:
 *
 *   IterationReporter iteration(this, threadId, 100);
 *
 *   for( each pixel )
 *     {
 *     ...
 *     iteration.CompletedStep();
 *     }
 *
 * When used in a non-threaded filter, the threadId argument should be 0.
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT IterationReporter
{
public:
  /** Constructor sets progress to 0 because the filter is starting.  */
  IterationReporter(ProcessObject *filter, ThreadIdType threadId,
                    unsigned long stepsPerUpdate = 100);

  /** Destructor */
  ~IterationReporter() {}

  /** Called by a filter once per iteration.  */
  void CompletedStep()
  {
    // Inline implementation for efficiency.
    // We don't need to test for thread id 0 here because the
    // constructor sets m_StepsBeforeUpdate to a value larger than
    // the number of pixels for threads other than 0.
    if ( --m_StepsBeforeUpdate == 0 )
      {
      m_StepsBeforeUpdate = m_StepsPerUpdate;
      m_Filter->InvokeEvent( IterationEvent() );
      }
  }

protected:
  ProcessObject *m_Filter;
  ThreadIdType   m_ThreadId;
  unsigned long  m_StepsPerUpdate;
  unsigned long  m_StepsBeforeUpdate;
};
} // end namespace itk

#endif

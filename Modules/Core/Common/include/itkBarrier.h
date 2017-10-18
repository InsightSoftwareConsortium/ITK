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
#ifndef itkBarrier_h
#define itkBarrier_h

#include "itkConditionVariable.h"

namespace itk
{
/**
 * \class Barrier
 * \brief Standard barrier class implementation for synchronizing the execution
 * of threads.
 *
 * A barrier class is used to synchronize threaded execution by allowing
 * threads to block until each has reached a desired state.  As each thread
 * enters the barrier it blocks. When all threads have entered the barrier,
 * all released and continue to execute.
 *
 * A thread enters the barrier by calling Wait() on the barrier class.
 * To set up a barrier class, call Initialize(n) where n is the number of
 * waiting threads that will trigger a release of the barrier.
 *
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT Barrier
  : public LightObject
{
public:
  /** Standard class typedefs. */
  typedef Barrier                    Self;
  typedef LightObject                Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Barrier, LightObject);

  /** Creates a new system variable used to implement the barrier.  The
      argument to this method is the number of threads that must Wait() on the
      barrier before it is cleared. */
  void Initialize(unsigned int);

  /** A thread calling this method waits until m_NumberOfThreads have called
   *  Wait() on the barrier.  When the final expected thread calls Wait(), all
   *  threads are released. */
  void Wait();

private:
  Barrier();
  ~Barrier() ITK_OVERRIDE;

  unsigned int               m_NumberArrived;
  unsigned int               m_NumberExpected;
  ConditionVariable::Pointer m_ConditionVariable;
  SimpleMutexLock            m_Mutex;
};
} // end namespace itk

#endif

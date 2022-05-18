/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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

#include "itkLightObject.h"
#include "itkObjectFactory.h"
#include "ITKDeprecatedExport.h"
#include <condition_variable>
#include <mutex>

namespace itk
{
/**
 * \class Barrier
 * \brief Standard barrier class implementation for synchronizing the execution
 * of threads.
 *
 * A barrier class is used to synchronize threaded execution by allowing
 * threads to block until each has reached a desired state. As each thread
 * enters the barrier it blocks. When all threads have entered the barrier,
 * all released and continue to execute.
 *
 * A thread enters the barrier by calling Barrier::Wait() on the barrier
 * class. To set up a barrier class, call Barrier::Initialize(unsigned int),
 * specifying the number of waiting threads that will trigger a release of the
 * barrier as the argument.
 *
 * NOTE: This class is only compatible with PlatformMultiThreader!
 *
 * \ingroup ITKDeprecated
 */
class ITKDeprecated_EXPORT Barrier : public LightObject
{
public:
  /** Standard class type aliases. */
  using Self = Barrier;
  using Superclass = LightObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Barrier, LightObject);

  /** Creates a new system variable used to implement the barrier.  The
      argument to this method is the number of threads that must Wait() on the
      barrier before it is cleared. */
  void
  Initialize(unsigned int);

  /** A thread calling this method waits until m_NumberOfThreads have called
   *  Wait() on the barrier.  When the final expected thread calls Wait(), all
   *  threads are released. */
  void
  Wait();

private:
  Barrier();
  ~Barrier() override;

  unsigned int            m_NumberArrived{ 0 };
  unsigned int            m_NumberExpected{ 0 };
  unsigned int            m_Generation{ 0 }; // Allows successive waits
  std::condition_variable m_ConditionVariable;
  std::mutex              m_Mutex;
};
} // end namespace itk

#endif

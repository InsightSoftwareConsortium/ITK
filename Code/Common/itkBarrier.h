/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBarrier.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBarrier_h_
#define __itkBarrier_h_

#include "itkLightObject.h"
#include "itkConditionVariable.h"
#include "itkMutexLock.h"

#ifdef ITK_USE_FETCHOP_BARRIERS
extern "C" {
#include <sys/pmo.h>
#include <fetchop.h>
}
#endif

namespace itk {

/**
 * \class Barrier
 * \brief Standard barrier class implementation for synchronizing the execution
 * of threads.
 *
 * A barrier class is used to synchronize threaded execution by allowing
 * threads to block until each has reached a desired state.  As each thread
 * enters the barrier it blocks. When all threads have entered the barrier,
 * all all released and continue to execute.
 *
 * A thread enters the barrier by calling Wait() on the barrier class.
 * To set up a barrier class, call Initialize(n) where n is the number of
 * waiting threads that will trigger a release of the barrier.
 *
 * \par NOTE FOR SGI USERS. You may optionally enable a fetchop library
 * implementation of barriers that will give significantly faster performance
 * over the sproc barrier class.  With the fetchop implementation, you are
 * limited to Barrier::m_MaxBarriers separate barrier instantiations, although
 * this limit can be safely raised if necessary.  To enable the fetchop
 * implementation, set ITK_USE_FETCHOP_BARRIERS and link applications against
 * -lfetchop.
 *
*/
class ITKCommon_EXPORT Barrier : public LightObject
{
public:
  /** Standard class typedefs. */
  typedef Barrier      Self;
  typedef LightObject  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Barrier, Object);

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
  ~Barrier();

#if defined ITK_USE_FETCHOP_BARRIERS
  static bool  m_ReservoirInitialized;
  static atomic_reservoir_t m_Reservoir;
  static int m_MaxBarriers;
  atomic_var_t *m_Pvar;

  char pad1[128];              // Attempt to put
  volatile int m_FetchopFlag;  // m_Fetchop on its
  char pad2[128];              // own cache line.

#elif defined ITK_USE_SPROC
  barrier_t *m_Barrier;

#else
  ConditionVariable::Pointer m_ConditionVariable;
  unsigned int m_NumberArrived;
  SimpleMutexLock m_Mutex;

#endif

  unsigned int m_NumberExpected;
};

} // end namespace itk

#endif

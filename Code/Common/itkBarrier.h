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

#ifdef ITK_USE_SPROC
extern "C" {
#include <sys/pmo.h>
#include <fetchop.h>
}
#include <ulocks.h>
#include <sys/sysmp.h>
#endif

#ifdef ITK_USE_SPROC 
#include <sys/types.h>
#include <sys/prctl.h>
#include <ulocks.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/signal.h>
#include <sys/sysmp.h>
#include <sys/errno.h>
#include <sys/syssgi.h>
#include <sys/time.h>
extern "C" {
#include <sys/pmo.h>
#include <fetchop.h>
}
#endif

namespace itk
{
struct Barrier_Private
{
  char Pad1[128];
  SimpleMutexLock m_Mutex;
  ConditionVariable::Pointer m_Cond0;
  ConditionVariable::Pointer m_Cond1;
  
  int m_NumberOfWaiters;
  
  unsigned long m_CycleCount;
  
#ifdef ITK_USE_SPROC
  barrier_t * m_Barrier; // MP library implementation
  atomic_var_t * pvar;   // fetchop library implementation
  
  char pad2[128];
  volatile int flag;  // We want this on it's own cache line
  char pad3[128];
#endif
  
  Barrier_Private()
  {
    m_Cond0= ConditionVariable::New();
    m_Cond1= ConditionVariable::New();
  };
};
  
/**
 * \class Barrier
 *
 * Barriers are supported on unix systems. On the SGI (IRIX) barriers have been
 * implemented using the fetchop library and (independently) using the SGI
 * arena. The algorithm will by default try to create a barrier using the
 * fetchop  library and only in case of failure will use the arena. The reason
 * that the fetchop implementation is given priority is that it is considered
 * to be more efficient. 
 */
class ITK_EXPORT Barrier : public LightObject
{
public:
 /** Standard class typedefs. */
  typedef Barrier     Self;
  typedef LightObject  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(Barrier, Object);

  /** Creates a new system variable used to implement the barrier */
  void Initialize(int count);
  
  /** Frees the system variable */
  void Remove();
  
  /** A thread calling this method waits until m_NumberOfThreads have called
      Wait() on the barrier */
  void Wait();
  
#ifdef ITK_USE_SPROC
  int m_NumberOfProcessors;
  
  static bool use_fetchop;
  static atomic_reservoir_t reservoir;
  static bool reservoir_initialized;
#endif
  
private:
  Barrier();
  ~Barrier();
  
  char pad1[128];
  Barrier_Private m_Barrier;
  unsigned long m_NumberOfThreads;
  char pad2[128];
};

}//itk

#endif

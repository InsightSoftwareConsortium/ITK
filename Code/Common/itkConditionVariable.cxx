/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConditionVariable.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef  _itkConditionVariable_cxx_
#define  _itkConditionVariable_cxx_

#include "itkConditionVariable.h"

namespace itk {
  
ConditionVariable::ConditionVariable()
{
  m_Sema= Semaphore::New();
  m_Sema->Initialize (0);
  
  m_Lock = MutexLock::New();
  m_NumberOfWaiters = 0;
#ifdef ITK_USE_PTHREADS
  pthread_mutex_init(&m_Mutex, NULL);
  pthread_cond_init(&m_Cond, NULL);
#endif
}
  
ConditionVariable::~ConditionVariable()
{
#ifdef ITK_USE_PTHREADS
  pthread_mutex_destroy(&m_Mutex);
  pthread_cond_destroy(&m_Cond);
  m_Sema->Remove();
#endif
}

void ConditionVariable::Signal()  
{
#ifdef ITK_USE_PTHREADS
  pthread_cond_signal(&m_Cond);
#else 
  
  m_Lock->Lock();
  if (m_NumberOfWaiters> 0)
    {
      m_NumberOfWaiters--;
      m_Sema->Up();
    }
  m_Lock->Unlock();
  
#endif
}

void ConditionVariable::Broadcast()
{
#ifdef ITK_USE_PTHREADS
  pthread_cond_broadcast(&m_Cond);
#else
  
  m_Lock->Lock();
  while (m_NumberOfWaiters > 0)
    {
      m_NumberOfWaiters--;
      m_Sema->Up();
    }
  m_Lock->Unlock();
  
#endif
}

void ConditionVariable::Wait(SimpleMutexLock * mutex)
{

#ifdef ITK_USE_PTHREADS
  pthread_cond_wait(&m_Cond, &mutex->GetMutexLock() );
#else
  m_Lock->Lock();
  m_NumberOfWaiters++;
  m_Lock->Unlock();  

  mutex->Unlock();
  m_Sema->Down();
  mutex->Lock();
#endif
}

}//end of namespace itk

#endif

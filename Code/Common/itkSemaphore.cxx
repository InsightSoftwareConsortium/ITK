/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSemaphore.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSemaphore_cxx_
#define __itkSemaphore_cxx_

#include "itkConfigure.h"
#include "itkSemaphore.h"

namespace itk {
  
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  int Semaphore::unix_semaphore_key = 12345;
  SimpleMutexLock Semaphore::m_Mutex;
#endif
  
Semaphore::Semaphore ()
{
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  m_Sema= -1;
#endif
  
#ifndef ITK_USE_UNIX_IPC_SEMAPHORES  

#ifdef ITK_USE_SPROC
  m_Sema = 0;
#endif

#endif
  
#ifdef ITK_USE_WIN32_THREADS
  m_Sema = 0;
#endif
}
  
void Semaphore::Initialize(unsigned int value)
{
  
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  std::cout << "Using IPC" << std::endl;
  // Obtain a lock over the 'unix_semaphore_key' so that the new semaphore is
  // created with a unique unix_semaphore_key 
  Semaphore::m_Mutex.Lock();
  m_Sema = Semaphore::UnixIpcSemaphoreCreate(Semaphore::unix_semaphore_key);

  // by default the semaphore created has value 0
  Semaphore::unix_semaphore_key++;
  Semaphore::m_Mutex.Unlock();
  
  for (unsigned int i = 0; i < value; i++)
    {
    this->Up();
    }
  
#endif
  
#ifndef ITK_USE_UNIX_IPC_SEMAPHORES  
#ifdef ITK_USE_SPROC
  std::cout << "using sproc" << std::endl;

  if (MultiThreader::GetInitialized() == false)
    {
      MultiThreader::Initialize();
    }

  m_Sema = usnewsema(MultiThreader::GetThreadArena(), static_cast<int>(value));
  if (!m_Sema )
    {
      itkExceptionMacro( << " sem_init call failed with code " << m_Sema );
    }
  
#endif
#ifdef ITK_USE_PTHREADS
  if ( sem_init(&m_Sema, 0, value) != 0 )
    {
      itkExceptionMacro( << "sem_init call failed" );
    }
#endif
#endif
  
#ifdef ITK_USE_WIN32_THREADS
  m_Sema = CreateSemaphore( 0, value, 0x7FFFFFFF, 0);
  if (m_Sema == 0)
    {
      itkExceptionMacro( << "CreateSemaphore call failed" );
    }
#endif
}
  
void Semaphore::Up()
{
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  Semaphore::UnixIpcSemaphoreUp(m_Sema);
#endif
  
#ifndef ITK_USE_UNIX_IPC_SEMAPHORES

#ifdef ITK_USE_SPROC
  if (usvsema(m_Sema) == -1)
    {
      itkExceptionMacro( << "usvsema call failed." );
    }
#endif
#ifdef ITK_USE_PTHREADS
  if ( sem_post(&m_Sema) != 0 )
    {
      itkExceptionMacro( << "sem_post call failed." );
    }
#endif

#endif  // ITK_USE_UNIX_SEMAPHORES
  
#ifdef ITK_USE_WIN32_THREADS
  if ( ! ReleaseSemaphore ((HANDLE) m_Sema, 1 , 0) )
    {
      itkExceptionMacro( << "Semaphore post call failed." );
    }
#endif
}

void Semaphore::Down()
{
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  Semaphore::UnixIpcSemaphoreDown (m_Sema);
#endif
  
#ifndef ITK_USE_UNIX_IPC_SEMAPHORES
#ifdef ITK_USE_SPROC
  if (uspsema(m_Sema) == -1)
    {
      itkExceptionMacro( << "uspsema call failed." );
    }
#endif
#ifdef ITK_USE_PTHREADS
  if (sem_wait(&m_Sema) != 0)
    {
      itkExceptionMacro( << "sem_wait call failed." );
    }
#endif
#endif

#ifdef ITK_USE_WIN32_THREADS
  if ( WaitForSingleObject(m_Sema, INFINITE) == WAIT_FAILED )
    {
      itkExceptionMacro( << "WaitForSingleObject call failed. ");
    }
#endif
}

Semaphore::~Semaphore()
{
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  if (m_Sema != -1)
    {
      this->Remove();
    }
#endif
  
#ifndef ITK_USE_UNIX_IPC_SEMAPHORES  
#ifdef ITK_USE_SPROC
  if (m_Sema != 0)
    {
      this->Remove();
    }
#endif
#ifdef ITK_USE_PTHREADS  
  this->Remove();
#endif
#endif
  
#ifdef ITK_USE_WIN32_THREADS
  if (m_Sema != 0)
    {
      this->Remove();
    }
#endif
}

void Semaphore::Remove ()
{
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  if (m_Sema != -1)
    {
      Semaphore::UnixIpcSemaphoreRemove(m_Sema);
      m_Sema= -1;
    }
#endif
  
#ifndef ITK_USE_UNIX_IPC_SEMAPHORES
#ifdef ITK_USE_SPROC
  if (MultiThreader::GetThreadArena() != 0)
    {
      if (m_Sema != 0)
  usfreesema (m_Sema, MultiThreader::GetThreadArena());
      m_Sema= 0;
    }
#endif
#ifdef ITK_USE_PTHREADS
  if ( sem_destroy(&m_Sema) != 0 )
    {
    itkExceptionMacro( << "sem_destroy call failed. " );
    }
#endif
#endif
  
#ifdef ITK_USE_WIN32_THREADS
  if (m_Sema != 0)
    CloseHandle(m_Sema);
  m_Sema= 0;
#endif
}

#ifdef ITK_USE_UNIX_IPC_SEMAPHORES

// UnixIpcSemaphoreCreate: will return the semaphore id (system wide)
// of the semaphore number (key) you give.
// If no semaphore has been established for this number, one is created.
int Semaphore::UnixIpcSemaphoreCreate(int unix_semaphore_key)
{
  int sid= -1;
  std::string s; 
 
  if( (sid = semget( (key_t)unix_semaphore_key, 1, 0666 | IPC_CREAT )) == -1 )
    {
    s =  "Error# %i in function UnixIpcSemaphoreCreate. - ";
    switch (errno)
      {
      case EEXIST:
        s += "Semaphore already exists. - ";
        break;
      case ENOSPC:
        s+= "System imposed limit on the number of semaphores is exceeded - ";
        break;
      case EACCES:
        s+= "Permission is denied - ";
        break;
      }
    itkExceptionMacro( << s.c_str() );
    }
  
  return( sid );
}

// UnixIpcSemaphoreDown: the semaphore signal operation.
// sid must be the system wide semaphore number
// returned by UnixIpcSemaphoreCreate above
void Semaphore::UnixIpcSemaphoreDown(int sid)
{
  Semaphore::UnixIpcSemaphoreCall(sid, -1);
}

// UnixIpcSemaphoreUp: the semaphore release operation.
// sid must be the system wide semaphore number
// returned by UnixIpcSemaphoreCreate above
void Semaphore::UnixIpcSemaphoreUp(int sid)
{
  Semaphore::UnixIpcSemaphoreCall(sid, 1);
}

// UixIpcSemaphoreRemove: remove a semaphore from the system.
// sid must be the system wide semaphore number
// returned from UnixIpcSemaphoreCreate
void Semaphore::UnixIpcSemaphoreRemove(int sid)
{
  std::string s;
  
  if( semctl( sid, 0, IPC_RMID, 0 ) == -1 )
    {
    s =  "Error removing semaphore %i - ";
    switch (errno)
      {
      case EINVAL:
        s +=  "Semaphore id# is not valid. - ";
        break;
      case EACCES:
        s+=  "Permission is denied - ";
        break;
      case EPERM:
        s+= "Permission is denied - ";
        break;
      }
    itkExceptionMacro ( << s.c_str() );
    }
}

// UnixIpcSemaphoreCall: makes the system call semop for your given
// semaphore and operation.
void Semaphore::UnixIpcSemaphoreCall (int sid, int op)
{
  struct sembuf sb;
  std::string s;
  
  sb.sem_num = 0;
  sb.sem_op = op;
  sb.sem_flg = 0;
  if( semop( sid, &sb, 1 ) == -1 )
    {
    
    if( op == -1 )
      {
      s =  "Error %i in UnixIpcSemaphoreDown call for semaphore %i - ";
      }
    else
      {
      s =  "Error %i in UnixIpcSemaphoreUp call for semaphore %i - ";
      }
    switch (errno)
      {
      case EINVAL:
        s +=  "Semaphore id# is not valid. -";
        break;
      case EFBIG:
        s +=  "Invalid sem_num for semaphore - ";
        break;
      case EACCES:
        s += "Permission denied for semaphore - ";
        break;
      case EAGAIN:
        s += "The operation would result in suspension of the calling process but NOWAIT is true.";
        break;
      case EIDRM:
        s += "The semid is removed from the system.";
        break;
      }
    itkExceptionMacro( << s.c_str() );
    }
}

#endif

}//end if namespace itk

#endif

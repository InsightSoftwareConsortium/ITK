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
#include "itkSemaphore.h"

#ifdef __APPLE__
extern "C" {
#include <stdio.h>
#include <time.h>
}
#endif

namespace itk
{
#ifdef __APPLE__
int Semaphore:: m_SemaphoreCount = 0;
#endif

#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
int Semaphore::             m_IPCSemaphoreKey = 12345;
SimpleMutexLock Semaphore:: m_Mutex;
#endif

#ifdef __APPLE__
std::string Semaphore::GetUniqueName()
{
  char   s[255];
  time_t t = time(ITK_NULLPTR);

  snprintf(s, 254, "MACSEM%d%d", static_cast< int >( t ), m_SemaphoreCount);
  return std::string(s);
}

#endif

Semaphore::Semaphore ()
{
  m_Pad1[0]='\0';
  m_Pad2[0]='\0';
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  m_Sema = -1;
#endif

#ifdef __APPLE__
  m_Sema = ITK_NULLPTR;
  m_SemaphoreCount++;
  m_SemaphoreName = Semaphore::GetUniqueName();
#endif

#ifdef ITK_USE_WIN32_THREADS
  m_Sema = 0;
#endif

#ifdef ITK_USE_PTHREADS
  m_PThreadsSemaphoreRemoved = false;
#endif
}

void Semaphore::Initialize(unsigned int value)
{
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  // Obtain a lock over the m_IPCSemaphoreKey so that the new semaphore is
  // created with a unique unix_semaphore_key
  Semaphore::m_Mutex.Lock();
  m_Sema = Semaphore::UnixIpcSemaphoreCreate(Semaphore::m_IPCSemaphoreKey);

  // by default the semaphore created has value 0
  Semaphore::m_IPCSemaphoreKey++;
  Semaphore::m_Mutex.Unlock();

  for ( unsigned int i = 0; i < value; i++ )
    {
    this->Up();
    }
#endif

#ifndef ITK_USE_UNIX_IPC_SEMAPHORES
#ifdef ITK_USE_PTHREADS

  m_PThreadsSemaphoreRemoved = false;

#if defined sun
  if ( sema_init(&m_Sema, 0, value, ITK_NULLPTR) != 0 )
    {
    itkExceptionMacro(<< "sema_init call failed");
    }
#elif defined  __APPLE__
  m_Sema  = sem_open(m_SemaphoreName.c_str(), O_CREAT, 0x0644, value);
  if ( m_Sema == (sem_t *)SEM_FAILED )
    {
    //  perror("FAILED WITH ERROR:" );
    itkExceptionMacro( << "sem_open call failed on " << m_SemaphoreName.c_str() );
    }
#else
  if ( sem_init(&m_Sema, 0, value) != 0 )
    {
    itkExceptionMacro(<< "sem_init call failed");
    }
#endif // if defined sun

#endif // ifdef ITK_USE_PTHREADS
#endif // ifndef ITK_USE_UNIX_IPC_SEMAPHORES

#ifdef ITK_USE_WIN32_THREADS
  m_Sema = CreateSemaphore(0, value, 0x7FFFFFFF, 0);
  if ( m_Sema == 0 )
    {
    itkExceptionMacro(<< "CreateSemaphore call failed");
    }
#endif
}

void Semaphore::Up()
{
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  Semaphore::UnixIpcSemaphoreUp(m_Sema);
#endif

#ifndef ITK_USE_UNIX_IPC_SEMAPHORES

#ifdef ITK_USE_PTHREADS
#ifdef sun
  if ( sema_post(&m_Sema) != 0 )
    {
    itkExceptionMacro(<< "sema_post call failed.");
    }
#else
#ifdef __APPLE__
  if ( sem_post(m_Sema) != 0 )
    {
    itkExceptionMacro(<< "sem_post call failed.");
    }
#else
  if ( sem_post(&m_Sema) != 0 )
    {
    itkExceptionMacro(<< "sem_post call failed.");
    }
#endif
#endif // ifdef sun
#endif

#endif  // ITK_USE_UNIX_SEMAPHORES

#ifdef ITK_USE_WIN32_THREADS
  if ( !ReleaseSemaphore ( (HANDLE)m_Sema, 1, 0 ) )
    {
    itkExceptionMacro(<< "Semaphore post call failed.");
    }
#endif
}

void Semaphore::Down()
{
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  Semaphore::UnixIpcSemaphoreDown (m_Sema);
#endif

#ifndef ITK_USE_UNIX_IPC_SEMAPHORES
#ifdef ITK_USE_PTHREADS
#ifdef sun
  if ( sema_wait(&m_Sema) != 0 )
    {
    itkExceptionMacro(<< "sema_wait call failed.");
    }
#else
#ifdef __APPLE__
  if ( sem_wait(m_Sema) != 0 )
    {
    itkExceptionMacro(<< "sem_wait call failed.");
    }
#else

  if ( sem_wait(&m_Sema) != 0 )
    {
    itkExceptionMacro(<< "sem_wait call failed.");
    }
#endif
#endif
#endif
#endif

#ifdef ITK_USE_WIN32_THREADS
  if ( WaitForSingleObject(m_Sema, INFINITE) == WAIT_FAILED )
    {
    itkExceptionMacro(<< "WaitForSingleObject call failed. ");
    }
#endif
}

Semaphore::~Semaphore()
{
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  if ( m_Sema != -1 )
    {
    this->Remove();
    }
#endif

#ifndef ITK_USE_UNIX_IPC_SEMAPHORES
#ifdef ITK_USE_PTHREADS
  if ( !m_PThreadsSemaphoreRemoved )
    {
    this->Remove();
    }
#endif
#endif

#ifdef ITK_USE_WIN32_THREADS
  if ( m_Sema != 0 )
    {
    this->Remove();
    }
#endif
}

void Semaphore::Remove()
{
#ifdef ITK_USE_UNIX_IPC_SEMAPHORES
  if ( m_Sema != -1 )
    {
    Semaphore::UnixIpcSemaphoreRemove(m_Sema);
    m_Sema = -1;
    }
#endif

#ifndef ITK_USE_UNIX_IPC_SEMAPHORES

#ifdef ITK_USE_PTHREADS
  m_PThreadsSemaphoreRemoved = true;
#ifdef sun
  if ( sema_destroy(&m_Sema) != 0 )
    {
    itkExceptionMacro(<< "sema_destroy call failed. ");
    }
#else

#ifndef __APPLE__
  if ( sem_destroy(&m_Sema) != 0 )
    {
    itkExceptionMacro(<< "sem_destroy call failed. ");
    }
#else //Still need to close semaphore and delete the file descriptor on MacOSX,
      // otherwise the shared memory space is eventually exhosted.
  //Eventually (i.e. after several days of ITK regresssion testing) the
  // semaphore creation process was failing with errno=ENOSPC
  //This implementation detail was taken from
  // http://developer.apple.com/macosx/multithreadedprogramming.html
  if ( sem_close(this->m_Sema) != 0 )
    {
    itkExceptionMacro(<< "sem_close call failed. ");
    }
  if ( sem_unlink( this->m_SemaphoreName.c_str() ) != 0 )
    {
    itkExceptionMacro(<< "sem_unlink call failed. ");
    }
#endif

#endif // sun
#endif // pthreads

#endif // semaphores
  //#endif

#ifdef ITK_USE_WIN32_THREADS
  if ( m_Sema != 0 )
    {
    CloseHandle(m_Sema);
    }
  m_Sema = 0;
#endif
}

#ifdef ITK_USE_UNIX_IPC_SEMAPHORES

// UnixIpcSemaphoreCreate: will return the semaphore id (system wide)
// of the semaphore number (key) you give.
// If no semaphore has been established for this number, one is created.
int Semaphore::UnixIpcSemaphoreCreate(int unix_semaphore_key)
{
  int         sid = -1;
  std::string s;

  if ( ( sid = semget( (key_t)unix_semaphore_key, 1, 0666 | IPC_CREAT ) ) == -1 )
    {
    s =  "Error#%i in function UnixIpcSemaphoreCreate. - ";
    switch ( errno )
      {
      case EEXIST:
        s += "Semaphore already exists. - ";
        break;
      case ENOSPC:
        s += "System imposed limit on the number of semaphores is exceeded - ";
        break;
      case EACCES:
        s += "Permission is denied - ";
        break;
      }
    itkExceptionMacro( << s.c_str() );
    }

  return ( sid );
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

  if ( semctl(sid, 0, IPC_RMID, 0) == -1 )
    {
    s =  "Error removing semaphore %i - ";
    switch ( errno )
      {
      case EINVAL:
        s += "Semaphore id#is not valid. - ";
        break;
      case EACCES:
        s += "Permission is denied - ";
        break;
      case EPERM:
        s += "Permission is denied - ";
        break;
      }
    itkExceptionMacro ( << s.c_str() );
    }
}

// UnixIpcSemaphoreCall: makes the system call semop for your given
// semaphore and operation.
void Semaphore::UnixIpcSemaphoreCall(int sid, int op)
{
  struct sembuf sb;
  std::string   s;

  sb.sem_num = 0;
  sb.sem_op = op;
  sb.sem_flg = 0;
  if ( semop(sid, &sb, 1) == -1 )
    {
    if ( op == -1 )
      {
      s =  "Error %i in UnixIpcSemaphoreDown call for semaphore %i - ";
      }
    else
      {
      s =  "Error %i in UnixIpcSemaphoreUp call for semaphore %i - ";
      }
    switch ( errno )
      {
      case EINVAL:
        s += "Semaphore id#is not valid. -";
        break;
      case EFBIG:
        s += "Invalid sem_num for semaphore - ";
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
} //end if namespace itk

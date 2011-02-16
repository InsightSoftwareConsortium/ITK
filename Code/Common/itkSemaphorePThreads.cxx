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

extern "C" {
#include <stdio.h>
#include <time.h>
#include <fcntl.h>
}

namespace itk
{
static int SemaphoreCount = 0;

static std::string GetUniqueName()
{
  char   s[255];
  time_t t = time(0);

  snprintf(s, 254, "MACSEM%d%d", static_cast< int >( t ), SemaphoreCount);
  SemaphoreCount++;
  return std::string(s);
}

Semaphore::Semaphore ()
{
  m_Sema = 0;
  m_PThreadsSemaphoreRemoved = false;
}

void Semaphore::Initialize(unsigned int value)
{
  std::string name = GetUniqueName();
  m_PThreadsSemaphoreRemoved = false;

  m_Sema  = sem_open(name.c_str(), O_CREAT, 0x0644, value);
  if ( m_Sema == (sem_t *)SEM_FAILED )
    {
    //  perror("FAILED WITH ERROR:" );
    itkExceptionMacro( << "sem_open call failed on " << name.c_str() );
    }

}

void Semaphore::Up()
{
  if ( sem_post(m_Sema) != 0 )
    {
    itkExceptionMacro(<< "sem_post call failed.");
    }
}

void Semaphore::Down()
{

  if ( sem_wait(m_Sema) != 0 )
    {
    itkExceptionMacro(<< "sem_wait call failed.");
    }
}

Semaphore::~Semaphore()
{
  if ( !m_PThreadsSemaphoreRemoved )
    {
    this->Remove();
    }
}

void Semaphore::Remove()
{
  m_PThreadsSemaphoreRemoved = true;
  if ( sem_destroy(m_Sema) != 0 )
    {
    itkExceptionMacro(<< "sem_destroy call failed. ");
    }
}

} //end if namespace itk
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

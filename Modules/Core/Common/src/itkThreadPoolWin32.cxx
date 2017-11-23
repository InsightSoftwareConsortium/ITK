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
#include "itkThreadPool.h"
#include <utility>
#include <process.h>

namespace itk
{
ThreadIdType
ThreadPool
::GetGlobalDefaultNumberOfThreadsByPlatform()
{
  SYSTEM_INFO sysInfo;

  GetSystemInfo(&sysInfo);
  ThreadIdType num = sysInfo.dwNumberOfProcessors;
  return num;
}

//Returns the last Win32 error, in string format. Returns an empty string if there is no error.
std::string GetLastErrorAsString()
{
  //Get the error message, if any.
  DWORD errorMessageID = ::GetLastError();
  if (errorMessageID == 0)
    {
    return std::string(); //No error message has been recorded
    }

  LPSTR messageBuffer = ITK_NULLPTR;
  size_t size = FormatMessageA(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
      NULL, errorMessageID, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR)&messageBuffer, 0, NULL);

  std::string message(messageBuffer, size);

  //Free the buffer.
  LocalFree(messageBuffer);

  return message;
}

void
ThreadPool
::PlatformCreate(Semaphore &semaphore)
{
  semaphore = CreateSemaphore(ITK_NULLPTR, 0, 0x7ffffffel, ITK_NULLPTR);
  if (semaphore == ITK_NULLPTR)
    {
    itkGenericExceptionMacro(<< "CreateSemaphore error. " << GetLastErrorAsString());
    }
}

void
ThreadPool
::PlatformWait(Semaphore &semaphore)
{
  DWORD dwWaitResult = WaitForSingleObject(semaphore, INFINITE);
  if (dwWaitResult != WAIT_OBJECT_0)
    {
    itkGenericExceptionMacro(<< "SemaphoreWait error. " << GetLastErrorAsString());
    }
}

void
ThreadPool
::PlatformSignal(Semaphore &semaphore)
{
  if (!ReleaseSemaphore(semaphore, 1, ITK_NULLPTR))
    {
    itkGenericExceptionMacro(<< "SignalSemaphore error. " << GetLastErrorAsString());
    }
}

void
ThreadPool
::PlatformDelete(Semaphore &semaphore)
{
  if (!CloseHandle(semaphore))
    {
    itkGenericExceptionMacro(<< "DeleteSemaphore error. " << GetLastErrorAsString());
    }
}

bool
ThreadPool
::PlatformClose(ThreadProcessIdType &threadId)
{
  return CloseHandle(threadId);
}

void
ThreadPool
::AddThread()
{
  ThreadProcessIdType threadHandle = reinterpret_cast<ThreadProcessIdType>(_beginthreadex(
    ITK_NULLPTR,
    0,
    ThreadPool::ThreadExecute,
    ITK_NULLPTR,
    0,
    ITK_NULLPTR));

  if (threadHandle == ITK_NULLPTR)
    {
    itkDebugMacro(<< "ERROR adding thread to thread pool");
    itkExceptionMacro(<< "Cannot create thread. " << GetLastErrorAsString());
    }
  m_Threads.push_back(threadHandle);
}

}

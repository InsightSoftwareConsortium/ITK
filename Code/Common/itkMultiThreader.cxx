/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiThreader.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkMultiThreader.h"
#include "itkObjectFactory.h"
#ifndef _WIN32
#include <unistd.h>
#endif
#ifdef _WIN32
#pragma warning ( disable : 4786 )
#include "itkWindows.h"
#include <process.h>
#endif

#ifdef ITK_USE_SPROC
#include <sys/prctl.h>
#include <wait.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/resource.h>
#endif

namespace itk {
extern "C"
{
  typedef void*(*c_void_cast)(void*);
}
  
// Initialize static member that controls global maximum number of threads : 0 => Not initialized.
int MultiThreader::m_GlobalMaximumNumberOfThreads = 0;
int MultiThreader::m_GlobalDefaultNumberOfThreads = 0;

void MultiThreader::SetGlobalMaximumNumberOfThreads(int val)
{
  if (val == m_GlobalMaximumNumberOfThreads)
    {
    return;
    }
  m_GlobalMaximumNumberOfThreads = val;
}
  
int MultiThreader::GetGlobalMaximumNumberOfThreads()
{
  return m_GlobalMaximumNumberOfThreads;
}

void MultiThreader::SetGlobalDefaultNumberOfThreads(int val)
{
  if (val == m_GlobalDefaultNumberOfThreads)
    {
    return;
    }
  m_GlobalDefaultNumberOfThreads = val;
}

int MultiThreader::GetGlobalDefaultNumberOfThreads()
{
  if (m_GlobalDefaultNumberOfThreads == 0)
    {
    int num;
#ifdef ITK_USE_SPROC
    // Default the number of threads to be the number of available
    // processors if we are using sproc()
    num = prctl( PR_MAXPPROCS );
#endif
    
#ifdef ITK_USE_PTHREADS
    // Default the number of threads to be the number of available
    // processors if we are using pthreads()
#ifdef _SC_NPROCESSORS_ONLN
    num = sysconf( _SC_NPROCESSORS_ONLN );
#elif defined(_SC_NPROC_ONLN)
    num = sysconf( _SC_NPROC_ONLN );
#else
    num = 1;
#endif
#if defined(__SVR4) && defined(sun) && defined(PTHREAD_MUTEX_NORMAL)
    pthread_setconcurrency(num);
#endif
#endif
    
#if defined(_WIN32)
    {
  SYSTEM_INFO sysInfo;
  GetSystemInfo(&sysInfo);
  num = sysInfo.dwNumberOfProcessors;
    }
#endif
    
#ifndef ITK_USE_WIN32_THREADS
#ifndef ITK_USE_SPROC
#ifndef ITK_USE_PTHREADS
    // If we are not multithreading, the number of threads should
    // always be 1
    num = 1;
#endif  
#endif  
#endif
    
    // Limit the number of threads
    if (num > ITK_MAX_THREADS)
      {
      num = ITK_MAX_THREADS;
      }
    m_GlobalDefaultNumberOfThreads = num;
    }
  
  return m_GlobalDefaultNumberOfThreads;
}

// Constructor. Default all the methods to NULL. Since the
// ThreadInfoArray is static, the ThreadIDs can be initialized here
// and will not change.
#ifdef ITK_USE_SPROC
bool      MultiThreader::m_Initialized = false;
usptr_t * MultiThreader::m_ThreadArena= 0;
int       MultiThreader::m_DevzeroFd  = -1;

void MultiThreader::Initialize()
{
  usconfig(CONF_ARENATYPE, US_SHAREDONLY);
  //  usconfig(CONF_INITSIZE, 256*1024*1024); // not to be used if we are auto growing the arena
  usconfig(CONF_INITUSERS, (unsigned int) ITK_MAX_THREADS);
  
  usconfig(CONF_AUTOGROW, 1);
  usconfig(CONF_AUTORESV, 1);
  
  MultiThreader::m_ThreadArena = usinit("/dev/zero");
  
  int code= usadd (MultiThreader::m_ThreadArena);
  
  MultiThreader::m_Initialized = true;
  
  MultiThreader::m_DevzeroFd= open("/dev/zero", O_RDWR);
}
#endif

MultiThreader::MultiThreader()
{
  for (int i = 0; i < ITK_MAX_THREADS; i++)
    {
    m_ThreadInfoArray[i].ThreadID           = i;
    m_ThreadInfoArray[i].ActiveFlag         = 0;
    m_ThreadInfoArray[i].ActiveFlagLock     = 0;
    
    m_MultipleMethod[i]                     = 0;
    m_MultipleData[i]                       = 0;
    
    m_SpawnedThreadActiveFlag[i]            = 0;
    m_SpawnedThreadActiveFlagLock[i]        = 0;
    m_SpawnedThreadInfoArray[i].ThreadID    = i;
    }
  
  m_SingleMethod = 0;
  m_SingleData = 0;
  m_NumberOfThreads = this->GetGlobalDefaultNumberOfThreads();
}

MultiThreader::~MultiThreader()
{
#ifdef ITK_USE_SPROC
  if (MultiThreader::m_ThreadArena != 0)
    {
    // close the file
    close (MultiThreader::m_DevzeroFd);
    // unmap the arena
    usdetach (MultiThreader::m_ThreadArena);
    
    MultiThreader::m_ThreadArena= 0;
    MultiThreader::m_Initialized= false;
    MultiThreader::m_DevzeroFd= -1;
    }
#endif
}

// Set the user defined method that will be run on NumberOfThreads threads
// when SingleMethodExecute is called.
void MultiThreader::SetSingleMethod( ThreadFunctionType f, void *data )
{
  m_SingleMethod = f;
  m_SingleData   = data;
}

// Set one of the user defined methods that will be run on NumberOfThreads
// threads when MultipleMethodExecute is called. This method should be
// called with index = 0, 1, ..,  NumberOfThreads-1 to set up all the
// required user defined methods
void MultiThreader::SetMultipleMethod( int index, ThreadFunctionType f, void *data )
{ 
  // You can only set the method for 0 through NumberOfThreads-1
  if ( index >= m_NumberOfThreads )
    {
    itkExceptionMacro( << "Can't set method " << index << " with a thread count of " << m_NumberOfThreads );
    }
  else
    {
    m_MultipleMethod[index] = f;
    m_MultipleData[index]   = data;
    }
}

// Execute the method set as the SingleMethod on NumberOfThreads threads.
void MultiThreader::SingleMethodExecute()
{
  int thread_loop;
  ThreadProcessIDType process_id[ITK_MAX_THREADS];
  
  if ( !m_SingleMethod)
    {
    itkExceptionMacro( << "No single method set!" );
    return;
    }
  
  // obey the global maximum number of threads limit
  if (m_GlobalMaximumNumberOfThreads &&
      m_NumberOfThreads > m_GlobalMaximumNumberOfThreads)
    {
    m_NumberOfThreads = m_GlobalMaximumNumberOfThreads;
    }
  
  // sproc needs special initialization logic prior to spawning
  // threads
#ifdef ITK_USE_SPROC
# define STACK_SIZE 8*1024*1024
  // set up the arena by the parent process ONLY if going to use more
  // than 1 threads
  if (!MultiThreader::m_Initialized && m_NumberOfThreads > 1)
    {
    MultiThreader::Initialize();
    }
  
  struct rlimit64 rlpOld;
  int code= getrlimit64 (RLIMIT_STACK, &rlpOld);
  if (code != 0) itkExceptionMacro("getrlimit failed in Multithreader");
  
  struct rlimit64 rlpNew;
  rlpNew.rlim_cur= STACK_SIZE;
  rlpNew.rlim_max= rlpOld.rlim_max;
  code= setrlimit64 (RLIMIT_STACK, &rlpNew);
  if (code != 0) itkExceptionMacro("setrlimit failed in Multithreader");
#endif


  // Spawn a set of threads through the SingleMethodProxy. Exceptions
  // thrown from a thread will be caught by the SingleMethodProxy. A
  // naive mechanism is in place for determining whether a thread
  // threw an exception.
  //
  // Thanks to Hannu Helminen for suggestions on how to catch
  // exceptions thrown by threads.
  bool exceptionOccurred = false;
  try
    {
    for (thread_loop = 1; thread_loop < m_NumberOfThreads; thread_loop++ )
      {
      m_ThreadInfoArray[thread_loop].UserData    = m_SingleData;
      m_ThreadInfoArray[thread_loop].NumberOfThreads = m_NumberOfThreads;
      m_ThreadInfoArray[thread_loop].ThreadFunction = m_SingleMethod;

      process_id[thread_loop]
        = this->DispatchSingleMethodThread(&m_ThreadInfoArray[thread_loop]);
      }
    }
  catch (...)
    {
    // If creation of any thread failed, we must make sure that all
    // threads are correctly cleaned
    exceptionOccurred = true;
    }
  
  // Now, the parent thread calls this->SingleMethod() itself
  //
  //
  try
    {
    m_ThreadInfoArray[0].UserData = m_SingleData;
    m_ThreadInfoArray[0].NumberOfThreads = m_NumberOfThreads;
    m_SingleMethod((void *)(&m_ThreadInfoArray[0]));
    }
  catch ( ProcessAborted &excp )
    {
    // Need cleanup and rethrow ProcessAborted
    // close down other threads
    for (thread_loop = 1; thread_loop < m_NumberOfThreads; thread_loop++ )
      {
      try
        {
        this->WaitForSingleMethodThread(process_id[thread_loop]);
        }
      catch (...)
        {
        }
      }
    
#ifdef ITK_USE_SPROC
    // reset the resource limits if using sproc
    code= setrlimit64 (RLIMIT_STACK, &rlpOld);
#endif

    // rethrow
    throw excp;
    }
  catch (...)
    {
    // if this method fails, we must make sure all threads are
    // correctly cleaned
    exceptionOccurred = true;
    }
    
  // The parent thread has finished this->SingleMethod() - so now it
  // waits for each of the other processes to exit
  for (thread_loop = 1; thread_loop < m_NumberOfThreads; thread_loop++ )
    {
    try
      {
      this->WaitForSingleMethodThread(process_id[thread_loop]);
      if (m_ThreadInfoArray[thread_loop].ThreadExitCode
          != ThreadInfoStruct::SUCCESS)
        {
        exceptionOccurred = true;
        }
      }
    catch (...)
      {
      exceptionOccurred = true; 
      }
    }

#ifdef ITK_USE_SPROC
  // reset the resource limits if using sproc
  code= setrlimit64 (RLIMIT_STACK, &rlpOld);
  if (code != 0)
    {
    itkExceptionMacro("setrlimit failed in Multithreader");
    }
#endif
  
  if (exceptionOccurred)
    {
    itkExceptionMacro("Exception occurred during SingleMethodExecute");
    }
}

void MultiThreader::MultipleMethodExecute()
{
  int                thread_loop;
  
#ifdef ITK_USE_WIN32_THREADS
  DWORD              threadId;
  HANDLE             process_id[ITK_MAX_THREADS];
#endif
  
#ifdef ITK_USE_SPROC
  siginfo_t          info_ptr;
  int                process_id[ITK_MAX_THREADS];
#endif
  
#ifdef ITK_USE_PTHREADS
  pthread_t          process_id[ITK_MAX_THREADS];
#endif
  
  // obey the global maximum number of threads limit
  if (m_GlobalMaximumNumberOfThreads &&
      m_NumberOfThreads > m_GlobalMaximumNumberOfThreads)
    {
    m_NumberOfThreads = m_GlobalMaximumNumberOfThreads;
    }
  
  for ( thread_loop = 0; thread_loop < m_NumberOfThreads; thread_loop++ )
    {
    if ( m_MultipleMethod[thread_loop] == (ThreadFunctionType)0)
      {
    itkExceptionMacro( << "No multiple method set for: " << thread_loop );
    return;
      }
    }
  
  // We are using sproc (on SGIs), pthreads(on Suns), _beginthreadex
  // on a PC or a single thread (the default)  
  
#ifdef ITK_USE_WIN32_THREADS
  // Using _beginthreadex on a PC
  //
  // We want to use _beginthreadex to start m_NumberOfThreads - 1 
  // additional threads which will be used to call the NumberOfThreads-1
  // methods defined in this->MultipleMethods[](). The parent thread
  // will call m_MultipleMethods[NumberOfThreads-1]().  When it is done,
  // it will wait for all the children to finish. 
  //
  // First, start up the m_NumberOfThreads-1 processes.  Keep track
  // of their process ids for use later in the waitid call
  for ( thread_loop = 1; thread_loop < m_NumberOfThreads; thread_loop++ )
    {
    m_ThreadInfoArray[thread_loop].UserData = 
      m_MultipleData[thread_loop];
    m_ThreadInfoArray[thread_loop].NumberOfThreads = m_NumberOfThreads;
    
    process_id[thread_loop] = (void *)
      _beginthreadex(0, 0,
                     (unsigned int (__stdcall *)(void *))m_MultipleMethod[thread_loop],
                     ((void *)(&m_ThreadInfoArray[thread_loop])), 0,
                     (unsigned int *)&threadId);
    
    if (process_id == 0)
      {
    itkExceptionMacro("Error in thread creation !!!");
      } 
    }
  
  // Now, the parent thread calls the last method itself
  m_ThreadInfoArray[0].UserData = m_MultipleData[0];
  m_ThreadInfoArray[0].NumberOfThreads = m_NumberOfThreads;
  (m_MultipleMethod[0])((void *)(&m_ThreadInfoArray[0]) );
  
  // The parent thread has finished its method - so now it
  // waits for each of the other processes (created with sproc) to
  // exit
  for ( thread_loop = 1; thread_loop < m_NumberOfThreads; thread_loop++ )
    {
    WaitForSingleObject(process_id[thread_loop], INFINITE);
    }
  
  // close the threads
  for ( thread_loop = 1; thread_loop < m_NumberOfThreads; thread_loop++ )
    {
    CloseHandle(process_id[thread_loop]);
    }
#endif
  
#ifdef ITK_USE_SPROC
  // Using sproc() on an SGI
  //
  // We want to use sproc to start m_NumberOfThreads - 1 additional
  // threads which will be used to call the NumberOfThreads-1 methods
  // defined in m_MultipleMethods[](). The parent thread
  // will call m_MultipleMethods[NumberOfThreads-1]().  When it is done,
  // it will wait for all the children to finish. 
  //
  // First, start up the m_NumberOfThreads-1 processes.  Keep track
  // of their process ids for use later in the waitid call
  
  if (!MultiThreader::m_Initialized && m_NumberOfThreads > 1)
    {
    MultiThreader::Initialize();
    }
  
  struct rlimit64 rlpOld;
  int code= getrlimit64 (RLIMIT_STACK, &rlpOld);
  if (code != 0) itkExceptionMacro("getrlimit failed in Multithreader");
  
# define STACK_SIZE 8*1024*1024
  struct rlimit64 rlpNew;
  rlpNew.rlim_cur= STACK_SIZE;
  rlpNew.rlim_max= rlpOld.rlim_max;
  code= setrlimit64 (RLIMIT_STACK, &rlpNew);
  if (code != 0) itkExceptionMacro("setrlimit failed in Multithreader");
  
  for ( thread_loop = 1; thread_loop < m_NumberOfThreads; thread_loop++ )
    {
      m_ThreadInfoArray[thread_loop].UserData = m_MultipleData[thread_loop];
      m_ThreadInfoArray[thread_loop].NumberOfThreads = m_NumberOfThreads;
      process_id[thread_loop] = sproc( m_MultipleMethod[thread_loop], PR_SALL, 
               ( (void *)(&m_ThreadInfoArray[thread_loop]) ) );
    }
  
  // Now, the parent thread calls the last method itself
  m_ThreadInfoArray[0].UserData = m_MultipleData[0];
  m_ThreadInfoArray[0].NumberOfThreads = m_NumberOfThreads;
  (m_MultipleMethod[0])((void *)(&m_ThreadInfoArray[0]) );
  
  // The parent thread has finished its method - so now it
  // waits for each of the other processes (created with sproc) to
  // exit
  for ( thread_loop = 1; thread_loop < m_NumberOfThreads; thread_loop++ )
    {
      waitid( P_PID, (id_t) process_id[thread_loop], &info_ptr, WEXITED );
    }
  
  code= setrlimit64 (RLIMIT_STACK, &rlpOld);
  if (code != 0)
    {
    itkExceptionMacro("setrlimit failed in Multithreader");
    }
#endif
  
#ifdef ITK_USE_PTHREADS
  // Using POSIX threads
  //
  // We want to use pthread_create to start m_NumberOfThreads - 1 
  // additional
  // threads which will be used to call the NumberOfThreads-1 methods
  // defined in m_MultipleMethods[](). The parent thread
  // will call m_MultipleMethods[NumberOfThreads-1]().  When it is done,
  // it will wait for all the children to finish. 
  //
  // First, start up the m_NumberOfThreads-1 processes.  Keep track
  // of their process ids for use later in the pthread_join call
  
  pthread_attr_t attr;
  
#ifdef ITK_HP_PTHREADS
  pthread_attr_create( &attr );
#else  
  pthread_attr_init(&attr);
#ifndef __CYGWIN__
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_PROCESS);
#endif
#endif
  
  for ( thread_loop = 1; thread_loop < m_NumberOfThreads; thread_loop++ )
    {
    m_ThreadInfoArray[thread_loop].UserData = 
      m_MultipleData[thread_loop];
    m_ThreadInfoArray[thread_loop].NumberOfThreads = m_NumberOfThreads;
#ifdef ITK_HP_PTHREADS
    pthread_create( &(process_id[thread_loop]),
                    attr, m_MultipleMethod[thread_loop],  
                    ( (void *)(&m_ThreadInfoArray[thread_loop]) ) );
#else
    pthread_create( &(process_id[thread_loop]),
                    &attr, reinterpret_cast<c_void_cast>(m_MultipleMethod[thread_loop]),  
                    ( (void *)(&m_ThreadInfoArray[thread_loop]) ) );
#endif
    }
  
  // Now, the parent thread calls the last method itself
  m_ThreadInfoArray[0].UserData = m_MultipleData[0];
  m_ThreadInfoArray[0].NumberOfThreads = m_NumberOfThreads;
  (m_MultipleMethod[0])((void *)(&m_ThreadInfoArray[0]) );
  
  // The parent thread has finished its method - so now it
  // waits for each of the other processes to exit
  for ( thread_loop = 1; thread_loop < m_NumberOfThreads; thread_loop++ )
    {
    pthread_join( process_id[thread_loop], 0 );
    }
#endif
  
#ifndef ITK_USE_WIN32_THREADS
#ifndef ITK_USE_SPROC
#ifndef ITK_USE_PTHREADS
  // There is no multi threading, so there is only one thread.
  m_ThreadInfoArray[0].UserData    = m_MultipleData[0];
  m_ThreadInfoArray[0].NumberOfThreads = m_NumberOfThreads;
  (m_MultipleMethod[0])( (void *)(&m_ThreadInfoArray[0]) );
#endif
#endif
#endif
}

int MultiThreader::SpawnThread( ThreadFunctionType f, void *UserData )
{
  int id;
  
#ifdef ITK_USE_WIN32_THREADS
  DWORD              threadId;
#endif
  
  id = 0;
  
  while ( id < ITK_MAX_THREADS )
    {
    if ( ! m_SpawnedThreadActiveFlagLock[id]  )
      {
    m_SpawnedThreadActiveFlagLock[id] = MutexLock::New();
      }
    m_SpawnedThreadActiveFlagLock[id]->Lock();
    if (m_SpawnedThreadActiveFlag[id] == 0)
      {
    // We've got a useable thread id, so grab it
    m_SpawnedThreadActiveFlag[id] = 1;
    m_SpawnedThreadActiveFlagLock[id]->Unlock();
    break;
      }
    m_SpawnedThreadActiveFlagLock[id]->Unlock();
    
    id++;
    }
  
  if ( id >= ITK_MAX_THREADS )
    {
    itkExceptionMacro( << "You have too many active threads!" );
    return -1;
    }
  
  m_SpawnedThreadInfoArray[id].UserData        = UserData;
  m_SpawnedThreadInfoArray[id].NumberOfThreads = 1;
  m_SpawnedThreadInfoArray[id].ActiveFlag = &m_SpawnedThreadActiveFlag[id];
  m_SpawnedThreadInfoArray[id].ActiveFlagLock = m_SpawnedThreadActiveFlagLock[id];
  
  // We are using sproc (on SGIs), pthreads(on Suns or HPs), 
  // _beginthreadex (on win32), or generating an error  
  
#ifdef ITK_USE_WIN32_THREADS
  // Using _beginthreadex on a PC
  //
  m_SpawnedThreadProcessID[id] = (void *)
    _beginthreadex(0, 0, (unsigned int (__stdcall *)(void *))f, 
                   ((void *)(&m_SpawnedThreadInfoArray[id])), 0,
                   (unsigned int *)&threadId);
  if (m_SpawnedThreadProcessID[id] == 0)
    {
    itkExceptionMacro("Error in thread creation !!!");
    } 
#endif
  
#ifdef ITK_USE_SPROC
  // Using sproc() on an SGI
  //
  m_SpawnedThreadProcessID[id] = sproc( f, PR_SADDR, ( (void *)(&m_SpawnedThreadInfoArray[id]) ) );
  
#endif
  
#ifdef ITK_USE_PTHREADS
  // Using POSIX threads
  //
  pthread_attr_t attr;
  
#ifdef ITK_HP_PTHREADS
  pthread_attr_create( &attr );
#else  
  pthread_attr_init(&attr);
#ifndef __CYGWIN__
  pthread_attr_setscope(&attr, PTHREAD_SCOPE_PROCESS);
#endif
#endif
  
#ifdef ITK_HP_PTHREADS
  pthread_create( &(m_SpawnedThreadProcessID[id]),
                  attr, reinterpret_cast<c_void_cast>(f),  
                  ( (void *)(&m_SpawnedThreadInfoArray[id]) ) );
#else
  pthread_create( &(m_SpawnedThreadProcessID[id]),
                  &attr, reinterpret_cast<c_void_cast>(f),  
                  ( (void *)(&m_SpawnedThreadInfoArray[id]) ) );
#endif
  
#endif
  
#ifndef ITK_USE_WIN32_THREADS
#ifndef ITK_USE_SPROC
#ifndef ITK_USE_PTHREADS
  // There is no multi threading, so there is only one thread.
  // This won't work - so give an error message.
  itkExceptionMacro( << "Cannot spawn thread in a single threaded environment!" );
  m_SpawnedThreadActiveFlagLock[id] = 0;
  id = -1;
#endif
#endif
#endif
  
  return id;
}

void MultiThreader::TerminateThread( int ThreadID )
{
  if ( !m_SpawnedThreadActiveFlag[ThreadID] )
    {
    return;
    }
  
  m_SpawnedThreadActiveFlagLock[ThreadID]->Lock();
  m_SpawnedThreadActiveFlag[ThreadID] = 0;
  m_SpawnedThreadActiveFlagLock[ThreadID]->Unlock();
  
#ifdef ITK_USE_WIN32_THREADS
  WaitForSingleObject(m_SpawnedThreadProcessID[ThreadID], INFINITE);
  CloseHandle(m_SpawnedThreadProcessID[ThreadID]);
#endif
  
#ifdef ITK_USE_SPROC
  siginfo_t info_ptr;
  
  waitid( P_PID, (id_t) m_SpawnedThreadProcessID[ThreadID], 
          &info_ptr, WEXITED );
#endif
  
#ifdef ITK_USE_PTHREADS
  pthread_join( m_SpawnedThreadProcessID[ThreadID], 0 );
#endif
  
#ifndef ITK_USE_WIN32_THREADS
#ifndef ITK_USE_SPROC
#ifndef ITK_USE_PTHREADS
  // There is no multi threading, so there is only one thread.
  // This won't work - so give an error message.
  itkExceptionMacro(<< "Cannot terminate thread in single threaded environment!");
#endif
#endif
#endif
  
  m_SpawnedThreadActiveFlagLock[ThreadID] = 0;
  m_SpawnedThreadActiveFlagLock[ThreadID] = 0;
}

// Print method for the multithreader
void MultiThreader::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent); 
  
  os << indent << "Thread Count: " << m_NumberOfThreads << "\n";
  os << indent << "Global Maximum Number Of Threads: " << 
    m_GlobalMaximumNumberOfThreads << std::endl;
}

ITK_THREAD_RETURN_TYPE
MultiThreader
::SingleMethodProxy( void *arg )
{
  // grab the ThreadInfoStruct originally prescribed
  MultiThreader::ThreadInfoStruct
    *threadInfoStruct
    = reinterpret_cast<MultiThreader::ThreadInfoStruct *>(arg);

  // execute the user specified threader callback, catching any exceptions
  try
    {
    (*threadInfoStruct->ThreadFunction)(threadInfoStruct);
    threadInfoStruct->ThreadExitCode
      = MultiThreader::ThreadInfoStruct::SUCCESS;
    }
  catch (ProcessAborted &)
    {
    threadInfoStruct->ThreadExitCode
      = MultiThreader::ThreadInfoStruct::ITK_PROCESS_ABORTED_EXCEPTION;
    }
  catch (ExceptionObject &)
    {
    threadInfoStruct->ThreadExitCode
      = MultiThreader::ThreadInfoStruct::ITK_EXCEPTION;
    }
  catch (std::exception &)
    {
    threadInfoStruct->ThreadExitCode
      = MultiThreader::ThreadInfoStruct::STD_EXCEPTION;
    }
  catch (...)
    {
    threadInfoStruct->ThreadExitCode
      = MultiThreader::ThreadInfoStruct::UNKNOWN;
    }

  return ITK_THREAD_RETURN_VALUE;
}

void
MultiThreader
::WaitForSingleMethodThread(ThreadProcessIDType threadHandle)
{
#ifdef ITK_USE_WIN32_THREADS
  // Using _beginthreadex on a PC
  WaitForSingleObject(threadHandle, INFINITE);
  CloseHandle(threadHandle);
#endif

#ifdef ITK_USE_SPROC
  // Using sproc() on an SGI
  siginfo_t          info_ptr;
  waitid( P_PID, (id_t) threadHandle, &info_ptr, WEXITED );
#endif

#ifdef ITK_USE_PTHREADS
  // Using POSIX threads
  if ( pthread_join( threadHandle, 0 ) )
    {
    itkExceptionMacro(<< "Unable to join thread.");
    }
#endif      


#ifndef ITK_USE_WIN32_THREADS
#ifndef ITK_USE_SPROC
#ifndef ITK_USE_PTHREADS
    // No threading library specified.  Do nothing.  No joining or waiting
    // necessary.
#endif
#endif
#endif
    
}



ThreadProcessIDType
MultiThreader
::DispatchSingleMethodThread(MultiThreader::ThreadInfoStruct *threadInfo)
{
#ifdef ITK_USE_WIN32_THREADS
  // Using _beginthreadex on a PC
  DWORD threadId;
  HANDLE threadHandle =  (HANDLE)_beginthreadex(0, 0,
               (unsigned int (__stdcall *)(void *))this->SingleMethodProxy, 
               ((void *)threadInfo), 0, (unsigned int *)&threadId);
  if (threadHandle == NULL)
    {
    itkExceptionMacro("Error in thread creation !!!");
    } 
  return threadHandle;
#endif

  
#ifdef ITK_USE_SPROC
  // Using sproc() on an SGI
    pid_t threadHandle = 
      sproc( this->SingleMethodProxy, PR_SALL, 
       reinterpret_cast<void *>(threadInfo));
    if ( threadHandle == -1)
      {
      itkExceptionMacro("sproc call failed. Code: " << errno << std::endl);
      }
    return threadHandle;
#endif


    
#ifdef ITK_USE_PTHREADS
  // Using POSIX threads
    pthread_attr_t attr;
    pthread_t threadHandle;

#ifdef ITK_HP_PTHREADS
    pthread_attr_create( &attr );
#else  
    pthread_attr_init(&attr);
#if !defined(__CYGWIN__)
    pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);
#endif
#endif

#ifdef ITK_HP_PTHREADS
    pthread_create( &threadHandle,
        attr, reinterpret_cast<c_void_cast>(this->SingleMethodProxy),  
        reinterpret_cast<void *>(threadInfo));
#else
    int                threadError;
    threadError =
      pthread_create( &threadHandle, &attr, reinterpret_cast<c_void_cast>(this->SingleMethodProxy),  
          reinterpret_cast<void *>(threadInfo));
    if (threadError != 0)
      {
      itkExceptionMacro(<< "Unable to create a thread.  pthread_create() returned "
                    << threadError);
      }
#endif
    return threadHandle;
#endif      


#ifndef ITK_USE_WIN32_THREADS
#ifndef ITK_USE_SPROC
#ifndef ITK_USE_PTHREADS
    // No threading library specified.  Do nothing.  The computation
    // will be run by the main execution thread.
#endif
#endif
#endif
    
}


} // end namespace itk

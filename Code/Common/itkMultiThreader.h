/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiThreader.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMultiThreader_h
#define __itkMultiThreader_h

#include "itkObject.h"
#include "itkMutexLock.h"

#ifdef ITK_USE_SPROC
#include <sys/types.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/prctl.h>
#include <wait.h>
#include <errno.h>
#include <ulocks.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/signal.h>
#include <sys/sysmp.h>
#include <sys/errno.h>
#include <sys/syssgi.h>

extern "C" {
#include <sys/pmo.h>
#include <fetchop.h>
}
#endif

#ifdef ITK_USE_PTHREADS
#include <pthread.h>
#endif

namespace itk
{
/** \class MultiThreader
 * \brief A class for performing multithreaded execution
 *
 * Multithreader is a class that provides support for multithreaded
 * execution using sproc() on an SGI, or pthread_create on any platform
 * supporting POSIX threads.  This class can be used to execute a single
 * method on multiple threads, or to specify a method per thread.
 *
 * \ingroup OSSystemObejcts
 *
 * \par Note
 * If ITK_USE_SPROC is defined, then sproc() will be used to create
 * multiple threads on an SGI. If ITK_USE_PTHREADS is defined, then
 * pthread_create() will be used to create multiple threads (on
 * a sun, for example). */

// The maximum number of threads allowed
#ifdef ITK_USE_SPROC
#define ITK_MAX_THREADS              128
#endif

#ifdef ITK_USE_PTHREADS
#define ITK_MAX_THREADS              128
#endif

#ifdef ITK_USE_WIN32_THREADS
#define ITK_MAX_THREADS              128
#endif

// cygwin threads are unreliable
#ifdef __CYGWIN__
#undef ITK_MAX_THREADS
#define ITK_MAX_THREADS 1
#endif

#ifndef ITK_MAX_THREADS
#define ITK_MAX_THREADS 1
#endif

/** \par Note
 * If ITK_USE_PTHREADS is defined, then the multithreaded
 * function is of type void *, and returns NULL
 * Otherwise the type is void which is correct for WIN32
 * and SPROC. */
#ifdef ITK_USE_SPROC
typedef int ThreadProcessIDType;
#endif

#ifdef ITK_USE_PTHREADS
typedef void *(*ThreadFunctionType)(void *);
typedef pthread_t ThreadProcessIDType;
#define ITK_THREAD_RETURN_VALUE  NULL
#define ITK_THREAD_RETURN_TYPE   void *
#endif

#ifdef ITK_USE_WIN32_THREADS
typedef LPTHREAD_START_ROUTINE ThreadFunctionType;
typedef HANDLE ThreadProcessIDType;
#define ITK_THREAD_RETURN_VALUE 0
#define ITK_THREAD_RETURN_TYPE DWORD __stdcall
#endif

#ifndef ITK_THREAD_RETURN_VALUE
typedef void (*ThreadFunctionType)(void *);
typedef int ThreadProcessIDType;
#define ITK_THREAD_RETURN_VALUE
#define ITK_THREAD_RETURN_TYPE void
#endif
  
class ITKCommon_EXPORT MultiThreader : public Object 
{
public:
  /** Standard class typedefs. */
  typedef MultiThreader         Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiThreader, Object);

  /** Get/Set the number of threads to create. It will be clamped to the range
   * 1 - ITK_MAX_THREADS, so the caller of this method should check that the
   * requested number of threads was accepted. */
  itkSetClampMacro( NumberOfThreads, int, 1, ITK_MAX_THREADS );
  itkGetMacro( NumberOfThreads, int );
    
  /** Set/Get the maximum number of threads to use when multithreading.
   * This limits and overrides any other settings for multithreading.
   * A value of zero indicates no limit. */
  static void SetGlobalMaximumNumberOfThreads(int val);
  static int  GetGlobalMaximumNumberOfThreads();
  
  /** Set/Get the value which is used to initialize the NumberOfThreads
   * in the constructor.  Initially this default is set to the number of 
   * processors or 8 (which ever is less). */
  static void SetGlobalDefaultNumberOfThreads(int val);
  static int  GetGlobalDefaultNumberOfThreads();
  
  /** Execute the SingleMethod (as define by SetSingleMethod) using
   * m_NumberOfThreads threads. */
  void SingleMethodExecute();

  /** Execute the MultipleMethods (as define by calling SetMultipleMethod
   * for each of the required m_NumberOfThreads methods) using
   * m_NumberOfThreads threads. */
  void MultipleMethodExecute();
  
  /** Set the SingleMethod to f() and the UserData field of the
   * ThreadInfoStruct that is passed to it will be data.
   * This method (and all the methods passed to SetMultipleMethod)
   * must be of type itkThreadFunctionType and must take a single argument of
   * type void *. */
  void SetSingleMethod(ThreadFunctionType, void *data );
  
  /** Set the MultipleMethod at the given index to f() and the UserData 
   * field of the ThreadInfoStruct that is passed to it will be data. */
  void SetMultipleMethod( int index, ThreadFunctionType, void *data ); 

  /** Create a new thread for the given function. Return a thread id
   * which is a number between 0 and ITK_MAX_THREADS - 1. This id should
   * be used to kill the thread at a later time. */
  int SpawnThread( ThreadFunctionType, void *data );

  /** Terminate the thread that was created with a SpawnThreadExecute() */
  void TerminateThread( int thread_id );
  
#ifdef ITK_USE_SPROC
  static bool GetInitialized()
  { return m_Initialized; }
  static usptr_t * GetThreadArena()
  { return m_ThreadArena; }

  static void Initialize();
#endif
  
  /** This is the structure that is passed to the thread that is
   * created from the SingleMethodExecute, MultipleMethodExecute or
   * the SpawnThread method. It is passed in as a void *, and it is up
   * to the method to cast correctly and extract the information.  The
   * ThreadID is a number between 0 and NumberOfThreads-1 that
   * indicates the id of this thread. The NumberOfThreads is
   * this->NumberOfThreads for threads created from
   * SingleMethodExecute or MultipleMethodExecute, and it is 1 for
   * threads created from SpawnThread.  The UserData is the (void
   * *)arg passed into the SetSingleMethod, SetMultipleMethod, or
   * SpawnThread method. */
  struct ThreadInfoStruct
  {
#ifdef ITK_USE_SPROC
    char Pad1[128];
#endif
    int                 ThreadID;
    int                 NumberOfThreads;
    int                 *ActiveFlag;
    MutexLock::Pointer  ActiveFlagLock;
    void                *UserData;
#ifdef ITK_USE_SPROC
    char Pad2[128];
#endif
  };
  
protected:
  MultiThreader();
  ~MultiThreader();
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  
#ifdef ITK_USE_SPROC
  static bool m_Initialized;
  static usptr_t * m_ThreadArena;
  static int m_DevzeroFd;
#endif

  MultiThreader(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The number of threads to use. */
  int                        m_NumberOfThreads;

  /** An array of thread info containing a thread id
   *  (0, 1, 2, .. ITK_MAX_THREADS-1), the thread count, and a pointer
   *  to void so that user data can be passed to each thread. */
  ThreadInfoStruct           m_ThreadInfoArray[ITK_MAX_THREADS];

  /** The methods to invoke. */
  ThreadFunctionType         m_SingleMethod;
  ThreadFunctionType         m_MultipleMethod[ITK_MAX_THREADS];
  
  /** Storage of MutexFunctions and ints used to control spawned 
   *  threads and the spawned thread ids. */
  int                        m_SpawnedThreadActiveFlag    [ITK_MAX_THREADS];
  MutexLock::Pointer         m_SpawnedThreadActiveFlagLock[ITK_MAX_THREADS];
  ThreadProcessIDType        m_SpawnedThreadProcessID     [ITK_MAX_THREADS];
  ThreadInfoStruct           m_SpawnedThreadInfoArray     [ITK_MAX_THREADS];
  
  /** Internal storage of the data. */
  void                       *m_SingleData;
  void                       *m_MultipleData[ITK_MAX_THREADS];

  /** Statics variables. */
  static int                  m_GlobalMaximumNumberOfThreads;
  static int                  m_GlobalDefaultNumberOfThreads;
  
  /** Friends of Multithreader.
   * ProcessObject is a friend so that it can call PrintSelf() on its
   * Multithreader. */
  friend class ProcessObject;
}; 
 
}  // end namespace itk
#endif

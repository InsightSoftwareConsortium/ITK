/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMutexLock.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkFastMutexLock.h"

namespace itk
{

// Construct a new vtkMutexLock 
SimpleFastMutexLock::SimpleFastMutexLock()
{
#ifdef VTK_USE_SPROC
  init_lock( &m_FastMutexLock );
#endif

#if defined(_WIN32) && !defined(ITK_USE_PTHREADS)
  //this->MutexLock = CreateMutex( NULL, FALSE, NULL ); 
  InitializeCriticalSection(&m_FastMutexLock);
#endif

#ifdef VTK_USE_PTHREADS
#ifdef VTK_HP_PTHREADS
  pthread_mutex_init(&(m_FastMutexLock), pthread_mutexattr_default);
#else
  pthread_mutex_init(&(m_FastMutexLock), NULL);
#endif
#endif

}

// Destruct the vtkMutexVariable
SimpleFastMutexLock::~SimpleFastMutexLock()
{
#if defined(_WIN32) && !defined(ITK_USE_PTHREADS)
  //CloseHandle(this->MutexLock);
  DeleteCriticalSection(&m_FastMutexLock);
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_destroy( &m_FastMutexLock);
#endif
}

// Lock the FastMutexLock
void SimpleFastMutexLock::Lock()
{
#ifdef ITK_USE_SPROC
  spin_lock( &m_FastMutexLock );
#endif

#if defined(_WIN32) && !defined(ITK_USE_PTHREADS)
  //WaitForSingleObject( this->MutexLock, INFINITE );
  EnterCriticalSection(&m_FastMutexLock);
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_lock( &m_FastMutexLock);
#endif
}

// Unlock the FastMutexLock
void SimpleFastMutexLock::Unlock()
{
#ifdef ITK_USE_SPROC
  release_lock( &m_FastMutexLock );
#endif

#if defined(_WIN32) && !defined(ITK_USE_PTHREADS)
  //ReleaseMutex( this->MutexLock );
  LeaveCriticalSection(&m_FastMutexLock);
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_unlock( &m_FastMutexLock);
#endif
}

void FastMutexLock::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os, indent);
}

}//end namespace itk

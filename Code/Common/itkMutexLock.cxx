/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutexLock.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkMutexLock.h"

namespace itk
{

// New for the SimpleMutex
SimpleMutexLock *SimpleMutexLock::New()
{
  return new SimpleMutexLock;
}

// Construct a new MutexLock 
SimpleMutexLock::SimpleMutexLock()
{
#ifdef ITK_USE_SPROC
  init_lock( &m_MutexLock );
#endif

#ifdef _WIN32
  m_MutexLock = CreateMutex( NULL, FALSE, NULL ); 
#endif

#ifdef ITK_USE_PTHREADS
#ifdef ITK_HP_PTHREADS
  pthread_mutex_init(&m_MutexLock, pthread_mutexattr_default);
#else
  pthread_mutex_init(&m_MutexLock, NULL);
#endif
#endif

}

// Destruct the MutexVariable
SimpleMutexLock::~SimpleMutexLock()
{
#ifdef _WIN32
  CloseHandle(m_MutexLock);
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_destroy( &m_MutexLock);
#endif
}

// Lock the MutexLock
void SimpleMutexLock::Lock()
{
#ifdef ITK_USE_SPROC
  spin_lock( &m_MutexLock );
#endif

#ifdef _WIN32
  WaitForSingleObject( m_MutexLock, INFINITE );
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_lock( &m_MutexLock);
#endif
}

// Unlock the MutexLock
void SimpleMutexLock::Unlock()
{
#ifdef ITK_USE_SPROC
  release_lock( &m_MutexLock );
#endif

#ifdef _WIN32
  ReleaseMutex( m_MutexLock );
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_unlock( &m_MutexLock);
#endif
}

void MutexLock::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os, indent);
}

}//end namespace itk

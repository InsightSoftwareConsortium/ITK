/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutexLock.cxx
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
#include "itkMutexLock.h"

namespace itk
{
// New for the SimpleMutex
SimpleMutexLock * SimpleMutexLock::New()
{
  return new SimpleMutexLock;
}

// Construct a new MutexLock
SimpleMutexLock::SimpleMutexLock()
{
#ifdef ITK_USE_WIN32_THREADS
  m_MutexLock = CreateMutex(NULL, FALSE, NULL);
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
#ifdef ITK_USE_WIN32_THREADS
  CloseHandle(m_MutexLock);
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_destroy(&m_MutexLock);
#endif
}

// Lock the MutexLock
void SimpleMutexLock::Lock()
{
#ifdef ITK_USE_WIN32_THREADS
  WaitForSingleObject(m_MutexLock, INFINITE);
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_lock(&m_MutexLock);
#endif
}

// Unlock the MutexLock
void SimpleMutexLock::Unlock()
{
#ifdef ITK_USE_WIN32_THREADS
  ReleaseMutex(m_MutexLock);
#endif

#ifdef ITK_USE_PTHREADS
  pthread_mutex_unlock(&m_MutexLock);
#endif
}

void MutexLock::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} //end namespace itk

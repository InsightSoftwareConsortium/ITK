/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMutexLock.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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

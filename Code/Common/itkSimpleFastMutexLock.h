/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleFastMutexLock.h
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
#ifndef __itkSimpleFastMutexLock_h
#define __itkSimpleFastMutexLock_h

#include "itkMacro.h"

#ifdef ITK_USE_SPROC
#include <abi_mutex.h>
#endif

#ifdef ITK_USE_PTHREADS
#include <pthread.h>
#endif
 
#if defined(_WIN32) && !defined(ITK_USE_PTHREADS)
#include "itkWindows.h"
#endif

namespace itk
{

#ifdef ITK_USE_SPROC
#include <abi_mutex.h>
typedef abilock_t FastMutexType;
#endif

#ifdef ITK_USE_PTHREADS
#include <pthread.h>
typedef pthread_mutex_t FastMutexType;
#endif
 
#if defined(_WIN32) && !defined(ITK_USE_PTHREADS)
#include <winbase.h>
typedef CRITICAL_SECTION FastMutexType;
#endif

#ifndef ITK_USE_SPROC
#ifndef ITK_USE_PTHREADS
#ifndef _WIN32
typedef int FastMutexType;
#endif
#endif
#endif

/** \class SimpleFastMutexLock
 * \brief Critical section locking class that can be allocated on the stack.
 * 
 * SimpleFastMutexLock is used by FastMutexLock to perform mutex locking.
 * SimpleFastMutexLock is not a subclass of Object and is designed to be
 * allocated on the stack.
 *
 * \ingroup OSSystemObjects
 */

// Critical Section object that is not a itkObject.
class ITK_EXPORT SimpleFastMutexLock
{
public:
  /** 
   * Standard "Self" typedef. 
   */
  typedef SimpleFastMutexLock       Self;
  
  /**
   * Constructor left public purposely because of stack allocation.
   */
  SimpleFastMutexLock();
  ~SimpleFastMutexLock();

  /**
   * Lock access.
   */
  void Lock( void ) const;

  /**
   * Unlock access.
   */
  void Unlock( void ) const;

protected:
  mutable FastMutexType   m_FastMutexLock;
};


}//end itk namespace
#endif


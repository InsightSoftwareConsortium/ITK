/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutexLock.h
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
#ifndef __itkMutexLock_h
#define __itkMutexLock_h

#include "itkObject.h"
#include "itkObjectFactory.h"

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
typedef abilock_t MutexType;
#endif

#ifdef ITK_USE_PTHREADS
typedef pthread_mutex_t MutexType;
#endif
 
#if defined(_WIN32) && !defined(ITK_USE_PTHREADS)
typedef HANDLE MutexType;
#endif

#ifndef ITK_USE_SPROC
#ifndef ITK_USE_PTHREADS
#ifndef _WIN32
typedef int MutexType;
#endif
#endif
#endif

/** \class SimpleMutexLock 
 * \brief Simple mutual exclusion locking class.
 
 * SimpleMutexLock allows the locking of variables which are accessed 
 * through different threads.  This header file also defines 
 * SimpleMutexLock which is not a subclass of Object.
 * 
 * \ingroup OSSystemObjects
 */
class ITK_EXPORT SimpleMutexLock
{
public:
  /** 
   * Standard "Self" typedef. 
   */
  typedef SimpleMutexLock       Self;
  
  /**
   * Constructor left public purposely
   */
  SimpleMutexLock();
  virtual ~SimpleMutexLock();

  /**
   * Method for creation through the object factory.
   */
  static SimpleMutexLock *New();
  void Delete() {delete this;}

  virtual const char *GetNameOfClass() {return "itkSimpleMutexLock";};
  
  /**
   * Lock the MutexLock.
   */
  void Lock( void );

  /**
   * Unlock the MutexLock.
   */
  void Unlock( void );

protected:
  MutexType   m_MutexLock;
};

/** \class MutexLock 
 * \brief Mutual exclusion locking class.
 *
 * MutexLock allows the locking of variables which are accessed 
 * through different threads.  This header file also defines 
 * SimpleMutexLock which is not a subclass of itkObject.
 * 
 * \ingroup OSSystemObjects
 */
class ITK_EXPORT MutexLock : public Object
{
public:
  /** 
   * Standard "Self" typedef.
   */
  typedef MutexLock       Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /** 
   * Smart pointer typedef support. 
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation.
   */
  itkNewMacro(Self);
  itkTypeMacro(MutexLock,Object);

  /**
   * Lock the itkMutexLock.
   */
  void Lock( void );

  /**
   * Unlock the MutexLock.
   */
  void Unlock( void );

protected:
  SimpleMutexLock   m_SimpleMutexLock;
  void PrintSelf(std::ostream& os, Indent indent) const;
};


inline void MutexLock::Lock( void )
{
  m_SimpleMutexLock.Lock();
}

inline void MutexLock::Unlock( void )
{
  m_SimpleMutexLock.Unlock();
}


}//end itk namespace
#endif

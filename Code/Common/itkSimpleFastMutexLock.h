/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSimpleFastMutexLock.h
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
class ITKCommon_EXPORT SimpleFastMutexLock
{
public:
  /** Standard class typedefs.  */
  typedef SimpleFastMutexLock       Self;
  
  /** Constructor and destructor left public purposely because of stack allocation. */
  SimpleFastMutexLock();
  ~SimpleFastMutexLock();
  
  /** Lock access. */
  void Lock( void ) const;

  /** Unlock access. */
  void Unlock( void ) const;

protected:
  mutable FastMutexType   m_FastMutexLock;
};

}//end itk namespace
#endif


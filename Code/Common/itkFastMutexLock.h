/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastMutexLock.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkFastMutexLock_h
#define __itkFastMutexLock_h

#include "itkObject.h"
#include "itkObjectFactory.h"

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
 
#ifdef _WIN32
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
   * Constructor left public purposely because of allocation
   * on the stack.
   */
  SimpleFastMutexLock();
  virtual ~SimpleFastMutexLock();

  /**
   * Method for creation through the object factory.
   */
  static SimpleFastMutexLock *New();
  void Delete() {delete this;}

  virtual const char *GetClassName() 
    {return "SimpleFastMutexLock";};
  
  /**
   * Lock access.
   */
  void Lock( void );

  /**
   * Unlock access.
   */
  void Unlock( void );

protected:
  FastMutexType   m_FastMutexLock;
};

/** \class FastMutexLock
 * \brief Critical section locking class.
 * 
 * FastMutexLock allows the locking of variables which are accessed 
 * through different threads.  This header file also defines 
 * SimpleFastMutexLock which is not a subclass of Object.
 * The API is identical to that of MutexLock, and the behavior is
 * identical as well, except on Windows 9x/NT platforms. The only difference
 * on these platforms is that MutexLock is more flexible, in that
 * it works across processes as well as across threads, but also costs
 * more, in that it evokes a 600-cycle x86 ring transition. The 
 * FastMutexLock provides a higher-performance equivalent (on 
 * Windows) but won't work across processes. Since it is unclear how,
 * in itk, an object at the itk level can be shared across processes
 * in the first place, one should use FastMutexLock unless one has
 * a very good reason to use MutexLock. If higher-performance equivalents
 * for non-Windows platforms (Irix, SunOS, etc) are discovered, they
 * should replace the implementations in this class
 */
class ITK_EXPORT FastMutexLock : public Object
{
public:
  /** 
   * Standard "Self" typedef.
   */
  typedef FastMutexLock       Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /** 
   * Smart pointer typedef support. 
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation.
   */
  itkNewMacro(Self);
  itkTypeMacro(FastMutexLock,Object);

  /**
   * Lock the itkFastMutexLock.
   */
  void Lock( void );

  /**
   * Unlock the FastMutexLock.
   */
  void Unlock( void );

protected:
  SimpleFastMutexLock   m_SimpleFastMutexLock;
  void PrintSelf(std::ostream& os, Indent indent);
};


inline void FastMutexLock::Lock( void )
{
  m_SimpleFastMutexLock.Lock();
}

inline void FastMutexLock::Unlock( void )
{
  m_SimpleFastMutexLock.Unlock();
}


#endif

}//end itk namespace

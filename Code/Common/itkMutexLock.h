/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutexLock.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkMutexVariable_h
#define __itkMutexVariable_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{

#ifdef ITK_USE_SPROC
#include <abi_mutex.h>
typedef abilock_t MutexType;
#endif

#ifdef ITK_USE_PTHREADS
#include <pthread.h>
typedef pthread_mutex_t MutexType;
#endif
 
#ifdef _WIN32
#include <winbase.h>
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
 * .SECTION Description
 * SimpleMutexLock allows the locking of variables which are accessed 
 * through different threads.  This header file also defines 
 * SimpleMutexLock which is not a subclass of Object.
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

  virtual const char *GetClassName() {return "itkSimpleMutexLock";};
  
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
 * .SECTION Description
 * MutexLock allows the locking of variables which are accessed 
 * through different threads.  This header file also defines 
 * SimpleMutexLock which is not a subclass of itkObject.
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
  void PrintSelf(std::ostream& os, Indent indent);
};


inline void MutexLock::Lock( void )
{
  m_SimpleMutexLock.Lock();
}

inline void MutexLock::Unlock( void )
{
  m_SimpleMutexLock.Unlock();
}

#endif

}//end itk namespace

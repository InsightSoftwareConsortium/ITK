/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMutexLock.h
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
 
#ifdef ITK_USE_WIN32_THREADS
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
 
#ifdef ITK_USE_WIN32_THREADS
typedef HANDLE MutexType;
#endif

#ifndef ITK_USE_SPROC
#ifndef ITK_USE_PTHREADS
#ifndef ITK_USE_WIN32_THREADS
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
class ITKCommon_EXPORT SimpleMutexLock
{
public:
  /** Standard class typedefs.  */
  typedef SimpleMutexLock       Self;
  
  /** Constructor and destructor left public purposely. */
  SimpleMutexLock();
  virtual ~SimpleMutexLock();
  
  /** Methods for creation and destruction through the object factory. */
  static SimpleMutexLock *New();
  void Delete() {delete this;}
  
  /** Used for debugging and other run-time purposes. */
  virtual const char *GetNameOfClass() {return "itkSimpleMutexLock";};
  
  /** Lock the MutexLock. */
  void Lock( void );

  /** Unlock the MutexLock. */
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
class ITKCommon_EXPORT MutexLock : public Object
{
public:
  /** Standard class typedefs. */
  typedef MutexLock       Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation. */
  itkNewMacro(Self);
  
  /** Run-time information. */
  itkTypeMacro(MutexLock,Object);

  /** Lock the itkMutexLock. */
  void Lock( void );

  /** Unlock the MutexLock. */
  void Unlock( void );

protected:
  MutexLock() {}
  ~MutexLock() {}
  
  SimpleMutexLock   m_SimpleMutexLock;
  void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  MutexLock(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
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

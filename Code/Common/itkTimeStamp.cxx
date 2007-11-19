/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTimeStamp.cxx
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
#include "itkTimeStamp.h"
#include "itkFastMutexLock.h"
#if defined(__APPLE__)
  #include <libkern/OSAtomic.h>
#endif

namespace itk
{

/**
 * Instance creation.
 */
TimeStamp*
TimeStamp
::New()
{
  return new Self;
}
  
 
/**
 * Make sure the new time stamp is greater than all others so far.
 */
void 
TimeStamp
::Modified()
{
// Windows optimization
#if defined(WIN32) || defined(_WIN32)
  static LONG itkTimeStampTime = 0;
  this->ModifiedTime = (unsigned long)InterlockedIncrement(&itkTimeStampTime);

// Mac optimization (64 bit)
#elif defined(__APPLE__) && __LP64__
  // "m_ModifiedTime" is "unsigned long", a type that changess sizes
  // depending on architecture.  The atomic increment is safe, since it
  // operates on a variable of the exact type needed.  The cast does not
  // change the size, but does change signedness, which is not ideal.
  static volatile int64_t itkTimeStampTime = 0;
  m_ModifiedTime = (unsigned long)OSAtomicIncrement64Barrier(&itkTimeStampTime);

// Mac optimization (32 bit, 10.4 or later)
#elif defined(__APPLE__) && (MAC_OS_X_VERSION_MIN_REQUIRED >= 1040)
  static volatile int32_t itkTimeStampTime = 0;
  m_ModifiedTime = (unsigned long)OSAtomicIncrement32Barrier(&itkTimeStampTime);

// General case
#else
  /**
   * Initialize static member
   */
  static unsigned long itkTimeStampTime = 0;

  /** Used for mutex locking */
  static SimpleFastMutexLock TimeStampMutex;
  
  TimeStampMutex.Lock();
  m_ModifiedTime = ++itkTimeStampTime;
  TimeStampMutex.Unlock();
#endif


}

} // end namespace itk

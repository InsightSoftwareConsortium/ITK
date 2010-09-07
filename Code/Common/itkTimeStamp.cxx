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

#if defined( _WIN32 )
  #include "itkWindows.h"

#elif defined( __APPLE__ )
// OSAtomic.h optimizations only used in 10.5 and later
  #include <AvailabilityMacros.h>
  #if MAC_OS_X_VERSION_MAX_ALLOWED >= 1050
    #include <libkern/OSAtomic.h>
  #endif

#elif defined( __GLIBCPP__ ) || defined( __GLIBCXX__ )
  #if ( __GNUC__ > 4 ) || ( ( __GNUC__ == 4 ) && ( __GNUC_MINOR__ >= 2 ) )
  #include <ext/atomicity.h>
  #else
  #include <bits/atomicity.h>
  #endif

#endif

namespace itk
{
#if defined( __GLIBCXX__ ) // g++ 3.4+

using __gnu_cxx::__exchange_and_add;

#endif

/**
 * Instance creation.
 */
TimeStamp *
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
#if defined( WIN32 ) || defined( _WIN32 )
  static LONG itkTimeStampTime = 0;
  m_ModifiedTime = (unsigned long)InterlockedIncrement(&itkTimeStampTime);

  // Mac optimization
#elif defined( __APPLE__ ) && ( MAC_OS_X_VERSION_MIN_REQUIRED >= 1050 )
 #if __LP64__
  // "m_ModifiedTime" is "unsigned long", a type that changess sizes
  // depending on architecture.  The atomic increment is safe, since it
  // operates on a variable of the exact type needed.  The cast does not
  // change the size, but does change signedness, which is not ideal.
  static volatile int64_t itkTimeStampTime = 0;
  m_ModifiedTime = (unsigned long)OSAtomicIncrement64Barrier(&itkTimeStampTime);
 #else
  static volatile int32_t itkTimeStampTime = 0;
  m_ModifiedTime = (unsigned long)OSAtomicIncrement32Barrier(&itkTimeStampTime);
 #endif

// gcc optimization
#elif defined( __GLIBCPP__ ) || defined( __GLIBCXX__ )
  // We start from 1 since __exchange_and_add returns the old (non-incremented)
  // value. This is not really necessary but will make the absolute value of the
  // timestamp more consistent across platforms.
  static volatile _Atomic_word itkTimeStampTime = 1;
  m_ModifiedTime = (unsigned long)__exchange_and_add(&itkTimeStampTime, 1);

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

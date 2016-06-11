/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkMutexLockHolder_h
#define itkMutexLockHolder_h

#include "itkMacro.h"

namespace itk
{
/** \class MutexLockHolder
 *  \brief A container to store a Mutex.
 *  This holder class for ensuring that locks are released in
 *  the event of an exception being thrown after the lock was
 *  created.
 *
 * \ingroup ITKCommon
 */
template< typename TMutex >
class MutexLockHolder
{
public:
  typedef MutexLockHolder Self;
  typedef TMutex          MutexType;

  MutexLockHolder(MutexType & mutex, const bool noblock=false)
   :m_Mutex(mutex),
    m_LockCaptured(true)
  {
    if( noblock == false )
      {
      m_Mutex.Lock();
      }
    else
      {
      m_LockCaptured = m_Mutex.TryLock();
      }
  }

  /** True if the holder has acquired the lock, for no-blocking
   + constructor this will always be true.
   */
  inline bool GetLockCaptured() const { return this->m_LockCaptured; }
  operator bool () const { return this->m_LockCaptured; }

  ~MutexLockHolder()
  {
    if ( m_LockCaptured )
     {
      m_Mutex.Unlock();
     }
  }

protected:
  MutexType & m_Mutex;
  bool        m_LockCaptured;
private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MutexLockHolder);

};

} //end itk namespace

#endif

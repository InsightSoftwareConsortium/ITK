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
#ifndef itkFastMutexLock_h
#define itkFastMutexLock_h

#include "itkObject.h"
#include "itkSimpleFastMutexLock.h"
#include "itkObjectFactory.h"

namespace itk
{
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
 *
 * \ingroup OSSystemObjects
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT FastMutexLock:public Object
{
public:
  /** Standard class typedefs. */
  typedef FastMutexLock              Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation. */
  itkNewMacro(Self);

  /** Run-time type information. */
  itkTypeMacro(FastMutexLock, Object);

  /** Lock the itkFastMutexLock. */
  void Lock();

  /** Non-blocking Lock access.
   \return bool - true if lock is captured, false if it was already held by someone else.
   */
  bool TryLock();

  /** Unlock the FastMutexLock. */
  void Unlock();

protected:
  FastMutexLock() {}
  ~FastMutexLock() ITK_OVERRIDE {}

  SimpleFastMutexLock m_SimpleFastMutexLock;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(FastMutexLock);
};

inline void FastMutexLock::Lock()
{
  m_SimpleFastMutexLock.Lock();
}

inline bool FastMutexLock::TryLock()
{
  return m_SimpleFastMutexLock.TryLock();
}

inline void FastMutexLock::Unlock()
{
  m_SimpleFastMutexLock.Unlock();
}
} //end itk namespace
#endif

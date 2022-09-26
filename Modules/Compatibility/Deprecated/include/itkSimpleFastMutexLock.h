/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#ifndef itkSimpleFastMutexLock_h
#define itkSimpleFastMutexLock_h

#include "itkMacro.h"
#include "itkThreadSupport.h"
#include "ITKDeprecatedExport.h"

namespace itk
{

/**
 * \class SimpleFastMutexLock
 * \brief Critical section locking class that can be allocated on the stack.
 *
 * SimpleFastMutexLock is used by FastMutexLock to perform mutex locking.
 * SimpleFastMutexLock is not a subclass of Object and is designed to be
 * allocated on the stack.
 *
 * \ingroup OSSystemObjects
 * \ingroup ITKDeprecated
 */

// Critical Section object that is not a itkObject.
class ITKDeprecated_EXPORT SimpleFastMutexLock
{
public:
  /** Standard class type aliases.  */
  using Self = SimpleFastMutexLock;

  /** Constructor and destructor left public purposely because of stack
    allocation. */
  SimpleFastMutexLock();
  ~SimpleFastMutexLock();

  /** Lock access. */
  void
  Lock() const;

  /** Non-blocking Lock access.
   \return bool - true if lock is captured, false if it was already heald by someone else.
   */
  bool
  TryLock() const;

  /** Unlock access. */
  void
  Unlock() const;

protected:
  mutable FastMutexType m_FastMutexLock;
};
} // namespace itk
#endif

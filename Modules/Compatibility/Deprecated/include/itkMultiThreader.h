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

#ifndef itkMultiThreader_h
#define itkMultiThreader_h

#if !defined(ITK_LEGACY_REMOVE)
#  include "itkMultiThreaderBase.h"
#  include "itkPlatformMultiThreader.h"
namespace itk
{
/** Since ITK 5.0 MultiThreader has been split into a class hierarchy.
 *  Most of the time you will want to replace it by MultiThreaderBase.
 *
 *  Additionally, call this->DynamicMultiThreadingOff(); prior to Update()
 *  (for example in constructor) if any of the following is true:
 *  - Your filter needs a constant, in-advance known number of threads
 *  - Your filter uses threadId parameter in ThreadedGenerateData()
 *  - Your filter uses a custom region splitting method */
using MultiThreader = MultiThreaderBase;


/** Replace it by PlatformMultiThreader if any of the following is true:
 *  - Your filter uses cross-thread synchronization e.g. itkBarrier
 *  - Your filter uses MultipleMethodExecute()
 *  - Your filter uses SpawnThread/TerminateThread. */
// using MultiThreader = PlatformMultiThreader;
} // namespace itk
#else // ITK_LEGACY_REMOVE
#  error itkMultiThreader.h is a legacy file since ITK 5.0 and will be removed in the future.
#endif // ITK_LEGACY_REMOVE

#endif // itkMultiThreader_h

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

#include "itkImageRegionSplitterSlowDimension.h"
#include "itkImageSourceCommon.h"
#include "itkSimpleFastMutexLock.h"
#include "itkMutexLockHolder.h"

namespace itk
{

namespace
{
SimpleFastMutexLock globalDefaultSplitterLock;
ImageRegionSplitterBase::Pointer globalDefaultSplitter;
}

const ImageRegionSplitterBase*  ImageSourceCommon::GetGlobalDefaultSplitter(void)
{
  if ( globalDefaultSplitter.IsNull() )
    {
    // thread safe lazy initialization, prevent race condition on
    // setting, with an atomic set if null.
    MutexLockHolder< SimpleFastMutexLock > lock(globalDefaultSplitterLock);
    if ( globalDefaultSplitter.IsNull() )
      {
      globalDefaultSplitter = ImageRegionSplitterSlowDimension::New().GetPointer();
      }
    }
  return globalDefaultSplitter;
}


}

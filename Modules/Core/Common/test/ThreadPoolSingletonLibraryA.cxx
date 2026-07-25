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
#include "itkSingleton.h"
#include "itkThreadPool.h"

// Each of these libraries links its own copy of a statically built ITKCommon, the way every
// wrapped Python module does. They share only the SingletonIndex handed to them by the driver.
extern "C" ITK_ABI_EXPORT void
ThreadPoolSingletonLibraryA_AdoptSingletonIndex(itk::SingletonIndex * singletonIndex)
{
  itk::SingletonIndex::SetInstance(singletonIndex);
}

extern "C" ITK_ABI_EXPORT void *
ThreadPoolSingletonLibraryA_GetThreadPool()
{
  const itk::ThreadPool::Pointer threadPool = itk::ThreadPool::GetInstance();
  threadPool->Register(); // Pin it, so a duplicate singleton cannot reuse the freed address.
  return threadPool.GetPointer();
}

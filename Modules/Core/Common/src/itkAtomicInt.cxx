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

  Program:   Visualization Toolkit
  Module:    vtkAtomicInt.cxx

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/

#include "itkAtomicInt.h"
#include "itkMutexLockHolder.h"


#if !defined(ITK_GCC_ATOMICS_32) && !defined(ITK_APPLE_ATOMICS_32) &&\
    !defined(ITK_WINDOWS_ATOMICS_32) && !ITK_COMPILER_CXX_ATOMIC
# define ITK_LOCK_BASED_ATOMICS_32
#endif

#if !defined(ITK_GCC_ATOMICS_64) && !defined(ITK_APPLE_ATOMICS_64) &&\
    !defined(ITK_WINDOWS_ATOMICS_64) && !ITK_COMPILER_CXX_ATOMIC
# define ITK_LOCK_BASED_ATOMICS_64
#endif

namespace itk
{

#if defined(ITK_LOCK_BASED_ATOMICS_32) || defined(ITK_LOCK_BASED_ATOMICS_64)

#if defined(ITK_LOCK_BASED_ATOMICS_64)
Detail::AtomicOps<8>::AtomicType::AtomicType(int64_t init)
  : var(init),
    mutex( new SimpleFastMutexLock )
{
}

Detail::AtomicOps<8>::AtomicType::~AtomicType()
{
  delete mutex;
}
#endif

#if defined(ITK_LOCK_BASED_ATOMICS_32)
Detail::AtomicOps<4>::AtomicType::AtomicType(int32_t init)
  : var(init),
  mutex( new SimpleFastMutexLock )
{
}

Detail::AtomicOps<4>::AtomicType::~AtomicType()
{
  delete mutex;
}
#endif

#endif // ITK_LOCK_BASED_ATOMICS


namespace Detail
{

#if defined(ITK_WINDOWS_ATOMICS_64) || defined(ITK_LOCK_BASED_ATOMICS_64)

int64_t AtomicOps<8>::AddAndFetch(AtomicType *ref, int64_t val)
{
#if defined(ITK_WINDOWS_ATOMICS_64)
# if defined(ITK_HAS_INTERLOCKEDADD)
  return InterlockedAdd64(ref, val);
# else
  return InterlockedExchangeAdd64(ref, val) + val;
# endif
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return ref->var += val;
#endif
}

int64_t AtomicOps<8>::SubAndFetch(AtomicType *ref, int64_t val)
{
#if defined(ITK_WINDOWS_ATOMICS_64)
# if defined(ITK_HAS_INTERLOCKEDADD)
  return InterlockedAdd64(ref, -val);
# else
  return InterlockedExchangeAdd64(ref, -val) - val;
# endif
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return ref->var -= val;
#endif
}

int64_t AtomicOps<8>::PreIncrement(AtomicType *ref)
{
#if defined(ITK_WINDOWS_ATOMICS_64)
  return InterlockedIncrement64(ref);
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return ++(ref->var);
#endif
}

int64_t AtomicOps<8>::PreDecrement(AtomicType *ref)
{
#if defined(ITK_WINDOWS_ATOMICS_64)
  return InterlockedDecrement64(ref);
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return --(ref->var);
#endif
}

int64_t AtomicOps<8>::PostIncrement(AtomicType *ref)
{
#if defined(ITK_WINDOWS_ATOMICS_64)
  int64_t val = InterlockedIncrement64(ref);
  return --val;
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return (ref->var)++;
#endif
}

int64_t AtomicOps<8>::PostDecrement(AtomicType *ref)
{
#if defined(ITK_WINDOWS_ATOMICS_64)
  int64_t val = InterlockedDecrement64(ref);
  return ++val;
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return (ref->var)--;
#endif
}

int64_t AtomicOps<8>::Load(const AtomicType *ref)
{
#if defined(ITK_WINDOWS_ATOMICS_64)
  int64_t val;
  InterlockedExchange64(&val, *ref);
  return val;
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return ref->var;
#endif
}

void AtomicOps<8>::Store(AtomicType *ref, int64_t val)
{
#if defined(ITK_WINDOWS_ATOMICS_64)
  InterlockedExchange64(ref, val);
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  ref->var = val;
#endif
}

#endif // defined(ITK_WINDOWS_ATOMICS_64) || defined(ITK_LOCK_BASED_ATOMICS_64)


#if defined(ITK_WINDOWS_ATOMICS_32) || defined(ITK_LOCK_BASED_ATOMICS_32)

int32_t AtomicOps<4>::AddAndFetch(AtomicType *ref, int32_t val)
{
#if defined(ITK_WINDOWS_ATOMICS_32)
# if defined(ITK_HAS_INTERLOCKEDADD)
  return InterlockedAdd(reinterpret_cast<long*>(ref), val);
# else
  return InterlockedExchangeAdd(reinterpret_cast<long*>(ref), val) + val;
# endif
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return ref->var += val;
#endif
}

int32_t AtomicOps<4>::SubAndFetch(AtomicType *ref, int32_t val)
{
#if defined(ITK_WINDOWS_ATOMICS_32)
# if defined(ITK_HAS_INTERLOCKEDADD)
  return InterlockedAdd(reinterpret_cast<long*>(ref), -val);
# else
  return InterlockedExchangeAdd(reinterpret_cast<long*>(ref), -val) - val;
# endif
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return ref->var -= val;
#endif
}

int32_t AtomicOps<4>::PreIncrement(AtomicType *ref)
{
#if defined(ITK_WINDOWS_ATOMICS_32)
  return InterlockedIncrement(reinterpret_cast<long*>(ref));
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return ++(ref->var);
#endif
}

int32_t AtomicOps<4>::PreDecrement(AtomicType *ref)
{
#if defined(ITK_WINDOWS_ATOMICS_32)
  return InterlockedDecrement(reinterpret_cast<long*>(ref));
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return --(ref->var);
#endif
}

int32_t AtomicOps<4>::PostIncrement(AtomicType *ref)
{
#if defined(ITK_WINDOWS_ATOMICS_32)
  int32_t val = InterlockedIncrement(reinterpret_cast<long*>(ref));
  return --val;
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return (ref->var)++;
#endif
}

int32_t AtomicOps<4>::PostDecrement(AtomicType *ref)
{
#if defined(ITK_WINDOWS_ATOMICS_32)
  int32_t val = InterlockedDecrement(reinterpret_cast<long*>(ref));
  return ++val;
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return (ref->var)--;
#endif
}

int32_t AtomicOps<4>::Load(const AtomicType *ref)
{
#if defined(ITK_WINDOWS_ATOMICS_32)
  long val;
  InterlockedExchange(&val, *ref);
  return val;
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  return ref->var;
#endif
}

void AtomicOps<4>::Store(AtomicType *ref, int32_t val)
{
#if defined(ITK_WINDOWS_ATOMICS_32)
  InterlockedExchange(reinterpret_cast<long*>(ref), val);
#else
  MutexLockHolder<SimpleFastMutexLock> mutexHolder(*ref->mutex);
  ref->var = val;
#endif
}

#endif // defined(ITK_WINDOWS_ATOMICS_32) || defined(ITK_LOCK_BASED_ATOMICS_32)

} // namespace Detail

} // namespace itk

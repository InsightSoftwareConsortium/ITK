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
 * Program:   Visualization Toolkit
 * Module:    vtkAtomicInt.h
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *  All rights reserved.
 *  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.
 *
 *     This software is distributed WITHOUT ANY WARRANTY; without even
 *     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE.  See the above copyright notice for more information.
 *
 *=========================================================================*/

#ifndef itkAtomicIntDetail_h
#define itkAtomicIntDetail_h

#include "itkMacro.h"
#include "itkIntTypes.h"
#include "itkSimpleFastMutexLock.h"
#include "itkConceptChecking.h"

#if ITK_COMPILER_CXX_ATOMIC
# include <atomic>
#elif defined(ITK_HAVE_SYNC_BUILTINS)
# define ITK_GCC_ATOMICS_32
# define ITK_GCC_ATOMICS_64
#elif defined(__APPLE__)
# include <libkern/OSAtomic.h>
# define ITK_APPLE_ATOMICS_32
# if ITK_SIZEOF_VOID_P == 8 || defined(__i386__)
#   define ITK_APPLE_ATOMICS_64
# endif
#elif defined(_WIN32) && defined(_MSC_VER)
# define ITK_WINDOWS_ATOMICS_32
# if ITK_SIZEOF_VOID_P == 8
#   define ITK_WINDOWS_ATOMICS_64
# endif
#endif

namespace itk
{

namespace Detail
{

template <size_t VSize> class AtomicOps;


#if ITK_COMPILER_CXX_ATOMIC

template <size_t VSize> struct BaseType;

template <size_t VSize> class AtomicOps
{
public:
  typedef typename BaseType<VSize>::Type AtomicType;
  typedef typename BaseType<VSize>::Type ValueType;
};

#elif defined ITK_HAVE_SYNC_BUILTINS

template <size_t VSize> struct BaseType;

template <size_t VSize> class AtomicOps
{
public:
  typedef typename BaseType<VSize>::Type AtomicType;
  typedef typename BaseType<VSize>::Type ValueType;

  static ValueType AddAndFetch(ValueType *ref, ValueType val)
  {
    return __sync_add_and_fetch(ref, val);
  }

  static ValueType SubAndFetch(ValueType *ref, ValueType val)
  {
    return __sync_sub_and_fetch(ref, val);
  }

  static ValueType PreIncrement(ValueType *ref)
  {
    return __sync_add_and_fetch(ref, 1);
  }

  static ValueType PreDecrement(ValueType *ref)
  {
    return __sync_sub_and_fetch(ref, 1);
  }

  static ValueType PostIncrement(ValueType *ref)
  {
    return __sync_fetch_and_add(ref, 1);
  }

  static ValueType PostDecrement(ValueType *ref)
  {
    return __sync_fetch_and_sub(ref, 1);
  }

  static ValueType Load(const ValueType *ref)
  {
    __sync_synchronize();
    return *static_cast<const volatile ValueType *>(ref);
  }

  static void Store(ValueType *ref, ValueType val)
  {
    *static_cast<volatile ValueType*>(ref) = val;
    __sync_synchronize();
  }
};

#endif // defined ITK_HAVE_SYNC_BUILTINS

#if defined(ITK_GCC_ATOMICS_64) || ITK_COMPILER_CXX_ATOMIC
template<> struct BaseType<8>
{
  itkAlignedTypedef( 8, int64_t, Type );
};

#elif defined(ITK_APPLE_ATOMICS_64)
template <> class AtomicOps<8>
{
public:
  itkAlignedTypedef( 8, int64_t, AtomicType );
  typedef int64_t ValueType;

  static int64_t AddAndFetch(int64_t *ref, int64_t val)
  {
    return OSAtomicAdd64Barrier(val, ref);
  }

  static int64_t SubAndFetch(int64_t *ref, int64_t val)
  {
    return OSAtomicAdd64Barrier(-val, ref);
  }

  static int64_t PreIncrement(int64_t *ref)
  {
    return OSAtomicIncrement64Barrier(ref);
  }

  static int64_t PreDecrement(int64_t *ref)
  {
    return OSAtomicDecrement64Barrier(ref);
  }

  static int64_t PostIncrement(int64_t *ref)
  {
    int64_t val = OSAtomicIncrement64Barrier(ref);
    return --val;
  }

  static int64_t PostDecrement(int64_t *ref)
  {
    int64_t val = OSAtomicDecrement64Barrier(ref);
    return ++val;
  }

  static int64_t Load(const int64_t *ref)
  {
    OSMemoryBarrier();
    return *static_cast<const volatile int64_t*>(ref);
  }

  static void Store(int64_t *ref, int64_t val)
  {
    *static_cast<volatile int64_t*>(ref) = val;
    OSMemoryBarrier();
  }
};

#else

template <> class ITKCommon_EXPORT AtomicOps<8>
{
public:
#if defined(ITK_WINDOWS_ATOMICS_64)
  itkAlignedTypedef( 8, int64_t, AtomicType );

#else
  struct ITKCommon_EXPORT AtomicType
  {
    int64_t var;
    SimpleFastMutexLock *mutex;

    AtomicType(int64_t init);
    ~AtomicType();
  };
#endif
  typedef int64_t ValueType;

  static int64_t AddAndFetch(AtomicType *ref, int64_t val);
  static int64_t SubAndFetch(AtomicType *ref, int64_t val);
  static int64_t PreIncrement(AtomicType *ref);
  static int64_t PreDecrement(AtomicType *ref);
  static int64_t PostIncrement(AtomicType *ref);
  static int64_t PostDecrement(AtomicType *ref);
  static int64_t Load(const AtomicType *ref);
  static void Store(AtomicType *ref, int64_t val);
};

#endif

#if defined(ITK_GCC_ATOMICS_32) || ITK_COMPILER_CXX_ATOMIC
template<> struct BaseType<4>
{
  itkAlignedTypedef( 4, int32_t, Type );
};

#elif defined(ITK_APPLE_ATOMICS_32)
template <> class AtomicOps<4>
{
public:
  itkAlignedTypedef( 4, int32_t, AtomicType );
  typedef int32_t ValueType;

  static int32_t AddAndFetch(int32_t *ref, int32_t val)
  {
    return OSAtomicAdd32Barrier(val, ref);
  }

  static int32_t SubAndFetch(int32_t *ref, int32_t val)
  {
    return OSAtomicAdd32Barrier(-val, ref);
  }

  static int32_t PreIncrement(int32_t *ref)
  {
    return OSAtomicIncrement32Barrier(ref);
  }

  static int32_t PreDecrement(int32_t *ref)
  {
    return OSAtomicDecrement32Barrier(ref);
  }

  static int32_t PostIncrement(int32_t *ref)
  {
    int32_t val = OSAtomicIncrement32Barrier(ref);
    return --val;
  }

  static int32_t PostDecrement(int32_t *ref)
  {
    int32_t val = OSAtomicDecrement32Barrier(ref);
    return ++val;
  }

  static int32_t Load(const int32_t *ref)
  {
    OSMemoryBarrier();
    return *static_cast<const volatile int32_t*>(ref);
  }

  static void Store(int32_t *ref, int32_t val)
  {
    *static_cast<volatile int32_t*>(ref) = val;
    OSMemoryBarrier();
  }
};

#else

template <> class ITKCommon_EXPORT AtomicOps<4>
{
public:
#if defined(ITK_WINDOWS_ATOMICS_32)
    itkAlignedTypedef( 4, int32_t, AtomicType );
#else
  struct ITKCommon_EXPORT AtomicType
  {
    int32_t var;
    SimpleFastMutexLock *mutex;

    AtomicType(int32_t init);
    ~AtomicType();
  };
#endif
  typedef int32_t ValueType;

  static int32_t AddAndFetch(AtomicType *ref, int32_t val);
  static int32_t SubAndFetch(AtomicType *ref, int32_t val);
  static int32_t PreIncrement(AtomicType *ref);
  static int32_t PreDecrement(AtomicType *ref);
  static int32_t PostIncrement(AtomicType *ref);
  static int32_t PostDecrement(AtomicType *ref);
  static int32_t Load(const AtomicType *ref);
  static void Store(AtomicType *ref, int32_t val);
};

#endif

template <typename T>
struct IsAtomicSupportedIntegralType
{
  typedef IsAtomicSupportedIntegralType Self;
  struct Constraints {
    typedef Concept::Detail::UniqueType_bool< true >                  TrueT;
    typedef Concept::Detail::UniqueType_bool< NumericTraits<T>::is_specialized > SpecializedT;
    typedef Concept::Detail::UniqueType_bool< NumericTraits<T>::is_integer >     IntegralT;
    typedef Concept::Detail::UniqueType_bool <sizeof(T) == 4 || sizeof(T) == 8 >  SizeT;
    void constraints()
    {
      IntegralT a = TrueT();
      IntegralT b = TrueT();
      IntegralT c = TrueT();

      IgnoreUnusedVariable(a);
      IgnoreUnusedVariable(b);
      IgnoreUnusedVariable(c);
    }
  };


  itkConceptConstraintsMacro();
};

} // end namespace Detail
} // end namespace itk

#endif

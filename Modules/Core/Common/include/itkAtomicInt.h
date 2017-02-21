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

#ifndef itkAtomicInt_h
#define itkAtomicInt_h

#include "itkMacro.h"
#include "itkIntTypes.h"
#include "itkAtomicIntDetail.h"
#include "itkConceptChecking.h"

#if !ITK_COMPILER_CXX_ATOMIC
# include <cstddef>
# include <limits>
#else
# include <atomic>
#endif


namespace itk
{

/** \class AtomicInt
 *  \brief Provides support for atomic integers
 *
 *
 * Objects of atomic types are C++ objects that are free from data races;
 * that is, if one thread writes to an atomic object while another thread
 * reads from it, the behavior is well-defined. AtomicInt provides
 * a subset of the std::atomic API and implementation, for 32 bit and 64 bit
 * integers and pointer types. For these types, AtomicInt defines a
 * number of operations that happen atomically - without interruption
 * by another thread. Furthermore, these operations happen in a
 * sequentially-consistent way and use full memory fences. This means
 * that operations relating to atomic variables happen in the specified
 * order and the results are made visible to other processing cores to
 * guarantee proper sequential operation. Other memory access patterns
 * supported by std::atomic are not currently supported.
 *
 * Note that when atomic operations are not available on a particular
 * platform or compiler, mutexes, which are significantly slower, are used
 * as a fallback.
 *
 * \ingroup ITKCommon
 */

 #if !ITK_COMPILER_CXX_ATOMIC
template <typename T>
class AtomicInt
{
private:
  typedef Detail::AtomicOps<sizeof(T)> Impl;
  itkConceptMacro( SupportedInteger, ( Detail::IsAtomicSupportedIntegralType<T> ) );

public:
  AtomicInt() : m_Object(0)
  {
  }

  AtomicInt(T val)
    : m_Object(static_cast<typename Impl::ValueType>(val))
  {
  }

  AtomicInt(const AtomicInt<T> &ai)
    : m_Object(static_cast<typename Impl::ValueType>(ai.load()))
  {
  }

  T operator++()
  {
    return static_cast<T>(Impl::PreIncrement(&this->m_Object));
  }

  T operator++(int)
  {
    return static_cast<T>(Impl::PostIncrement(&this->m_Object));
  }

  T operator--()
  {
    return static_cast<T>(Impl::PreDecrement(&this->m_Object));
  }

  T operator--(int)
  {
    return static_cast<T>(Impl::PostDecrement(&this->m_Object));
  }

  T operator+=(T val)
  {
    return static_cast<T>(Impl::AddAndFetch(&this->m_Object,
      static_cast<typename Impl::ValueType>(val)));
  }

  T operator-=(T val)
  {
    return static_cast<T>(Impl::SubAndFetch(&this->m_Object,
      static_cast<typename Impl::ValueType>(val)));
  }

  operator T() const
  {
    return static_cast<T>(Impl::Load(&this->m_Object));
  }

  T operator=(T val)
  {
    Impl::Store(&this->m_Object, static_cast<typename Impl::ValueType>(val));
    return val;
  }

  AtomicInt<T>& operator=(const AtomicInt<T> &ai)
  {
    this->store(ai.load());
    return *this;
  }

  T load() const
  {
    return static_cast<T>(Impl::Load(&this->m_Object));
  }

  void store(T val)
  {
    Impl::Store(&this->m_Object, static_cast<typename Impl::ValueType>(val));
  }

private:
  typename Impl::AtomicType m_Object;
};


template <typename T>
class AtomicInt<T*>
{
private:
  typedef Detail::AtomicOps<sizeof(T*)> Impl;

public:
  AtomicInt() : m_Object(0)
  {
  }

  AtomicInt(T* val)
    : m_Object(reinterpret_cast<typename Impl::ValueType>(val))
  {
  }

  AtomicInt(const AtomicInt<T*> &ai)
    : m_Object(reinterpret_cast<typename Impl::ValueType>(ai.load()))
  {
  }

  T* operator++()
  {
    return reinterpret_cast<T*>(Impl::AddAndFetch(&this->m_Object, sizeof(T)));
  }

  T* operator++(int)
  {
    T* val = reinterpret_cast<T*>(Impl::AddAndFetch(&this->m_Object, sizeof(T)));
    return --val;
  }

  T* operator--()
  {
    return reinterpret_cast<T*>(Impl::SubAndFetch(&this->m_Object, sizeof(T)));
  }

  T* operator--(int)
  {
    T* val = reinterpret_cast<T*>(Impl::AddAndFetch(&this->m_Object, sizeof(T)));
    return ++val;
  }

  T* operator+=(std::ptrdiff_t val)
  {
    return reinterpret_cast<T*>(Impl::AddAndFetch(&this->m_Object,
                                                  val * sizeof(T)));
  }

  T* operator-=(std::ptrdiff_t val)
  {
    return reinterpret_cast<T*>(Impl::SubAndFetch(&this->m_Object,
                                                  val * sizeof(T)));
  }

  operator T*() const
  {
    return reinterpret_cast<T*>(Impl::Load(&this->m_Object));
  }

  T* operator=(T* val)
  {
    Impl::Store(&this->m_Object,
                reinterpret_cast<typename Impl::ValueType>(val));
    return val;
  }

  AtomicInt<T*>& operator=(const AtomicInt<T*> &ai)
  {
    this->store(ai.load());
    return *this;
  }

  T* load() const
  {
    return reinterpret_cast<T*>(Impl::Load(&this->m_Object));
  }

  void store(T* val)
  {
    Impl::Store(&this->m_Object,
                reinterpret_cast<typename Impl::ValueType>(val));
  }

private:
  typename Impl::AtomicType m_Object;
};


template <> class AtomicInt<void*>
{
private:
  typedef Detail::AtomicOps<sizeof(void*)> Impl;

public:
  AtomicInt() : m_Object(0)
  {
  }

  AtomicInt(void* val)
    : m_Object(reinterpret_cast<Impl::ValueType>(val))
  {
  }

  AtomicInt(const AtomicInt<void*> &ai)
    : m_Object(reinterpret_cast<Impl::ValueType>(ai.load()))
  {
  }

  operator void*() const
  {
    return reinterpret_cast<void*>(Impl::Load(&this->m_Object));
  }

  void* operator=(void* val)
  {
    Impl::Store(&this->m_Object,
                reinterpret_cast<Impl::ValueType>(val));
    return val;
  }

  AtomicInt<void*>& operator=(const AtomicInt<void*> &ai)
  {
    this->store(ai.load());
    return *this;
  }

  void* load() const
  {
    return reinterpret_cast<void*>(Impl::Load(&this->m_Object));
  }

  void store(void* val)
  {
    Impl::Store(&this->m_Object,
                reinterpret_cast<Impl::ValueType>(val));
  }

private:
  Impl::AtomicType m_Object;
};

#else // C++11 ITK_COMPILER_CXX_ATOMIC == 1

template <typename T>
class AtomicInt
{
private:
  typedef Detail::AtomicOps<sizeof(T)> Impl;
  typedef typename Impl::ValueType     ValueType;
  itkConceptMacro( SupportedInteger, ( Detail::IsAtomicSupportedIntegralType<T> ) );

public:
  AtomicInt() : m_Object(0)
  {
  }

  AtomicInt(T val)
    : m_Object(static_cast<ValueType>(val))
  {
  }

  AtomicInt(const AtomicInt<T> &ai)
    : m_Object(static_cast<ValueType>(ai.load()))
  {
  }

  T operator++()
  {
    return static_cast<T>(++m_Object);
  }

  T operator++(int)
  {
    return static_cast<T>(m_Object++);
  }

  T operator--()
  {
    return static_cast<T>(--m_Object);
  }

  T operator--(int)
  {
    return static_cast<T>(m_Object--);
  }

  T operator+=(T val)
  {
    return static_cast<T>(m_Object+=static_cast<ValueType>(val));
  }

  T operator-=(T val)
  {
    return static_cast<T>(m_Object-=static_cast<ValueType>(val));
  }

  operator T() const
  {
    return static_cast<T>(m_Object.load());
  }

  T operator=(T val)
  {
    m_Object.store(static_cast<ValueType>(val));
    return val;
  }

  AtomicInt<T>& operator=(const AtomicInt<T> &ai)
  {
    this->store(ai.load());
    return *this;
  }

  T load() const
  {
    return static_cast<T>(m_Object.load());
  }

  void store(T val)
  {
    m_Object.store(static_cast<ValueType>(val));
  }

  private:
    std::atomic<typename Detail::AtomicOps<sizeof(T)>::ValueType > m_Object;
};


template <typename T>
class AtomicInt<T*>
{
private:
  typedef Detail::AtomicOps<sizeof(T*)> Impl;
  typedef typename Impl::ValueType      ValueType;

public:
  AtomicInt() : m_Object(0)
  {
  }

  AtomicInt(T* val)
    : m_Object(reinterpret_cast<ValueType>(val))
  {
  }

  AtomicInt(const AtomicInt<T*> &ai)
    : m_Object(reinterpret_cast<ValueType>(ai.load()))
  {
  }

  T* operator++()
  {
    return reinterpret_cast<T*>(m_Object.fetch_add(sizeof(T))+sizeof(T));
  }

  T* operator++(int)
  {
    return reinterpret_cast<T*>(m_Object.fetch_add(sizeof(T)));
  }

  T* operator--()
  {
    return reinterpret_cast<T*>(m_Object.fetch_sub(sizeof(T))-sizeof(T));
  }

  T* operator--(int)
  {
    return reinterpret_cast<T*>(m_Object.fetch_sub(sizeof(T)));
  }

  T* operator+=(std::ptrdiff_t val)
  {
    return reinterpret_cast<T*>(m_Object += val * sizeof(T));
  }

  T* operator-=(std::ptrdiff_t val)
  {
    return reinterpret_cast<T*>(m_Object -= val * sizeof(T));
  }

  operator T*() const
  {
    return reinterpret_cast<T*>(m_Object.load());
  }

  T* operator=(T* val)
  {
    m_Object.store(reinterpret_cast<ValueType>(val));
    return val;
  }

  AtomicInt<T*>& operator=(const AtomicInt<T*> &ai)
  {
    this->store(ai.load());
    return *this;
  }

  T* load() const
  {
    return reinterpret_cast<T*>(m_Object.load());
  }

  void store(T* val)
  {
    m_Object.store(reinterpret_cast<ValueType>(val));
  }

  private:
    std::atomic<typename Detail::AtomicOps<sizeof(T*)>::ValueType > m_Object;
};


template <> class AtomicInt<void*>
{
private:
  typedef Detail::AtomicOps<sizeof(void*)> Impl;
  typedef Impl::ValueType         ValueType;

public:
  AtomicInt() : m_Object(0)
  {
  }

  AtomicInt(void* val)
    : m_Object(reinterpret_cast<ValueType>(val))
  {
  }

  AtomicInt(const AtomicInt<void*> &ai)
    : m_Object(reinterpret_cast<ValueType>(ai.load()))
  {
  }

  operator void*() const
  {
    return reinterpret_cast<void*>(m_Object.load());
  }

  void* operator=(void* val)
  {
    m_Object.store(reinterpret_cast<ValueType>(val));
    return val;
  }

  AtomicInt<void*>& operator=(const AtomicInt<void*> &ai)
  {
    this->store(ai.load());
    return *this;
  }

  void* load() const
  {
    return reinterpret_cast<void*>(m_Object.load());
  }

  void store(void* val)
  {
    m_Object.store(reinterpret_cast<ValueType>(val));
  }

  private:
    std::atomic<typename Detail::AtomicOps<sizeof(void*)>::ValueType > m_Object;
};

#endif

} // end namespace itk

#endif

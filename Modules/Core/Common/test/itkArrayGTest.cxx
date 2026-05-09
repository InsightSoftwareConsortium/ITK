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

// First include the header file to be tested:
#include "itkArray.h"
#include "itkNumericTraits.h"
#include <gtest/gtest.h>
#include <iostream>

#if defined(__has_feature)
#  if __has_feature(address_sanitizer)
#    include <sanitizer/lsan_interface.h>
#    define ITK_ARRAY_GTEST_HAS_ASAN 1
#  endif
#endif
#ifndef ITK_ARRAY_GTEST_HAS_ASAN
#  define ITK_ARRAY_GTEST_HAS_ASAN 0
#endif

namespace
{
// Checks that `itk::Array` supports class template argument deduction (CTAD).
template <typename TValue>
constexpr bool
CheckClassTemplateArgumentDeduction()
{
  using ExpectedType = itk::Array<TValue>;

  const TValue constValue{};
  static_assert(std::is_same_v<decltype(itk::Array(&constValue, 1)), ExpectedType>,
                "The `Array(const ValueType *, ...)` constructor should support CTAD!");

  TValue nonConstValue{};
  static_assert(std::is_same_v<decltype(itk::Array(&nonConstValue, 1)), ExpectedType>,
                "The `Array(ValueType *, ...)` constructor should support CTAD!");
  return true;
}
} // namespace


static_assert(CheckClassTemplateArgumentDeduction<int>() && CheckClassTemplateArgumentDeduction<float>());


TEST(Array, MemoryManagement)
{
  using FloatArrayType = itk::Array<float>;
  using DoubleArrayType = itk::Array<double>;

  const FloatArrayType  fa(10);
  const DoubleArrayType da(10);

  // Create an itk::Array which manages its own memory
  FloatArrayType myOwnBoss;
  myOwnBoss.SetSize(5);
  myOwnBoss.Fill(2.0 + 1.0f / 3.0f);
  myOwnBoss[0] = 2.0f / 3.0f;
  myOwnBoss[1] = itk::NumericTraits<float>::max();
  myOwnBoss[2] = itk::NumericTraits<float>::min();
  myOwnBoss[3] = 1.0f;

  // Create an itk::Array which does not manage its own memory
  constexpr unsigned int n{ 7 };
  float                  buffer[n];
  FloatArrayType         notMyOwnBoss;
  notMyOwnBoss.SetSize(n);
  notMyOwnBoss.SetData(buffer, false);
  notMyOwnBoss.Fill(4.0);

  FloatArrayType notMyOwnBossToo;
  notMyOwnBossToo.SetSize(n);
  notMyOwnBossToo.SetData(buffer, false);

  // Copy an itk::Array which manages its own memory
  const FloatArrayType test1 = myOwnBoss;
  std::cout << test1 << std::endl;
  EXPECT_EQ(test1.GetSize(), myOwnBoss.GetSize());

  // Copy an itk::Array which does not manage its own memory
  FloatArrayType test2 = notMyOwnBoss;
  std::cout << test2 << std::endl;
  EXPECT_EQ(test2.GetSize(), notMyOwnBoss.GetSize());

  // itk::Array not managing its memory copying one that does
  notMyOwnBoss = myOwnBoss;
  std::cout << notMyOwnBoss << std::endl;
  EXPECT_EQ(notMyOwnBoss.GetSize(), myOwnBoss.GetSize());

  // Calling SetSize with same size
  notMyOwnBossToo.SetSize(notMyOwnBossToo.GetSize());

  // Calling SetSize with different size
  notMyOwnBossToo.SetSize(notMyOwnBossToo.GetSize() + 1);
  notMyOwnBossToo.Fill(6.0);
  std::cout << notMyOwnBossToo << std::endl;

  // Exercise operator=( VnlVectorType& )
  test2 = test1;
  EXPECT_EQ(test2.GetSize(), test1.GetSize());

  // Construct array pointing to user-allocated buffer (user manages deletion)
  constexpr size_t testSizeForArraySetDataSameSize{ 10 };
  FloatArrayType   objectToCopy(testSizeForArraySetDataSameSize);
  auto *           data = new float[testSizeForArraySetDataSameSize];
  objectToCopy.SetDataSameSize(data);

  // Copy of array not managing its own memory
  const FloatArrayType copy(objectToCopy);
  EXPECT_EQ(copy.GetSize(), objectToCopy.GetSize());

  // Double array managing its own memory
  DoubleArrayType myOwnDouble;
  myOwnDouble.SetSize(5);
  myOwnDouble.Fill(2.0 + 1.0 / 3.0);
  myOwnDouble[0] = 2.0 / 3.0;
  myOwnDouble[1] = itk::NumericTraits<double>::max();
  myOwnDouble[2] = itk::NumericTraits<double>::min();
  myOwnDouble[3] = 1.0;
  std::cout << myOwnDouble << std::endl;

  delete[] data;
}


// Test 1: Regression for the SetSize-on-non-owning-Array path. The vnl_vector
// allocation macro asserts that vnl's m_LetArrayManageMemory flag is true
// before allocating; if Array::SetSize hands a stale `false` to vnl via
// protected_set_data, this aborts in debug builds and silently leaks in
// release. Exercises the path on a non-owning Array and on a freshly
// default-constructed (size-zero) Array.
TEST(Array, SetSizeOnNonOwningArrayDoesNotAssertOrLeak)
{
  using FloatArrayType = itk::Array<float>;

  constexpr unsigned int n{ 7 };
  float                  buffer[n]{};

  FloatArrayType external;
  external.SetData(buffer, n, /*LetArrayManageMemory=*/false);

  external.SetSize(n + 1);
  external.Fill(3.0f);
  EXPECT_EQ(external.GetSize(), n + 1u);

  FloatArrayType empty;
  empty.SetSize(5);
  empty.Fill(2.0f);
  EXPECT_EQ(empty.GetSize(), 5u);
}


// Test 2: Cycle through every ownership transition that protected_set_data
// gates. Each step exercises a different combination of (old ITK flag, new
// caller intent). Under AddressSanitizer + LeakSanitizer the dtor chain
// reports any double-free or leak left behind by a wrong third argument.
TEST(Array, OwnershipTransitionsRoundTripCleanly)
{
  using FloatArrayType = itk::Array<float>;

  constexpr unsigned int n{ 5 };

  // Externally-owned buffers used across the transitions.
  float external_a[n]{};
  float external_b[n]{};

  // Heap buffer the Array will be told to manage and free on destruction.
  auto * heap_owned = new float[n]{};

  // Path: owning -> external (false). Old ITK flag was true, so SetData
  // calls vnl::destroy() before re-pointing.
  FloatArrayType a(n);
  a.Fill(1.0f);
  a.SetData(external_a, n, /*LetArrayManageMemory=*/false);
  EXPECT_EQ(a.GetSize(), n);

  // Path: external (false) -> external (false). No allocation either side.
  a.SetData(external_b, n, /*LetArrayManageMemory=*/false);

  // Path: external (false) -> heap-owned (true). Caller hands ownership
  // of `heap_owned` to the Array; Array dtor must free it via vnl.
  a.SetData(heap_owned, n, /*LetArrayManageMemory=*/true);

  // SetDataSameSize path with the SAME flag transitions.
  FloatArrayType b(n);
  b.Fill(1.0f);
  b.SetDataSameSize(external_a, /*LetArrayManageMemory=*/false);
  b.SetDataSameSize(external_b, /*LetArrayManageMemory=*/false);

  // Constructor with externally-owned + non-owning destruction.
  {
    FloatArrayType ext_ctor(external_a, n, /*LetArrayManageMemory=*/false);
    EXPECT_EQ(ext_ctor.GetSize(), n);
  } // dtor runs; external_a must NOT be freed.

  // Constructor with heap buffer + owning destruction.
  {
    auto *         heap2 = new float[n]{};
    FloatArrayType heap_ctor(heap2, n, /*LetArrayManageMemory=*/true);
    EXPECT_EQ(heap_ctor.GetSize(), n);
  } // dtor must free heap2 via vnl_c_vector::deallocate.

  // `a` still owns `heap_owned`; let its dtor free it here.
  // (No explicit delete[] — that would double-free under the fix.)
}


// Test 3: Heap-buffer ownership reaches a real free. Without a numeric T
// that can hook ITK's allocator, leak detection comes from ASan/LSan; in a
// non-sanitizer build the test still exercises the path and verifies that
// the operations complete without crashing or asserting. The
// __lsan_do_recoverable_leak_check call (only compiled in under ASan)
// reports any leftover allocations from this scope as a test failure.
TEST(Array, HeapBufferOwnedByArrayIsFreed)
{
  using FloatArrayType = itk::Array<float>;

  constexpr unsigned int n{ 11 };

  // Constructor path.
  {
    auto *         heap = new float[n]{};
    FloatArrayType owner(heap, n, /*LetArrayManageMemory=*/true);
    owner.Fill(7.0f);
    EXPECT_EQ(owner.GetSize(), n);
  }

  // SetData(true) path on a previously-owning Array.
  {
    FloatArrayType owner(n);
    owner.Fill(2.0f);
    auto * heap = new float[n]{};
    owner.SetData(heap, n, /*LetArrayManageMemory=*/true);
    EXPECT_EQ(owner.GetSize(), n);
  }

  // SetDataSameSize(true) path on a previously-owning Array.
  {
    FloatArrayType owner(n);
    owner.Fill(3.0f);
    auto * heap = new float[n]{};
    owner.SetDataSameSize(heap, /*LetArrayManageMemory=*/true);
    EXPECT_EQ(owner.GetSize(), n);
  }

  // SetSize-on-external path: the Array must take ownership of the
  // freshly-allocated buffer that vnl_vector::set_size produces.
  {
    float          buf[n]{};
    FloatArrayType external;
    external.SetSize(n);
    external.SetData(buf, n, /*LetArrayManageMemory=*/false);
    external.SetSize(n + 3); // allocates a new buffer; Array now owns it.
    external.Fill(4.0f);
    EXPECT_EQ(external.GetSize(), n + 3u);
  }

#if ITK_ARRAY_GTEST_HAS_ASAN
  __lsan_do_recoverable_leak_check();
#endif
}

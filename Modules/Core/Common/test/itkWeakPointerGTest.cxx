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

#include "itkGTest.h"

#include "itkWeakPointer.h"
#include "itkLightObject.h"

#include <type_traits> // For is_default_constructible, is_copy_constructible, etc.

namespace
{
using WeakPointerType = itk::WeakPointer<itk::LightObject>;

template <typename T>
constexpr bool
AllSpecialMemberFunctionsNoThrow()
{
  return std::is_nothrow_default_constructible<T>::value && std::is_nothrow_copy_constructible<T>::value &&
         std::is_nothrow_copy_assignable<T>::value && std::is_nothrow_move_constructible<T>::value &&
         std::is_nothrow_move_assignable<T>::value && std::is_nothrow_destructible<T>::value;
}

} // namespace

static_assert(AllSpecialMemberFunctionsNoThrow<WeakPointerType>(),
              "All special member functions should be non-throwing.");


TEST(WeakPointer, DefaultConstructedEqualsNullptr)
{
  ASSERT_EQ(WeakPointerType{}, nullptr);
  ASSERT_TRUE(WeakPointerType{} == nullptr);
  ASSERT_TRUE(nullptr == WeakPointerType{});
}


TEST(WeakPointer, CheckNull)
{
  WeakPointerType nullPtr;
  ASSERT_TRUE(nullPtr.IsNull());

  itk::LightObject::Pointer lightObject = itk::LightObject::New();
  WeakPointerType           ptr = lightObject.GetPointer();
  ASSERT_TRUE(ptr.IsNotNull());
}


TEST(WeakPointer, CheckSerialization)
{
  WeakPointerType nullPtr;
  std::cout << nullPtr << std::endl;

  itk::LightObject::Pointer lightObject = itk::LightObject::New();
  WeakPointerType           ptr = lightObject.GetPointer();
  std::cout << ptr << std::endl;
}


TEST(WeakPointer, ConvertedFromNullptrEqualsNullptr)
{
  ASSERT_EQ(WeakPointerType{ nullptr }, nullptr);
  ASSERT_TRUE(WeakPointerType{ nullptr } == nullptr);
  ASSERT_TRUE(nullptr == WeakPointerType{ nullptr });
}


TEST(WeakPointer, AssignedFromNullptrEqualsNullptr)
{
  WeakPointerType ptr;
  ptr = nullptr;
  ASSERT_EQ(ptr, nullptr);
  ASSERT_TRUE(ptr == nullptr);
  ASSERT_TRUE(nullptr == ptr);
}

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
#include "itkDeref.h"
#include <gtest/gtest.h>
#include "itkObject.h"


// Tests that `Deref(ptr)` throws a `DerefError` exception when its argument is null.
TEST(Deref, ThrowsExceptionWhenArgumentIsNull)
{
  const auto check = [](const auto & ptr) { EXPECT_THROW(itk::Deref(ptr), itk::DerefError); };

  check(static_cast<int *>(nullptr));
  check(static_cast<const int *>(nullptr));
  check(static_cast<itk::Object *>(nullptr));
}


// Tests that `Deref(ptr)` returns the same reference as `*ptr`, when `ptr` is not null.
TEST(Deref, ReturnsReferenceWhenArgumentIsNotNull)
{
  const auto check = [](const auto & ptr) {
    const auto & ref = itk::Deref(ptr);
    EXPECT_EQ(&ref, &*ptr);
  };

  constexpr int i{};
  check(&i);
  check(std::make_unique<int>().get());
  check(itk::Object::New().get());
}

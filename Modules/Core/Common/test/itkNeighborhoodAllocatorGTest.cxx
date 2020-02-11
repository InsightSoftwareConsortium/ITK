/*=========================================================================
 *
 *  Copyright NumFOCUS
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

// First include the header file to be tested:
#include "itkNeighborhoodAllocator.h"

#include <gtest/gtest.h>

#include <type_traits> // For is_pointer, is_const, and remove_reference.

namespace
{
template <typename T>
void
Expect_data_returns_pointer_to_first_element(T & container)
{
  static_assert(std::is_pointer<decltype(container.data())>::value, "data() must return a pointer");

  static_assert(std::is_const<typename std::remove_reference<decltype(*(container.data()))>::type>::value ==
                  std::is_const<typename std::remove_reference<decltype(container[0])>::type>::value,
                "*container.data() and container[0] must have the same const-ness");

  EXPECT_EQ(container.data(), &container[0]);
}


// Counts the number of instances of the class.
class ObjectCounter
{
private:
  static std::size_t m_Count;

public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ObjectCounter);

  ObjectCounter() ITK_NOEXCEPT { ++m_Count; }

  ~ObjectCounter() { --m_Count; }

  static std::size_t
  GetCount()
  {
    return m_Count;
  }
};

std::size_t ObjectCounter::m_Count{};


} // namespace


// Tests that for a non-empty NeighborhoodAllocator, data() returns a pointer to the first element.
TEST(NeighborhoodAllocator, DataReturnsPointerToFirstElement)
{
  itk::NeighborhoodAllocator<int> neighborhoodAllocator;
  neighborhoodAllocator.set_size(1);
  neighborhoodAllocator[0] = 0;
  Expect_data_returns_pointer_to_first_element(neighborhoodAllocator);

  const itk::NeighborhoodAllocator<int> constNeighborhoodAllocator = neighborhoodAllocator;
  Expect_data_returns_pointer_to_first_element(constNeighborhoodAllocator);
}


// Tests that neighborhoodAllocator.Allocate(i) sets the size to the specified number.
TEST(NeighborhoodAllocator, AllocateSetsSizeToSpecifiedNumber)
{
  for (unsigned int i{}; i <= 3; ++i)
  {
    itk::NeighborhoodAllocator<int> neighborhoodAllocator;

    neighborhoodAllocator.Allocate(i);

    EXPECT_EQ(neighborhoodAllocator.size(), i);
  }
}


// Tests that neighborhoodAllocator.Deallocate() sets the size to zero.
TEST(NeighborhoodAllocator, DeallocateSetsSizeToZero)
{
  for (unsigned int i{}; i <= 3; ++i)
  {
    itk::NeighborhoodAllocator<int> neighborhoodAllocator;

    neighborhoodAllocator.set_size(i);
    neighborhoodAllocator.Deallocate();
    EXPECT_EQ(neighborhoodAllocator.size(), 0);
  }
}


// Tests that neighborhoodAllocator.Allocate(i) constructs the specified number of objects.
TEST(NeighborhoodAllocator, AllocateConstructsTheSpecifiedNumberOfObjects)
{
  ASSERT_EQ(ObjectCounter::GetCount(), 0);

  for (unsigned int i{}; i <= 3; ++i)
  {
    itk::NeighborhoodAllocator<ObjectCounter> neighborhoodAllocator;

    neighborhoodAllocator.Allocate(i);

    EXPECT_EQ(ObjectCounter::GetCount(), i);
  }
}


// Tests that neighborhoodAllocator.Deallocate() destructs all objects that were there.
TEST(NeighborhoodAllocator, DeallocateDestructsAllObjects)
{
  ASSERT_EQ(ObjectCounter::GetCount(), 0);

  for (unsigned int i{}; i <= 3; ++i)
  {
    itk::NeighborhoodAllocator<ObjectCounter> neighborhoodAllocator;
    neighborhoodAllocator.set_size(i);
    ASSERT_EQ(ObjectCounter::GetCount(), i);

    neighborhoodAllocator.Deallocate();

    // Expect that there are zero objects left, after Deallocate():
    EXPECT_EQ(ObjectCounter::GetCount(), 0);
  }
}

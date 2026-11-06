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

#include "itkPathIterator.h"
#include "itkImage.h"
#include "itkPolyLineParametricPath.h"
#include "itkGTest.h"
#include <type_traits>

namespace
{
using ImageType = itk::Image<double, 2>;
using PathType = itk::PolyLineParametricPath<2>;

// HasSet tells whether `Set(pixel)` can be called on the specified iterator type.
template <typename TIterator, typename = void>
struct HasSet : std::false_type
{};

template <typename TIterator>
struct HasSet<TIterator,
              std::void_t<decltype(std::declval<TIterator &>().Set(std::declval<typename TIterator::PixelType>()))>>
  : std::true_type
{};

// HasAssignableValue tells whether a pixel can be assigned through the reference returned by `Value()`.
template <typename TIterator, typename = void>
struct HasAssignableValue : std::false_type
{};

template <typename TIterator>
struct HasAssignableValue<
  TIterator,
  std::void_t<decltype(std::declval<TIterator &>().Value() = std::declval<typename TIterator::PixelType>())>>
  : std::true_type
{};
} // namespace


// Checks that assignment copies the end-of-path state of the source iterator.
TEST(PathIterator, AssignmentCopiesEndState)
{
  const auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(4));
  image->AllocateInitialized();

  const auto           path = PathType::New();
  PathType::VertexType vertex;
  vertex.Fill(0.0);
  path->AddVertex(vertex);
  vertex[0] = 2.0;
  path->AddVertex(vertex);

  using IteratorType = itk::PathIterator<ImageType, PathType>;
  IteratorType atEnd(image, path);
  while (!atEnd.IsAtEnd())
  {
    ++atEnd;
  }

  IteratorType copy(image, path);
  ASSERT_FALSE(copy.IsAtEnd());
  copy = atEnd;
  EXPECT_TRUE(copy.IsAtEnd());
  EXPECT_EQ(copy.GetIndex(), atEnd.GetIndex());
}


// Checks that only the path iterator over a non-const image provides Set() and an assignable Value().
TEST(PathIterator, NoWriteAccessToConstImage)
{
  static_assert(!HasSet<itk::PathIterator<const ImageType, PathType>>::value);
  static_assert(HasAssignableValue<itk::PathIterator<ImageType, PathType>>::value);
  static_assert(!HasAssignableValue<itk::PathIterator<const ImageType, PathType>>::value);
  static_assert(!HasAssignableValue<itk::PathConstIterator<ImageType, PathType>>::value);
}

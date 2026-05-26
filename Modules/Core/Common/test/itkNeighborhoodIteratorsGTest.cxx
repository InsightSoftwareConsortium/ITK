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

// First include the header files to be tested:
#include "itkConstNeighborhoodIterator.h"
#include "itkConstNeighborhoodIteratorWithOnlyIndex.h"
#include "itkConstShapedNeighborhoodIterator.h"
#include "itkNeighborhoodIterator.h"
#include "itkShapedNeighborhoodIterator.h"

#include "itkImage.h"
#include <gtest/gtest.h>
#include <type_traits> // For is_same_v.


namespace
{
template <template <typename> typename TIteratorTemplate, typename TImage>
void
CheckIteratorSupportsClassTemplateArgumentDeduction()
{
  using SizeType = typename TImage::SizeType;
  using RegionType = typename TImage::RegionType;
  using PointerType = typename TImage::Pointer;
  static_assert(std::is_same_v<decltype(TIteratorTemplate(SizeType{}, PointerType{}, RegionType{})),
                               decltype(TIteratorTemplate<TImage>(SizeType{}, PointerType{}, RegionType{}))>);
}


template <template <typename> typename TIteratorTemplate, typename TImage>
void
CheckConstIteratorSupportsClassTemplateArgumentDeduction()
{
  using SizeType = typename TImage::SizeType;
  using RegionType = typename TImage::RegionType;
  using ConstPointerType = typename TImage::ConstPointer;
  static_assert(std::is_same_v<decltype(TIteratorTemplate(SizeType{}, ConstPointerType{}, RegionType{})),
                               decltype(TIteratorTemplate<TImage>(SizeType{}, ConstPointerType{}, RegionType{}))>);
}


template <template <typename> typename... TIteratorTemplate>
void
CheckIteratorsSupportClassTemplateArgumentDeduction()
{
  (CheckIteratorSupportsClassTemplateArgumentDeduction<TIteratorTemplate, itk::Image<int>>(), ...);
  (CheckIteratorSupportsClassTemplateArgumentDeduction<TIteratorTemplate, itk::Image<double, 3>>(), ...);
}


template <template <typename> typename... TIteratorTemplate>
void
CheckConstIteratorsSupportClassTemplateArgumentDeduction()
{
  (CheckConstIteratorSupportsClassTemplateArgumentDeduction<TIteratorTemplate, itk::Image<int>>(), ...);
  (CheckConstIteratorSupportsClassTemplateArgumentDeduction<TIteratorTemplate, itk::Image<double, 3>>(), ...);
}
} // namespace


// Checks that the iterator class templates support class template argument deduction (CTAD), when constructed by
// `IteratorTemplate(radius, ptr, region)`, with `ptr` being a `SmartPointer` to an image.
TEST(NeighborhoodIterators, SupportClassTemplateArgumentDeduction)
{
  CheckIteratorsSupportClassTemplateArgumentDeduction<itk::ConstNeighborhoodIterator,
                                                      itk::ConstNeighborhoodIteratorWithOnlyIndex,
                                                      itk::ConstShapedNeighborhoodIterator,
                                                      itk::NeighborhoodIterator,
                                                      itk::ShapedNeighborhoodIterator>();

  CheckConstIteratorsSupportClassTemplateArgumentDeduction<itk::ConstNeighborhoodIterator,
                                                           itk::ConstNeighborhoodIteratorWithOnlyIndex,
                                                           itk::ConstShapedNeighborhoodIterator>();
}

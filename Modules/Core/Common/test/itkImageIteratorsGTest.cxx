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
#include "itkImageIterator.h"
#include "itkImageIteratorWithIndex.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageRandomIteratorWithIndex.h"
#include "itkImageRandomNonRepeatingIteratorWithIndex.h"
#include "itkImageRegionExclusionIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionReverseIterator.h"
#include "itkImageReverseIterator.h"
#include "itkImageScanlineIterator.h"
#include "itkImageSliceIteratorWithIndex.h"

#include "itkImage.h"
#include "itkTestingGetBufferPointerCountingImage.h"
#include <gtest/gtest.h>
#include <type_traits> // For false_type and true_type.
#include <typeinfo>

namespace
{


template <typename TIterator>
void
ExpectConstructorCallsNonConstGetBufferPointer()
{
  EXPECT_TRUE(itk::Testing::IteratorConstructorCallsNonConstGetBufferPointer<TIterator>()) << typeid(TIterator).name();
}


template <template <typename> typename... TIteratorTemplate>
void
CheckIteratorsCallNonConstGetBufferPointer()
{
  (ExpectConstructorCallsNonConstGetBufferPointer<
     TIteratorTemplate<itk::Testing::GetBufferPointerCountingImage<int>>>(),
   ...);
}
} // namespace


// Checks that non-const iterators call the non-const GetBufferPointer() overload of the image on construction.
TEST(ImageIterators, ConstructorCallsNonConstGetBufferPointer)
{
  CheckIteratorsCallNonConstGetBufferPointer<itk::ImageIterator,
                                             itk::ImageIteratorWithIndex,
                                             itk::ImageLinearIteratorWithIndex,
                                             itk::ImageRandomIteratorWithIndex,
                                             itk::ImageRandomNonRepeatingIteratorWithIndex,
                                             itk::ImageRegionExclusionIteratorWithIndex,
                                             itk::ImageRegionIterator,
                                             itk::ImageRegionIteratorWithIndex,
                                             itk::ImageRegionReverseIterator,
                                             itk::ImageReverseIterator,
                                             itk::ImageScanlineIterator,
                                             itk::ImageSliceIteratorWithIndex>();
}


// Checks that ImageRegionReverseIterator(const ImageConstIterator &) calls the non-const GetBufferPointer() overload.
TEST(ImageIterators, ReverseIteratorConversionCallsNonConstGetBufferPointer)
{
  using ImageType = itk::Testing::GetBufferPointerCountingImage<int>;
  const auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(4));
  image->Allocate();
  const itk::ImageRegionConstIterator<ImageType> constIterator(image, image->GetBufferedRegion());

  const size_t numberOfCallsBefore = image->GetNumberOfNonConstGetBufferPointerCalls();
  [[maybe_unused]] const itk::ImageRegionReverseIterator<ImageType> reverseIterator(constIterator);
  EXPECT_GT(image->GetNumberOfNonConstGetBufferPointerCalls(), numberOfCallsBefore);
}

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
#include "itkTranslationTransform.h"

#include <gtest/gtest.h>

#include <algorithm> // For equal.
#include <numeric>   // For iota.


namespace
{

template <unsigned int VDimension>
void
Expect_SetParameters_throws_when_size_is_less_than_SpaceDimension()
{
  using TransformType = itk::TranslationTransform<double, VDimension>;
  const auto transform = TransformType::New();

  for (unsigned int size{}; size < TransformType::SpaceDimension; ++size)
  {
    const typename TransformType::ParametersType parameters(size, 0.0);
    EXPECT_THROW(transform->SetParameters(parameters), itk::ExceptionObject);
  }
}


template <unsigned int VDimension>
void
Expect_SetParameters_sets_translation_offset()
{
  using TransformType = itk::TranslationTransform<double, VDimension>;
  using ParametersType = typename TransformType::ParametersType;

  const auto transform = TransformType::New();

  // Check setting the offset to (0, 0, 0, ...).
  transform->SetParameters(ParametersType(VDimension, 0.0));
  EXPECT_EQ(transform->GetOffset(), typename TransformType::OutputVectorType());

  // Check setting the offset to (1, 2, 3, ...).
  ParametersType parameters(VDimension);
  std::iota(parameters.begin(), parameters.end(), 1.0);
  transform->SetParameters(parameters);
  EXPECT_TRUE(std::equal(parameters.begin(), parameters.end(), transform->GetOffset().begin()));
}

} // namespace


TEST(TranslationTransform, SetParametersThrowsWhenSizeIsLessThanSpaceDimension)
{
  Expect_SetParameters_throws_when_size_is_less_than_SpaceDimension<2>();
  Expect_SetParameters_throws_when_size_is_less_than_SpaceDimension<3>();
}


TEST(TranslationTransform, SetParametersSetsOffset)
{
  Expect_SetParameters_sets_translation_offset<2>();
  Expect_SetParameters_sets_translation_offset<3>();
}

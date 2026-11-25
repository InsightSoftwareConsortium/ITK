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
#include "itkChangeInformationImageFilter.h"
#include "itkImage.h"
#include <gtest/gtest.h>


// Checks the properties of a newly created ChangeInformationImageFilter.
TEST(ChangeInformationImageFilter, CheckNew)
{
  const auto check = [](const auto & newChangeInformationImageFilter) {
    ASSERT_NE(newChangeInformationImageFilter, nullptr);

    EXPECT_EQ(newChangeInformationImageFilter->GetReferenceImage(), nullptr);

    EXPECT_FALSE(newChangeInformationImageFilter->GetChangeSpacing());
    EXPECT_FALSE(newChangeInformationImageFilter->GetChangeOrigin());
    EXPECT_FALSE(newChangeInformationImageFilter->GetChangeDirection());
    EXPECT_FALSE(newChangeInformationImageFilter->GetChangeRegion());
    EXPECT_FALSE(newChangeInformationImageFilter->GetCenterImage());
    EXPECT_FALSE(newChangeInformationImageFilter->GetUseReferenceImage());

    EXPECT_TRUE(newChangeInformationImageFilter->GetOutputDirection().GetVnlMatrix().is_identity());

    const auto expectAllEqualTo = [](const auto & range, const auto expectedValue) {
      // Expects that all elements are equal to the expected value.
      for (const auto element : range)
      {
        EXPECT_EQ(element, expectedValue);
      }
    };

    expectAllEqualTo(newChangeInformationImageFilter->GetOutputSpacing(), 1.0);
    expectAllEqualTo(newChangeInformationImageFilter->GetOutputOrigin(), 0.0);
    expectAllEqualTo(newChangeInformationImageFilter->GetOutputOffset(), 0);
  };

  check(itk::ChangeInformationImageFilter<itk::Image<int, 2>>::New());
  check(itk::ChangeInformationImageFilter<itk::Image<float, 3>>::New());
}

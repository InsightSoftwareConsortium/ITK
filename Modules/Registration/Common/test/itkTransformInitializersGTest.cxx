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
#include "itkBSplineTransformInitializer.h"
#include "itkCenteredTransformInitializer.h"
#include "itkCenteredVersorTransformInitializer.h"

#include "itkBSplineTransform.h"
#include "itkEuler2DTransform.h"

#include <gtest/gtest.h>


// Checks an object created by BSplineTransformInitializer::New().
TEST(TransformInitializers, CheckNewBSplineTransformInitializer)
{
  using TransformType = itk::BSplineTransform<>;
  using ImageType = itk::Image<int, TransformType::SpaceDimension>;
  const auto transformInitializer = itk::BSplineTransformInitializer<TransformType, ImageType>::New();

  ASSERT_NE(transformInitializer, nullptr);

  EXPECT_EQ(transformInitializer->GetImage(), nullptr);
  EXPECT_EQ(transformInitializer->GetTransform(), nullptr);

  for (const auto sizeValue : transformInitializer->GetTransformDomainMeshSize())
  {
    EXPECT_EQ(sizeValue, 1);
  }
}


// Checks an object created by CenteredTransformInitializer::New().
TEST(TransformInitializers, CheckNewCenteredTransformInitializer)
{
  using TransformType = itk::Euler2DTransform<>;
  using ImageType = itk::Image<int, TransformType::SpaceDimension>;
  const auto transformInitializer = itk::CenteredTransformInitializer<TransformType, ImageType, ImageType>::New();

  ASSERT_NE(transformInitializer, nullptr);

  EXPECT_NE(transformInitializer->GetFixedCalculator(), nullptr);
  EXPECT_NE(transformInitializer->GetMovingCalculator(), nullptr);
}


// Checks an object created by CenteredVersorTransformInitializer::New().
TEST(TransformInitializers, CheckNewCenteredVersorTransformInitializer)
{
  using ImageType = itk::Image<int, 3>;
  const auto transformInitializer = itk::CenteredVersorTransformInitializer<ImageType, ImageType>::New();

  ASSERT_NE(transformInitializer, nullptr);

  EXPECT_NE(transformInitializer->GetFixedCalculator(), nullptr);
  EXPECT_NE(transformInitializer->GetMovingCalculator(), nullptr);
  EXPECT_FALSE(transformInitializer->GetComputeRotation());
}

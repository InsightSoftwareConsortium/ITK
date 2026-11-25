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
#include "itkMultiGradientOptimizerv4.h"
#include "itkGradientDescentOptimizerBasev4.h"

namespace
{
using OptimizerType = itk::MultiGradientOptimizerv4Template<double>;
using BaseType = itk::GradientDescentOptimizerBasev4Template<double>;
} // namespace

TEST(MultiGradientOptimizerv4, DefaultStopConditionIsMaximumIterations)
{
  auto optimizer = OptimizerType::New();
  EXPECT_EQ(optimizer->GetStopCondition(), itk::StopConditionObjectToObjectOptimizerEnum::MAXIMUM_NUMBER_OF_ITERATIONS);
}

TEST(MultiGradientOptimizerv4, DefaultStopConditionDescriptionIsNonEmpty)
{
  auto optimizer = OptimizerType::New();
  EXPECT_FALSE(optimizer->GetStopConditionDescription().empty());
}

TEST(MultiGradientOptimizerv4, DefaultStopConditionViaBasePointerMatchesSubclassDispatch)
{
  auto       optimizer = OptimizerType::New();
  BaseType * basePtr = optimizer;
  EXPECT_EQ(basePtr->GetStopCondition(), itk::StopConditionObjectToObjectOptimizerEnum::MAXIMUM_NUMBER_OF_ITERATIONS);
}

TEST(MultiGradientOptimizerv4, StopOptimizationDoesNotChangeMTime)
{
  auto       optimizer = OptimizerType::New();
  const auto mtimeBefore = optimizer->GetMTime();
  optimizer->StopOptimization();
  EXPECT_EQ(optimizer->GetMTime(), mtimeBefore);
}

TEST(MultiGradientOptimizerv4, EmptyOptimizersListBeforeAssignment)
{
  auto optimizer = OptimizerType::New();
  EXPECT_EQ(optimizer->GetOptimizersList().size(), 0u);
}

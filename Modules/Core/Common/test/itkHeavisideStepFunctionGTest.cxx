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

#include "itkHeavisideStepFunction.h"
#include "itkSinRegularizedHeavisideStepFunction.h"
#include "itkAtanRegularizedHeavisideStepFunction.h"
#include "itkMath.h"
#include "itkGTest.h"

#include <iostream>

// From itkHeavisideStepFunctionTest1.cxx
TEST(HeavisideStepFunction, ExactHeaviside)
{
  using InputType = double;
  using OutputType = double;

  using HeavisideFunctionBaseType = itk::HeavisideStepFunction<InputType, OutputType>;

  auto functionBase0 = HeavisideFunctionBaseType::New();

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(functionBase0, HeavisideStepFunction, HeavisideStepFunctionBase);

  std::cout << "GetNameOfClass() = " << functionBase0->GetNameOfClass() << std::endl;
  functionBase0->Print(std::cout);

  constexpr int minValue{ -20 };
  constexpr int maxValue{ 20 };

  constexpr InputType incValue{ 0.1 };

  for (int x = minValue; x < maxValue; ++x)
  {
    const InputType  ix = x * incValue;
    const OutputType f = functionBase0->Evaluate(ix);
    const OutputType df = functionBase0->EvaluateDerivative(ix);
    std::cout << ix << ' ' << f << ' ' << df << std::endl;
    if (ix < 0.0)
    {
      EXPECT_EQ(f, 0.0);
      EXPECT_EQ(df, 0.0);
    }
    else
    {
      EXPECT_EQ(f, 1.0);
    }
  }
}

// From itkSinRegularizedHeavisideStepFunctionTest1.cxx
TEST(HeavisideStepFunction, SinRegularized)
{
  using InputType = double;
  using OutputType = double;

  using HeavisideFunctionBaseType = itk::SinRegularizedHeavisideStepFunction<InputType, OutputType>;

  auto functionBase0 = HeavisideFunctionBaseType::New();

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(
    functionBase0, SinRegularizedHeavisideStepFunction, HeavisideStepFunctionBase);

  std::cout << "GetNameOfClass() = " << functionBase0->GetNameOfClass() << std::endl;
  functionBase0->Print(std::cout);

  constexpr double epsilon{ -1.0 };
  EXPECT_THROW(functionBase0->SetEpsilon(epsilon), itk::ExceptionObject);

  constexpr double epsilon0{ 1.0 };
  constexpr double epsilon1{ 1e-4 };

  EXPECT_EQ(functionBase0->GetEpsilon(), epsilon0);

  functionBase0->SetEpsilon(epsilon1);
  EXPECT_EQ(functionBase0->GetEpsilon(), epsilon1);

  constexpr double epsilon2{ 0.5 };
  functionBase0->SetEpsilon(epsilon2);

  constexpr int minValue{ -20 };
  constexpr int maxValue{ 20 };

  constexpr InputType incValue{ 0.1 };

  for (int x = minValue; x < maxValue; ++x)
  {
    const InputType  ix = x * incValue;
    const OutputType f = functionBase0->Evaluate(ix);
    const OutputType df = functionBase0->EvaluateDerivative(ix);
    std::cout << ix << ' ' << f << ' ' << df << std::endl;
  }
  // At x=0 with epsilon=0.5: Evaluate = 0.5, EvaluateDerivative = pi/2
  EXPECT_NEAR(functionBase0->Evaluate(0.0), 0.5, 1e-10);
  EXPECT_NEAR(functionBase0->EvaluateDerivative(0.0), itk::Math::pi / 2.0, 1e-4);
}

// From itkAtanRegularizedHeavisideStepFunctionTest1.cxx
TEST(HeavisideStepFunction, AtanRegularized)
{
  using InputType = double;
  using OutputType = double;

  using HeavisideFunctionBaseType = itk::AtanRegularizedHeavisideStepFunction<InputType, OutputType>;

  auto functionBase0 = HeavisideFunctionBaseType::New();

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(
    functionBase0, AtanRegularizedHeavisideStepFunction, HeavisideStepFunctionBase);

  std::cout << "GetNameOfClass() = " << functionBase0->GetNameOfClass() << std::endl;
  functionBase0->Print(std::cout);

  constexpr double epsilon{ -1.0 };
  EXPECT_THROW(functionBase0->SetEpsilon(epsilon), itk::ExceptionObject);

  constexpr double epsilon0{ 1.0 };
  constexpr double epsilon1{ 1e-4 };

  EXPECT_EQ(functionBase0->GetEpsilon(), epsilon0);

  functionBase0->SetEpsilon(epsilon1);
  EXPECT_EQ(functionBase0->GetEpsilon(), epsilon1);

  constexpr double epsilon2{ 0.5 };
  functionBase0->SetEpsilon(epsilon2);

  constexpr int minValue{ -20 };
  constexpr int maxValue{ 20 };

  constexpr InputType incValue{ 0.1 };

  for (int x = minValue; x < maxValue; ++x)
  {
    const InputType  ix = x * incValue;
    const OutputType f = functionBase0->Evaluate(ix);
    const OutputType df = functionBase0->EvaluateDerivative(ix);
    std::cout << ix << ' ' << f << ' ' << df << std::endl;
  }
  // At x=0 with epsilon=0.5: Evaluate = 0.5, EvaluateDerivative = 2/pi ≈ 0.63662
  EXPECT_NEAR(functionBase0->Evaluate(0.0), 0.5, 1e-10);
  EXPECT_NEAR(functionBase0->EvaluateDerivative(0.0), 2.0 / itk::Math::pi, 1e-4);
}

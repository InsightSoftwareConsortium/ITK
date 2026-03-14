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

#include "itkGaborKernelFunction.h"
#include "itkMath.h"
#include "itkGTest.h"

TEST(GaborKernelFunction, BasicObjectMethods)
{
  using KernelFunctionType = itk::GaborKernelFunction<double>;
  auto gabor = KernelFunctionType::New();

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(gabor, GaborKernelFunction, KernelFunctionBase);
}

TEST(GaborKernelFunction, SetGetParameters)
{
  using KernelFunctionType = itk::GaborKernelFunction<double>;
  auto gabor = KernelFunctionType::New();

  constexpr double sigma{ 1.5 };
  gabor->SetSigma(sigma);
  EXPECT_DOUBLE_EQ(gabor->GetSigma(), sigma);

  constexpr double frequency{ 2.0 };
  gabor->SetFrequency(frequency);
  EXPECT_DOUBLE_EQ(gabor->GetFrequency(), frequency);

  constexpr double phaseOffset{ 0.8 };
  gabor->SetPhaseOffset(phaseOffset);
  EXPECT_DOUBLE_EQ(gabor->GetPhaseOffset(), phaseOffset);

  gabor->SetCalculateImaginaryPart(true);
  EXPECT_TRUE(gabor->GetCalculateImaginaryPart());

  gabor->CalculateImaginaryPartOn();
  EXPECT_TRUE(gabor->GetCalculateImaginaryPart());

  gabor->CalculateImaginaryPartOff();
  EXPECT_FALSE(gabor->GetCalculateImaginaryPart());
}

TEST(GaborKernelFunction, EvaluateImaginaryPart)
{
  using KernelFunctionType = itk::GaborKernelFunction<double>;
  auto gabor = KernelFunctionType::New();

  gabor->SetSigma(1.5);
  gabor->SetFrequency(2.0);
  gabor->SetPhaseOffset(0.8);
  gabor->CalculateImaginaryPartOn();

  constexpr double tolerance{ 1e-12 };
  constexpr double point{ 2.86 };
  constexpr double expectedValue{ -0.13297125073713259 };

  EXPECT_NEAR(gabor->Evaluate(point), expectedValue, tolerance);
}

TEST(GaborKernelFunction, EvaluateRealPart)
{
  using KernelFunctionType = itk::GaborKernelFunction<double>;
  auto gabor = KernelFunctionType::New();

  gabor->SetSigma(1.5);
  gabor->SetFrequency(2.0);
  gabor->SetPhaseOffset(0.8);
  gabor->CalculateImaginaryPartOff();

  constexpr double tolerance{ 1e-12 };
  constexpr double point{ 2.86 };
  constexpr double expectedValue{ 0.093234196962237226 };

  EXPECT_NEAR(gabor->Evaluate(point), expectedValue, tolerance);
}

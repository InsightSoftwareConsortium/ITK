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
#include "itkCompensatedSummation.h"
#include "itkGTest.h"

#include "itkMath.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"

TEST(CompensatedSummation, ConvertedLegacyTest)
{
  using FloatType = float;
  constexpr long seedValue{ 17 };

  constexpr FloatType expectedMean{ 0.5 };

  constexpr itk::SizeValueType accumSize{ 50000000 };

  using GeneratorType = itk::Statistics::MersenneTwisterRandomVariateGenerator;
  auto generator = GeneratorType::New();
  generator->SetSeed(seedValue);

  FloatType vanillaSum = 0.0;
  using CompensatedSummationType = itk::CompensatedSummation<FloatType>;
  CompensatedSummationType floatAccumulator;
  FloatType                randomNumber = NAN;
  for (itk::SizeValueType ii = 0; ii < accumSize; ++ii)
  {
    randomNumber = generator->GetVariate();
    vanillaSum += randomNumber;
    floatAccumulator.AddElement(randomNumber);
  }
  const FloatType vanillaMean = vanillaSum / static_cast<FloatType>(accumSize);
  const FloatType vanillaError = itk::Math::Absolute(vanillaMean - expectedMean);
  const FloatType accumulatorSum = floatAccumulator.GetSum();
  const FloatType accumulatorMean = accumulatorSum / static_cast<FloatType>(accumSize);
  const FloatType accumulatorError = itk::Math::Absolute(accumulatorMean - expectedMean);

  EXPECT_LT(accumulatorError, vanillaError);
  EXPECT_LE(accumulatorError, 1.0e-4);

  // exercise other methods
  const CompensatedSummationType floatAccumulatorCopy = floatAccumulator;
  EXPECT_EQ(floatAccumulatorCopy.GetSum(), floatAccumulator.GetSum());

  const CompensatedSummationType floatAccumulatorCopy2 = floatAccumulator;
  EXPECT_EQ(floatAccumulatorCopy2.GetSum(), floatAccumulator.GetSum());

  floatAccumulator += randomNumber;
  floatAccumulator -= randomNumber;
  EXPECT_FLOAT_EQ(floatAccumulatorCopy2.GetSum(), floatAccumulator.GetSum());

  floatAccumulator *= randomNumber;
  floatAccumulator /= randomNumber;
  EXPECT_FLOAT_EQ(floatAccumulatorCopy2.GetSum(), floatAccumulator.GetSum());

  floatAccumulator.ResetToZero();
  EXPECT_FLOAT_EQ(floatAccumulator.GetSum(), FloatType{});

  floatAccumulator = 2.0;
  EXPECT_EQ(floatAccumulator.GetSum(), 2.0);
}

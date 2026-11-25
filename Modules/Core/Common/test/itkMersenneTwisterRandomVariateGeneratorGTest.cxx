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

#ifndef ITK_FUTURE_LEGACY_REMOVE
#  define ITK_LEGACY_SILENT
#endif

// First include the header file to be tested:
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "itkGTest.h"
#include "itkMath.h"
#include <random> // For mt19937.

// The class to be tested.
using itk::Statistics::MersenneTwisterRandomVariateGenerator;


// Check that the default seed is not just zero!
static_assert(MersenneTwisterRandomVariateGenerator::DefaultSeed != 0);


// Tests that DefaultSeed is the seed of a default-constructed generator.
TEST(MersenneTwisterRandomVariateGenerator, DefaultSeed)
{
  // Derived generator class whose `New()` simply calls the default-constructor.
  class DerivedGenerator : public MersenneTwisterRandomVariateGenerator
  {
  public:
    ITK_DISALLOW_COPY_AND_MOVE(DerivedGenerator);
    itkSimpleNewMacro(DerivedGenerator);

  protected:
    DerivedGenerator() = default;
    ~DerivedGenerator() override = default;
  };

  EXPECT_EQ(DerivedGenerator::New()->GetSeed(), MersenneTwisterRandomVariateGenerator::DefaultSeed);
}


// Tests that GetIntegerVariate() conforms with the C++11 requirement for std::mt19937,
// when the ITK generator uses the default seed of std::mt19937:
// "The 10000th consecutive invocation of a default-constructed object of type mt19937
// shall produce the value 4123659995."
// (C++11 section "Engines and engine adaptors with predefined parameters", [rand.predef])
TEST(MersenneTwisterRandomVariateGenerator, GetIntegerVariateConformsWithStdMt19937Requirement)
{
  const auto generator = MersenneTwisterRandomVariateGenerator::New();
  generator->SetSeed(std::mt19937::default_seed);

  for (int i = 1; i < 10000; ++i)
  {
    generator->GetIntegerVariate();
  }

  // Call GetIntegerVariate() for the 10000th time:
  const auto actualValue = generator->GetIntegerVariate();

  // The value required for std::mt19937 (C++11):
  constexpr auto requiredValue = 4123659995UL;
  ASSERT_EQ(actualValue, requiredValue);
}


// Tests that repeated calls to GetIntegerVariate() yield the same values
// as std::mt19937, when std::mt19937 uses the same seed as the ITK generator.
TEST(MersenneTwisterRandomVariateGenerator, GetIntegerVariateReturnsSameAsStdMt19937)
{
  const auto   generator = MersenneTwisterRandomVariateGenerator::New();
  std::mt19937 stdMt19937{ generator->GetSeed() };

  // Just repeat a few times, assuming that that should be enough.
  for (int i = 0; i < 10; ++i)
  {
    EXPECT_EQ(generator->GetIntegerVariate(), stdMt19937());
  }
}


// Tests that two GetNextSeed() calls return the very same seed value, when ResetNextSeed() is called before each of
// those calls.
TEST(MersenneTwisterRandomVariateGenerator, ResetNextSeed)
{
  // Call GetInstance() beforehand, to make sure the global instance is there already when calling GetNextSeed().
  [[maybe_unused]] const auto globalGenerator = MersenneTwisterRandomVariateGenerator::GetInstance();

  MersenneTwisterRandomVariateGenerator::ResetNextSeed();
  const auto seed = MersenneTwisterRandomVariateGenerator::GetNextSeed();

  MersenneTwisterRandomVariateGenerator::ResetNextSeed();
  EXPECT_EQ(MersenneTwisterRandomVariateGenerator::GetNextSeed(), seed);
}


TEST(MersenneTwisterRandomVariateGenerator, BasicObjectMethods)
{
  auto twister = MersenneTwisterRandomVariateGenerator::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(twister, MersenneTwisterRandomVariateGenerator, RandomVariateGeneratorBase);
}


TEST(MersenneTwisterRandomVariateGenerator, SingletonSeedAndKnownSequence)
{
  using Twister = MersenneTwisterRandomVariateGenerator;
  constexpr Twister::IntegerType seed{ 1234 };

  Twister::GetInstance()->SetSeed(seed);
  EXPECT_EQ(Twister::GetInstance()->GetSeed(), seed);

  auto twister = Twister::New();

  // New instance gets seed = singleton's seed + 1
  EXPECT_EQ(Twister::GetInstance()->GetSeed() + 1, twister->GetSeed());

  // Sync seeds and verify same sequence
  twister->SetSeed(Twister::GetInstance()->GetSeed());
  for (int i = 0; i < 200; ++i)
  {
    EXPECT_EQ(Twister::GetInstance()->GetIntegerVariate(), twister->GetIntegerVariate());
  }

  // Check known sequence of values
  constexpr Twister::IntegerType expected[5]{ static_cast<Twister::IntegerType>(3294740812u),
                                              static_cast<Twister::IntegerType>(4175194053u),
                                              static_cast<Twister::IntegerType>(3041332341u),
                                              static_cast<Twister::IntegerType>(199851601u),
                                              static_cast<Twister::IntegerType>(3422518480u) };
  for (const auto i : expected)
  {
    EXPECT_EQ(twister->GetIntegerVariate(), i);
  }
}


TEST(MersenneTwisterRandomVariateGenerator, NormalVariateStatistics)
{
  auto twister = MersenneTwisterRandomVariateGenerator::New();

  double        sum = 0.0;
  double        sum2 = 0.0;
  constexpr int count{ 500000 };
  for (int i = 0; i < count; ++i)
  {
    const double v = twister->GetNormalVariate();
    sum += v;
    sum2 += v * v;
  }
  const double mean = sum / static_cast<double>(count);
  const double variance = sum2 / static_cast<double>(count) - mean * mean;
  EXPECT_NEAR(mean, 0.0, 0.01);
  EXPECT_NEAR(variance, 1.0, 0.01);
}


#ifndef ITK_FUTURE_LEGACY_REMOVE
TEST(MersenneTwisterRandomVariateGenerator, LegacyInitializeEqualSetSeed)
{
  auto twister = MersenneTwisterRandomVariateGenerator::New();

  twister->Initialize();
  twister->SetSeed(1234);
  const MersenneTwisterRandomVariateGenerator::IntegerType withSetSeed = twister->GetIntegerVariate();
  twister->Initialize(1234);
  const MersenneTwisterRandomVariateGenerator::IntegerType withInitialize = twister->GetIntegerVariate();
  EXPECT_EQ(withSetSeed, withInitialize);
}
#endif

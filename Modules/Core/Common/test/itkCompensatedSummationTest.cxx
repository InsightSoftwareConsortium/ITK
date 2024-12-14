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
#include "itkStdStreamStateSave.h"

#include "itkMath.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"

int
itkCompensatedSummationTest(int, char *[])
{
  // Save the format stream variables for std::cout
  // They will be restored when coutState goes out of scope
  // scope.
  const itk::StdStreamStateSave coutState(std::cout);

  using FloatType = float;
  const long seedValue = 17;

  constexpr FloatType expectedMean = 0.5;

  constexpr itk::SizeValueType accumSize = 50000000;

  using GeneratorType = itk::Statistics::MersenneTwisterRandomVariateGenerator;
  auto generator = GeneratorType::New();
  generator->Initialize(seedValue);

  FloatType vanillaSum = 0.0;
  using CompensatedSummationType = itk::CompensatedSummation<FloatType>;
  CompensatedSummationType floatAccumulator;
  FloatType                randomNumber;
  for (itk::SizeValueType ii = 0; ii < accumSize; ++ii)
  {
    randomNumber = generator->GetVariate();
    vanillaSum += randomNumber;
    floatAccumulator.AddElement(randomNumber);
  }
  const FloatType vanillaMean = vanillaSum / static_cast<FloatType>(accumSize);
  const FloatType vanillaError = itk::Math::abs(vanillaMean - expectedMean);
  const FloatType accumulatorSum = floatAccumulator.GetSum();
  const FloatType accumulatorMean = accumulatorSum / static_cast<FloatType>(accumSize);
  const FloatType accumulatorError = itk::Math::abs(accumulatorMean - expectedMean);

  std::cout << "The expected mean is:     " << expectedMean << '\n';

  std::cout << "The vanilla sum is:       " << vanillaSum << '\n';
  std::cout << "The vanilla mean is:      " << vanillaMean << '\n';
  std::cout << "The vanilla error is:     " << vanillaError << '\n';

  std::cout << "The accumulator sum is:   " << accumulatorSum << '\n';
  std::cout << "The accumulator mean is:  " << accumulatorMean << '\n';
  std::cout << "The accumulator error is: " << accumulatorError << '\n';

  if (vanillaError <= accumulatorError || accumulatorError > 1.0e-4)
  {
    std::cerr << "The compensated summation did not compensate well (crazy compiler flags?)." << '\n';
    return EXIT_FAILURE;
  }

  // exercise other methods
  const CompensatedSummationType floatAccumulatorCopy = floatAccumulator;
  if (itk::Math::NotExactlyEquals(floatAccumulatorCopy.GetSum(), floatAccumulator.GetSum()))
  {
    std::cerr << "The copy constructor failed." << '\n';
    return EXIT_FAILURE;
  }

  CompensatedSummationType floatAccumulatorCopy2;
  floatAccumulatorCopy2 = floatAccumulator;
  if (itk::Math::NotExactlyEquals(floatAccumulatorCopy2.GetSum(), floatAccumulator.GetSum()))
  {
    std::cerr << "The assignment operator failed." << '\n';
    return EXIT_FAILURE;
  }

  floatAccumulator += randomNumber;
  floatAccumulator -= randomNumber;
  if (itk::Math::NotAlmostEquals(floatAccumulatorCopy2.GetSum(), floatAccumulator.GetSum()))
  {
    std::cerr << "The operator+= and operator-= are not reversible." << '\n';
    return EXIT_FAILURE;
  }

  floatAccumulator *= randomNumber;
  floatAccumulator /= randomNumber;
  if (itk::Math::NotAlmostEquals(floatAccumulatorCopy2.GetSum(), floatAccumulator.GetSum()))
  {
    std::cerr << "The operator*= and operator/= are not reversible." << '\n';
    return EXIT_FAILURE;
  }

  floatAccumulator.ResetToZero();
  if (itk::Math::NotAlmostEquals(floatAccumulator.GetSum(), FloatType{}))
  {
    std::cerr << "GetSize() did return the correct value!" << '\n';
    return EXIT_FAILURE;
  }

  floatAccumulator = 2.0;
  if (floatAccumulator.GetSum() != 2.0)
  {
    std::cerr << "operator= did not set the value." << '\n';
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}

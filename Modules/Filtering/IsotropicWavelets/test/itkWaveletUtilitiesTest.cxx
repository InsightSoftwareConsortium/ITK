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

#include "itkWaveletUtilities.h"
#include "itkTestingMacros.h"

bool
IndexToLevelBandTest(const unsigned int & linearIndex,
                     const unsigned int & levels,
                     const unsigned int & bands,
                     const unsigned int & expectedLevel,
                     const unsigned int & expectedBand)
{
  using namespace itk::utils;
  IndexPairType pairLvBand = IndexToLevelBandSteerablePyramid(linearIndex, levels, bands);
  unsigned int  lv = pairLvBand.first;
  unsigned int  b = pairLvBand.second;
  if (lv != expectedLevel || b != expectedBand)
  {
    std::cerr << "Error for linearIndex: " << linearIndex << "\n"
              << "result: (lv:" << lv << ", b:" << b << ")\n"
              << "should be: (" << expectedLevel << ", " << expectedBand << ")." << std::endl;
    return false;
  }
  else
  {
    return true;
  }
}

void
printComputeMaxNumberOfLevelsError(itk::Size<3> inputSize,
                                   unsigned int scaleFactor,
                                   unsigned int expected,
                                   unsigned int result)
{
  std::cerr << "Error in ComputeMaxNumberOfLevels with" << std::endl;
  std::cerr << "scaleFactor = " << scaleFactor << std::endl;
  std::cerr << "inputSize = " << inputSize << std::endl;
  std::cerr << "Expected: " << expected << ", but got " << result << std::endl;
}

bool
testComputeMaxNumberOfLevels()
{
  bool                   testPassed = true;
  constexpr unsigned int Dimension = 3;

  unsigned int scaleFactor = 2;
  using SizeType = itk::Size<Dimension>;
  SizeType inputSize;
  inputSize.Fill(12);
  unsigned int expected = 3;

  unsigned int result = itk::utils::ComputeMaxNumberOfLevels(inputSize, scaleFactor);
  if (result != expected)
  {
    printComputeMaxNumberOfLevelsError(inputSize, scaleFactor, expected, result);
    testPassed = false;
  }

  inputSize.Fill(16);
  expected = 4;
  result = itk::utils::ComputeMaxNumberOfLevels(inputSize, scaleFactor);

  if (result != expected)
  {
    printComputeMaxNumberOfLevelsError(inputSize, scaleFactor, expected, result);
    testPassed = false;
  }

  inputSize.Fill(17);
  expected = 1;
  result = itk::utils::ComputeMaxNumberOfLevels(inputSize, scaleFactor);

  if (result != expected)
  {
    printComputeMaxNumberOfLevelsError(inputSize, scaleFactor, expected, result);
    testPassed = false;
  }

  inputSize.Fill(22);
  expected = 2;
  result = itk::utils::ComputeMaxNumberOfLevels(inputSize, scaleFactor);

  if (result != expected)
  {
    printComputeMaxNumberOfLevelsError(inputSize, scaleFactor, expected, result);
    testPassed = false;
  }

  inputSize.Fill(3);
  expected = 1;
  result = itk::utils::ComputeMaxNumberOfLevels(inputSize, scaleFactor);

  if (result != expected)
  {
    printComputeMaxNumberOfLevelsError(inputSize, scaleFactor, expected, result);
    testPassed = false;
  }

  // Change scaleFactor
  scaleFactor = 3;
  inputSize.Fill(27);
  expected = 3;
  result = itk::utils::ComputeMaxNumberOfLevels(inputSize, scaleFactor);

  if (result != expected)
  {
    printComputeMaxNumberOfLevelsError(inputSize, scaleFactor, expected, result);
    testPassed = false;
  }

  scaleFactor = 4;
  inputSize.Fill(16);
  expected = 2;
  result = itk::utils::ComputeMaxNumberOfLevels(inputSize, scaleFactor);

  if (result != expected)
  {
    printComputeMaxNumberOfLevelsError(inputSize, scaleFactor, expected, result);
    testPassed = false;
  }

  // With odd values
  scaleFactor = 2;
  inputSize.Fill(111);
  expected = 1;
  result = itk::utils::ComputeMaxNumberOfLevels(inputSize, scaleFactor);

  scaleFactor = 2;
  inputSize.Fill(112);
  // four division by the scale factor resulting in an integer.
  // 112 / 2 / 2 / 2 / 2 = 7
  expected = 5;
  result = itk::utils::ComputeMaxNumberOfLevels(inputSize, scaleFactor);

  if (result != expected)
  {
    printComputeMaxNumberOfLevelsError(inputSize, scaleFactor, expected, result);
    testPassed = false;
  }

  return testPassed;
}


int
itkWaveletUtilitiesTest(int, char *[])
{
  bool testPassed = true;

  // Test IndexToLevelBand
  bool testOutputIndexToLevelBandPassed = true;
  {
    unsigned int linearIndex = 0;
    unsigned int levels = 1;
    unsigned int bands = 1;
    unsigned int expectedLevel = 0;
    unsigned int expectedBand = 1;
    testOutputIndexToLevelBandPassed =
      testOutputIndexToLevelBandPassed && IndexToLevelBandTest(linearIndex, levels, bands, expectedLevel, expectedBand);
  }
  {
    unsigned int linearIndex = 1;
    unsigned int levels = 1;
    unsigned int bands = 1;
    unsigned int expectedLevel = levels;
    unsigned int expectedBand = 0;
    testOutputIndexToLevelBandPassed =
      testOutputIndexToLevelBandPassed && IndexToLevelBandTest(linearIndex, levels, bands, expectedLevel, expectedBand);
  }
  {
    unsigned int linearIndex = 4;
    unsigned int levels = 1;
    unsigned int bands = 1;
    ITK_TRY_EXPECT_EXCEPTION(itk::utils::IndexToLevelBandSteerablePyramid(linearIndex, levels, bands));
  }
  {
    unsigned int linearIndex = 0;
    unsigned int levels = 2;
    unsigned int bands = 2;
    unsigned int expectedLevel = 0;
    unsigned int expectedBand = 1;
    testOutputIndexToLevelBandPassed =
      testOutputIndexToLevelBandPassed && IndexToLevelBandTest(linearIndex, levels, bands, expectedLevel, expectedBand);
  }
  {
    unsigned int linearIndex = 1;
    unsigned int levels = 2;
    unsigned int bands = 2;
    unsigned int expectedLevel = 0;
    unsigned int expectedBand = 2;
    testOutputIndexToLevelBandPassed =
      testOutputIndexToLevelBandPassed && IndexToLevelBandTest(linearIndex, levels, bands, expectedLevel, expectedBand);
  }
  {
    unsigned int linearIndex = 2;
    unsigned int levels = 2;
    unsigned int bands = 2;
    unsigned int expectedLevel = 1;
    unsigned int expectedBand = 1;
    testOutputIndexToLevelBandPassed =
      testOutputIndexToLevelBandPassed && IndexToLevelBandTest(linearIndex, levels, bands, expectedLevel, expectedBand);
  }
  {
    unsigned int linearIndex = 3;
    unsigned int levels = 2;
    unsigned int bands = 2;
    unsigned int expectedLevel = 1;
    unsigned int expectedBand = 2;
    testOutputIndexToLevelBandPassed =
      testOutputIndexToLevelBandPassed && IndexToLevelBandTest(linearIndex, levels, bands, expectedLevel, expectedBand);
  }
  {
    unsigned int linearIndex = 4;
    unsigned int levels = 2;
    unsigned int bands = 2;
    unsigned int expectedLevel = levels;
    unsigned int expectedBand = 0;
    testOutputIndexToLevelBandPassed =
      testOutputIndexToLevelBandPassed && IndexToLevelBandTest(linearIndex, levels, bands, expectedLevel, expectedBand);
  }
  {
    unsigned int linearIndex = 5;
    unsigned int levels = 2;
    unsigned int bands = 2;
    ITK_TRY_EXPECT_EXCEPTION(itk::utils::IndexToLevelBandSteerablePyramid(linearIndex, levels, bands));
  }

  if (!testOutputIndexToLevelBandPassed)
  {
    std::cerr << "Test failed in OutputIndexToLevelBand." << std::endl;
  }

  // Test ComputeMaxNumberOfLevels
  bool testComputeMaxNumberOfLevelsPassed = testComputeMaxNumberOfLevels();
  if (!testComputeMaxNumberOfLevelsPassed)
  {
    std::cerr << "Test failed in ComputerMaxNumberOfLevels." << std::endl;
  }


  testPassed = testOutputIndexToLevelBandPassed && testComputeMaxNumberOfLevelsPassed;
  if (testPassed)
  {
    return EXIT_SUCCESS;
  }
  else
  {
    return EXIT_FAILURE;
  }
}

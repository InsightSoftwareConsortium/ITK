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

#include "itkTestingMacros.h"
#include "itkStdStreamStateSave.h"

#include <sstream>
#include <cstdlib>

int
itkStdStreamStateSaveTest(int, char *[])
{
  // Set the fillch of std::cout with an explicit default fill character
  std::cout.fill(' ');

  // Get the state for each format state variable for std::cout
  const std::streamsize         defaultPrecision = std::cout.precision();
  const std::streamsize         defaultWidth = std::cout.width();
  const char                    defaultFill = std::cout.fill();
  const std::ios_base::fmtflags defaultFlags = std::cout.flags();

  {
    const itk::StdStreamStateSave coutState(std::cout);

    // Change some representative state variables
    std::cout.precision(14);
    std::cout.width(25);
    const int anInt = 123;
    std::cout.fill('%');
    std::cout << std::left << anInt << '\n';
    std::cout << std::showpos << anInt << '\n';
    std::cout << std::hex << anInt << '\n';
    std::cout << std::showbase << std::hex << anInt << '\n';
    const bool aBool = false;
    std::cout << aBool << '\n';
    std::cout << std::boolalpha << aBool << '\n';
    const double aDouble = 123.e-5;
    std::cout << aDouble << '\n';
    std::cout << std::scientific << aDouble << '\n';

  } // coutState goes out of scope and will restore original format state

  std::stringstream stream;
  // Set the fillch of std::stringstream with an explicit default fill character
  stream.fill(' ');

  const int originalInt = 10;
  {
    const itk::StdStreamStateSave sstreamState(stream);

    // Change some representative state variables
    stream.precision(14);
    stream.width(25);
    const int anInt = originalInt;
    stream.fill('%');
    stream << std::left << anInt << '\n';
    stream << std::showpos << anInt << '\n';
    stream << std::hex << anInt << '\n';
    stream << std::showbase << std::hex << anInt << '\n';
    const bool aBool = false;
    stream << aBool << '\n';
    stream << std::boolalpha << aBool << '\n';
    const double aDouble = 123.e-5;
    stream << aDouble << '\n';
    stream << std::scientific << aDouble << '\n';

  } // sstreamState goes out of scope and will restore original format state

  // Verify the value read from the stream matches the original value
  // written to the stream. If they do not match, then the hex state
  // is still in effect.
  int inputInt;
  stream >> inputInt;
  ITK_TEST_EXPECT_EQUAL(originalInt, inputInt);

  // Verify that the default is reset for std::cout
  ITK_TEST_EXPECT_EQUAL(std::cout.precision(), defaultPrecision);
  ITK_TEST_EXPECT_EQUAL(std::cout.width(), defaultWidth);
  ITK_TEST_EXPECT_EQUAL(std::cout.fill(), defaultFill);
  ITK_TEST_EXPECT_EQUAL(std::cout.flags(), defaultFlags);

  ITK_TEST_EXPECT_EQUAL(stream.precision(), defaultPrecision);
  ITK_TEST_EXPECT_EQUAL(stream.width(), defaultWidth);
  ITK_TEST_EXPECT_EQUAL(stream.fill(), defaultFill);
  ITK_TEST_EXPECT_EQUAL(stream.flags(), defaultFlags);

  return EXIT_SUCCESS;
}

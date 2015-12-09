/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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

int itkStdStreamStateSaveTest(int, char* [] )
{
  // Set the fillch of std::cout with an explicit default fill character
  std::cout.fill(' ');

  // Get the state for each format state variable for std::cout
  std::streamsize         defaultPrecision = std::cout.precision();
  std::streamsize         defaultWidth = std::cout.width();
  const char              defaultFill = std::cout.fill();
  std::ios_base::fmtflags defaultFlags = std::cout.flags();

  {
  itk::StdStreamStateSave coutState(std::cout);

  // Change some representative state variables
  std::cout.precision(14);
  std::cout.width(25);
  int anInt = 123;
  std::cout.fill('%');
  std::cout << std::left << anInt << std::endl;
  std::cout << std::showpos << anInt << std::endl;
  std::cout << std::hex << anInt << std::endl;
  std::cout << std::showbase << std::hex << anInt << std::endl;
  bool aBool = false;
  std::cout << aBool << std::endl;
  std::cout << std::boolalpha << aBool << std::endl;
  double aDouble = 123.e-5;
  std::cout << aDouble << std::endl;
  std::cout << std::scientific << aDouble << std::endl;

  } // coutState goes out of scope and will restore original format state

  std::stringstream stream;
  // Set the fillch of std::stringstream with an explicit default fill character
  stream.fill(' ');

  int originalInt = 10;
  {
  itk::StdStreamStateSave sstreamState(stream);

  // Change some representative state variables
  stream.precision(14);
  stream.width(25);
  int anInt = originalInt;
  stream.fill('%');
  stream << std::left << anInt << std::endl;
  stream << std::showpos << anInt << std::endl;
  stream << std::hex << anInt << std::endl;
  stream << std::showbase << std::hex << anInt << std::endl;
  bool aBool = false;
  stream << aBool << std::endl;
  stream << std::boolalpha << aBool << std::endl;
  double aDouble = 123.e-5;
  stream << aDouble << std::endl;
  stream << std::scientific << aDouble << std::endl;

  } // sstreamState goes out of scope and will restore original format state

  // Verify the value read from the stream matches the original value
  // written to the stream. If they do not match, then the hex state
  // is still in effect.
  int inputInt;
  stream >> inputInt;
  TEST_EXPECT_EQUAL(originalInt, inputInt);

  // Verify that the default is reset for std::cout
  TEST_EXPECT_EQUAL(std::cout.precision(), defaultPrecision);
  TEST_EXPECT_EQUAL(std::cout.width(), defaultWidth);
  TEST_EXPECT_EQUAL(std::cout.fill(), defaultFill);
  TEST_EXPECT_EQUAL(std::cout.flags(), defaultFlags);

  TEST_EXPECT_EQUAL(stream.precision(), defaultPrecision);
  TEST_EXPECT_EQUAL(stream.width(), defaultWidth);
  TEST_EXPECT_EQUAL(stream.fill(), defaultFill);
  TEST_EXPECT_EQUAL(stream.flags(), defaultFlags);

  return EXIT_SUCCESS;
}

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

#include <type_traits>

#include "itkMetaProgrammingLibrary.h"


int
itkMetaProgrammingLibraryTest(int, char *[])
{
  using namespace itk::mpl;

  // Or between constants
  static_assert(OrC<true, true, true>::Value == true, "Unit test failed");
  static_assert(OrC<true, true, false>::Value == true, "Unit test failed");
  static_assert(OrC<true, false, true>::Value == true, "Unit test failed");
  static_assert(OrC<true, false, false>::Value == true, "Unit test failed");
  static_assert(OrC<false, true, true>::Value == true, "Unit test failed");
  static_assert(OrC<false, true, false>::Value == true, "Unit test failed");
  static_assert(OrC<false, false, true>::Value == true, "Unit test failed");
  static_assert(OrC<false, false, false>::Value == false, "Unit test failed");

  static_assert(OrC<true, true>::Value == true, "Unit test failed");
  static_assert(OrC<true, false>::Value == true, "Unit test failed");
  static_assert(OrC<false, true>::Value == true, "Unit test failed");
  static_assert(OrC<false, false>::Value == false, "Unit test failed");

  // Or between types
  static_assert(std::is_same<Or<TrueType, TrueType, TrueType>::Type, TrueType>::value, "Unit test failed");
  static_assert(std::is_same<Or<TrueType, TrueType, FalseType>::Type, TrueType>::value, "Unit test failed");
  static_assert(std::is_same<Or<TrueType, FalseType, TrueType>::Type, TrueType>::value, "Unit test failed");
  static_assert(std::is_same<Or<TrueType, FalseType, FalseType>::Type, TrueType>::value, "Unit test failed");
  static_assert(std::is_same<Or<FalseType, TrueType, TrueType>::Type, TrueType>::value, "Unit test failed");
  static_assert(std::is_same<Or<FalseType, TrueType, FalseType>::Type, TrueType>::value, "Unit test failed");
  static_assert(std::is_same<Or<FalseType, FalseType, TrueType>::Type, TrueType>::value, "Unit test failed");
  static_assert(std::is_same<Or<FalseType, FalseType, FalseType>::Type, FalseType>::value, "Unit test failed");

  static_assert(std::is_same<Or<TrueType, TrueType>::Type, TrueType>::value, "Unit test failed");
  static_assert(std::is_same<Or<TrueType, FalseType>::Type, TrueType>::value, "Unit test failed");
  static_assert(std::is_same<Or<FalseType, TrueType>::Type, TrueType>::value, "Unit test failed");
  static_assert(std::is_same<Or<FalseType, FalseType>::Type, FalseType>::value, "Unit test failed");

  // And between constants
  static_assert(AndC<true, true>::Value == true, "Unit test failed");
  static_assert(AndC<true, false>::Value == false, "Unit test failed");
  static_assert(AndC<false, true>::Value == false, "Unit test failed");
  static_assert(AndC<false, false>::Value == false, "Unit test failed");

  // And between types
  static_assert(std::is_same<And<TrueType, TrueType>::Type, TrueType>::value, "Unit test failed");
  static_assert(std::is_same<And<TrueType, FalseType>::Type, FalseType>::value, "Unit test failed");
  static_assert(std::is_same<And<FalseType, TrueType>::Type, FalseType>::value, "Unit test failed");
  static_assert(std::is_same<And<FalseType, FalseType>::Type, FalseType>::value, "Unit test failed");

  // Xor between constants
  static_assert(XorC<true, true>::Value == false, "Unit test failed");
  static_assert(XorC<true, false>::Value == true, "Unit test failed");
  static_assert(XorC<false, true>::Value == true, "Unit test failed");
  static_assert(XorC<false, false>::Value == false, "Unit test failed");

  // Xor between types
  static_assert(std::is_same<Xor<TrueType, TrueType>::Type, FalseType>::value, "Unit test failed");
  static_assert(std::is_same<Xor<TrueType, FalseType>::Type, TrueType>::value, "Unit test failed");
  static_assert(std::is_same<Xor<FalseType, TrueType>::Type, TrueType>::value, "Unit test failed");
  static_assert(std::is_same<Xor<FalseType, FalseType>::Type, FalseType>::value, "Unit test failed");

  // Not between constants
  static_assert(NotC<true>::Value == false, "Unit test failed");
  static_assert(NotC<false>::Value == true, "Unit test failed");

  // Not between types
  static_assert(std::is_same<Not<TrueType>::Type, FalseType>::value, "Unit test failed");
  static_assert(std::is_same<Not<FalseType>::Type, TrueType>::value, "Unit test failed");

  return EXIT_SUCCESS;
}

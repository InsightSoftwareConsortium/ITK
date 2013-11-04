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


// C++11 moved the type traits from the std::tr1 namespace to just std
// namespace. This tries to compile and check for compatibility with
// the C++11 for the type_traits.

#include <type_traits>

int main(void)
{
    // Just try to use one type_traits function in tr1
  return std::is_convertible<float, double>::value;
}

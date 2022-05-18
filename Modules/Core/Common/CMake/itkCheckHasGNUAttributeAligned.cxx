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

namespace itk
{
// Opened this namespace, just to avoid KWStyle check error, "namespace not defined".
}

//
// Check if the compoler support the GNU attribute extension for
// alignment, and does not contain a bug which causes internal
// compiler segfault.
//

struct A
{
  char a;
};

struct B
{
  char b;
} __attribute__((aligned(64)));


// BUG DETECTION: This following usage may generate a segfault during
// compilation.
//
// The following block of code causes an internal compiler error with
// Apple's (GCC) 4.2.1 (Apple Inc. build5666) (dot 3) compiler when
// compiled in debug mode with the -g flag.
//
template <typename T>
class foo
{
  // NOTE: implicit constructor is required for this test

  struct A
  {
    char a;
  };
  using AlignedA = A __attribute__((aligned(64)));
  AlignedA * AlignedElementsOfA;
};


// This structure will generate a compiler error if the template
// argument is false
template <bool V>
struct OnlyTrue;
template <>
struct OnlyTrue<true>
{
  static bool Result = true;
};


int
main()
{
  foo<int> f;

  using AlignedA = A __attribute__((aligned(64)));

  return OnlyTrue<__alignof__(AlignedA) == 64>::Result && OnlyTrue<__alignof__(B) == 64>::Result;


  return 0;
}

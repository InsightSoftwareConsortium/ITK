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

/*
   This file tests if the compiler supports partial specialization or not
*/

template <class T>
class victor
{
  T data[256];

public:
  victor() {}
  T &operator[](unsigned i) { return data[i]; }
};

template <class T>
class victor<T *>
{
  T *data[256];
 public:
  T * &operator[](unsigned i) { return data[i]; }
  void slarf() { data[0] += (data[2] - data[1]); }
};

template <class A, class R>
struct foo {
  typedef A a;
  typedef R r;
};

template <class T> struct foo<T *, T *> { void bar() {} };
template <class T> struct foo<int *, T> { void baz() {} };

int main()
{
  return 0;
}


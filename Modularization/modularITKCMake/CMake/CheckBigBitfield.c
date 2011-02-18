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
#include <stdio.h>

#ifdef _WIN32
typedef unsigned __int64 nio_ullong;
#else
typedef unsigned long long nio_ullong;
#endif

typedef union {
  double d;
  struct {
    nio_ullong bogus0 : 60;
    unsigned int bogus1 : 2;
    unsigned int bogus2 : 2;
  } c;
} nio_test;

int
main(int argc, char *argv[]) {

  printf("hello, world.\n");
  return 0;
}

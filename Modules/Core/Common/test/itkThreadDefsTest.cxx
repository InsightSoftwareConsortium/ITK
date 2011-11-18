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

#include "itkObject.h"

int itkThreadDefsTest (int, char* [] )
{
#if defined(_NOTHREADS) && defined(ITK_USE_PTHREADS)
  std::cout << "ERROR: _NOTHREADS is defined and ITK_USE_PTHREADS is defined." << std::endl;
  std::cout << "STL containers WILL NOT BE thread safe on GNU c++ systems." << std::endl;
    std::cout << "The C++ compiler needs a -D_PTHREADS option." << std::endl;
  return EXIT_FAILURE;
#endif

  return EXIT_SUCCESS;
}

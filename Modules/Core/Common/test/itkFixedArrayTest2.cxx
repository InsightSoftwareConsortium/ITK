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

#include <iostream>
#include "itkFixedArray.h"
#include <time.h>
#include <cstring>
#include <string.h>

int itkFixedArrayTest2(int, char* [] )
{
  // Define the number of elements in the array
  const unsigned int nelements = 10000000L;

  // Define the number of runs used for timing
  const unsigned int nrun = 10;

  // Declare a simple timer
  clock_t t;

  typedef itk::FixedArray<double,2> ArrayType;

  // Declare an array of nelements FixedArray
  // and add a small margin to play with pointers
  // but not map outside the allocated memory
  ArrayType * vec = new ArrayType[nelements+8];

  // Fill it up with zeros
  memset(vec,0,(nelements+8)*sizeof(ArrayType));


  // Display the alignment of the array
  std::cout << "Initial alignment: " << (((size_t)vec)& 7) << "\n";

  // Start a simple experiment
  t = clock();
  double acc1 = 0.0;

  for (unsigned int i=0;i<nrun;++i)
    {
    for (unsigned int j=0;j<nelements;++j)
      {
      acc1 += vec[j][0];
      }
    }

  // Get the final timing and display it
  t=clock() - t;

  const double time1 = (t*1000.0) / CLOCKS_PER_SEC;

  std::cout << "Initial execution time: "
            << time1 << "ms\n";


  // We now force an 8 bytes aligned array

  // Cast the pointer to char to play with bytes
  char * p = reinterpret_cast<char*>( vec );

  // Move the char pointer until it is aligned on 8 bytes
  while ( ( (size_t)p ) % 8 )
    {
    ++p;
    }

  // Cast the 8 bytes aligned pointer back to the original type
  ArrayType * vec2 = reinterpret_cast<ArrayType*>( p );

  // Make sure the new pointer is well aligned by
  // displaying the alignment
  std::cout << "New alignment: " << (((size_t)vec2)& 7) << "\n";

  // Start the simple experiment on the 8 byte aligned array
  t = clock();
  double acc2 = 0.0;

  for (unsigned int i=0;i<nrun;++i)
    {
    for (unsigned int j=0;j<nelements;++j)
      {
      acc2 += vec2[j][0];
      }
    }

  // Get the final timing and display it
  t = clock() - t;

  const double time2 = (t*1000.0) / CLOCKS_PER_SEC;

  std::cout << "Execution time: "
            << time2 << "ms\n";


  // Free up the memory
  delete[] vec;

  const double ratio = 100.0 * ( time1 - time2 ) / time2;

  const bool sameptr = (vec==vec2);
  if (sameptr) std::cout << "Same pointers: true" <<std::endl;
  else std::cout << "Same pointers: false" <<std::endl;

  std::cout << "Performance ratio = " << ratio << "%" << std::endl;

  if( !sameptr && ratio > 20.0 ) // tolerates only 20%
    {
    std::cerr << "Performance degraded below tolerance" << std::endl;
    return EXIT_FAILURE;
    }


  // Make sure we do something with the sums otherwise everything
  // could be optimized away by the compiler
  if( acc1 == 0.0 && acc2 == 0.0 )
    {
    return EXIT_SUCCESS;
    }

  return EXIT_FAILURE;
}

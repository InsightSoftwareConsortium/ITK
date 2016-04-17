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

#include "itkArray.h"
#include "itkNumericTraits.h"

// Explicit instantiation to make sure all methods are compiled.
template class itk::Array<float>;

int itkArrayTest(int, char* [] )
{

  typedef itk::Array<float>  FloatArrayType;
  typedef itk::Array<double> DoubleArrayType;

  FloatArrayType  fa(10);
  DoubleArrayType da(10);

/**
 * The following section tests the functionality of the Array's
 * memory management.
 */

  //
  // Create an itk::Array which manages its own memory
  //
  FloatArrayType myOwnBoss;
  myOwnBoss.SetSize( 5 );
  myOwnBoss.Fill( 2.0 + 1.0f / 3.0f);
  myOwnBoss[0] = 2.0f / 3.0f;
  myOwnBoss[1] = itk::NumericTraits<float>::max();
  myOwnBoss[2] = itk::NumericTraits<float>::min();
  myOwnBoss[3] = 1.0f;

  //
  // Create an itk::Array which does not manage its own memory
  //
  const unsigned int n = 7;
  float buffer[n];
  FloatArrayType notMyOwnBoss;
  notMyOwnBoss.SetSize( n );
  notMyOwnBoss.SetData( buffer, false );
  notMyOwnBoss.Fill( 4.0 );

  FloatArrayType notMyOwnBossToo;
  notMyOwnBossToo.SetSize( n );
  notMyOwnBossToo.SetData( buffer, false );

  //
  // Copy an itk::Array which manages its own memory
  //
  FloatArrayType test1;
  test1 = myOwnBoss;
  std::cout << test1 << std::endl;

  //
  // Copy an itk::Array which does not manage its own memory
  //
  FloatArrayType test2;
  test2 = notMyOwnBoss;
  std::cout << test2 << std::endl;

  //
  // Testing itk::Array
  // which does not manage its own memory copying an itk::Array
  // which does.
  //
  notMyOwnBoss = myOwnBoss;
  std::cout << notMyOwnBoss << std::endl;

  //
  // Calling SetSize with an argument same as the current
  // size
  //
  notMyOwnBossToo.SetSize( notMyOwnBossToo.GetSize() );

  //
  // Calling SetSize with an argument different to the current
  // size
  //
  notMyOwnBossToo.SetSize( notMyOwnBossToo.GetSize() + 1 );
  notMyOwnBossToo.Fill( 6.0 );
  std::cout << notMyOwnBossToo << std::endl;

  // Exercise operator=( VnlVectorType& )
  test2 = test1;

  // Test the case where we construct an array that points
  // to a user allocated buffer where the user wants to
  // maintain responsibility for deleting the array.
  const size_t testSizeForArraySetDataSameSize=10;
  FloatArrayType objectToCopy(testSizeForArraySetDataSameSize);
  float* data = new float[testSizeForArraySetDataSameSize];
  objectToCopy.SetDataSameSize(data); // This implictly means LetArrayManageMemory=false

  // Make a copy of the array which is not managing its own memory.
  FloatArrayType copy(objectToCopy);

  // DO a double
  //
  // Create an itk::Array which manages its own memory
  //
  DoubleArrayType myOwnDouble;
  myOwnDouble.SetSize( 5 );
  myOwnDouble.Fill( 2.0 + 1.0 / 3.0);
  myOwnDouble[0] = 2.0 / 3.0;
  myOwnDouble[1] = itk::NumericTraits<double>::max();
  myOwnDouble[2] = itk::NumericTraits<double>::min();
  myOwnDouble[3] = 1.0;
  std::cout << myOwnDouble << std::endl;

  delete[] data;

  return EXIT_SUCCESS;

}

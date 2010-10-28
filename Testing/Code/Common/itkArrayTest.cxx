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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkArray.h"

// Explicit instantiation to make sure all methods are compiled.
#ifndef ITK_EXPLICIT_INSTANTIATION
  template class itk::Array<float>;
#endif

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
  myOwnBoss.Fill( 2.0 );

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

  return EXIT_SUCCESS;

}

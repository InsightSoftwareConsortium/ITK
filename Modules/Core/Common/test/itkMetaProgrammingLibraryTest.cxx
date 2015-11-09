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

#include "itkMetaProgrammingLibrary.h"
#include "itkIsSame.h"
#include "itkStaticAssert.h"


int itkMetaProgrammingLibraryTest(int,char*[])
{
  using namespace itk::mpl;

  // Or between constants
  itkStaticAssert(( OrC<true,  true,  true >::Value == true ), "Unit test failed");
  itkStaticAssert(( OrC<true,  false, true >::Value == true ), "Unit test failed");
  itkStaticAssert(( OrC<true,  true,  false>::Value == true ), "Unit test failed");
  itkStaticAssert(( OrC<false, false, true >::Value == true ), "Unit test failed");
  itkStaticAssert(( OrC<true,  false, false>::Value == true ), "Unit test failed");
  itkStaticAssert(( OrC<false, true,  false>::Value == true ), "Unit test failed");
  itkStaticAssert(( OrC<false, false, false>::Value == false), "Unit test failed");

  itkStaticAssert(( OrC<true,  true >::Value == true ), "Unit test failed");
  itkStaticAssert(( OrC<true,  false>::Value == true ), "Unit test failed");
  itkStaticAssert(( OrC<false, true >::Value == true ), "Unit test failed");
  itkStaticAssert(( OrC<false, false>::Value == false), "Unit test failed");

  // Or between types
  itkStaticAssert(( IsSame<Or<TrueType,  TrueType,  TrueType >::Type, TrueType >::Value), "Unit test failed");
  itkStaticAssert(( IsSame<Or<TrueType,  FalseType, TrueType >::Type, TrueType >::Value), "Unit test failed");
  itkStaticAssert(( IsSame<Or<TrueType,  TrueType,  FalseType>::Type, TrueType >::Value), "Unit test failed");
  itkStaticAssert(( IsSame<Or<FalseType, FalseType, TrueType >::Type, TrueType >::Value), "Unit test failed");
  itkStaticAssert(( IsSame<Or<TrueType,  FalseType, FalseType>::Type, TrueType >::Value), "Unit test failed");
  itkStaticAssert(( IsSame<Or<FalseType, TrueType,  FalseType>::Type, TrueType >::Value), "Unit test failed");
  itkStaticAssert(( IsSame<Or<FalseType, FalseType, FalseType>::Type, FalseType>::Value), "Unit test failed");

  itkStaticAssert(( IsSame<Or<TrueType,  TrueType >::Type, TrueType >::Value), "Unit test failed");
  itkStaticAssert(( IsSame<Or<TrueType,  FalseType>::Type, TrueType >::Value), "Unit test failed");
  itkStaticAssert(( IsSame<Or<FalseType, TrueType >::Type, TrueType >::Value), "Unit test failed");
  itkStaticAssert(( IsSame<Or<FalseType, FalseType>::Type, FalseType>::Value), "Unit test failed");

  // And between constants
  itkStaticAssert(( AndC<true,  true >::Value == true ), "Unit test failed");
  itkStaticAssert(( AndC<true,  false>::Value == false), "Unit test failed");
  itkStaticAssert(( AndC<false, true >::Value == false), "Unit test failed");
  itkStaticAssert(( AndC<false, false>::Value == false), "Unit test failed");

  // And between types
  itkStaticAssert(( IsSame<And<TrueType,  TrueType >::Type, TrueType >::Value), "Unit test failed");
  itkStaticAssert(( IsSame<And<TrueType,  FalseType>::Type, FalseType>::Value), "Unit test failed");
  itkStaticAssert(( IsSame<And<FalseType, TrueType >::Type, FalseType>::Value), "Unit test failed");
  itkStaticAssert(( IsSame<And<FalseType, FalseType>::Type, FalseType>::Value), "Unit test failed");

  // Xor between constants
  itkStaticAssert(( XorC<true,  true >::Value == false), "Unit test failed");
  itkStaticAssert(( XorC<true,  false>::Value == true ), "Unit test failed");
  itkStaticAssert(( XorC<false, true >::Value == true ), "Unit test failed");
  itkStaticAssert(( XorC<false, false>::Value == false), "Unit test failed");

  // Xor between types
  itkStaticAssert(( IsSame<Xor<TrueType,  TrueType >::Type, FalseType>::Value), "Unit test failed");
  itkStaticAssert(( IsSame<Xor<TrueType,  FalseType>::Type, TrueType >::Value), "Unit test failed");
  itkStaticAssert(( IsSame<Xor<FalseType, TrueType >::Type, TrueType >::Value), "Unit test failed");
  itkStaticAssert(( IsSame<Xor<FalseType, FalseType>::Type, FalseType>::Value), "Unit test failed");

  // Not between constants
  itkStaticAssert(( NotC<true >::Value == false), "Unit test failed");
  itkStaticAssert(( NotC<false>::Value == true ), "Unit test failed");

  // Not between types
  itkStaticAssert(( IsSame<Not<TrueType >::Type, FalseType>::Value), "Unit test failed");
  itkStaticAssert(( IsSame<Not<FalseType>::Type, TrueType >::Value), "Unit test failed");

  return EXIT_SUCCESS;
}

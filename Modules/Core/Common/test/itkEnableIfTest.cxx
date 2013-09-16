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

#include "itkEnableIf.h"
#include "itkIsSame.h"
#include "itkNumericTraits.h"
#include "itkFixedArray.h"
#include <stdlib.h>
#include <iostream>

namespace
{

  template< unsigned int D >
  typename itk::EnableIfC< D == 1 >::Type
  test1( void )
  {std::cout << "D is one" << std::endl;}

  template< unsigned int D >
  typename itk::EnableIfC< D != 1 >::Type
  test1( void )
  {std::cout << "D is not one" << std::endl;}

  template< typename T >
  typename itk::EnableIfC< sizeof( T ) == 1 >::Type
  test2( void )
  {std::cout << "T is one in size" << std::endl;}

  template< typename T >
  typename itk::EnableIfC< sizeof(T) != 1 >::Type
  test2( void )
  {std::cout << "D is not one in size" << std::endl;}


  template< typename T >
  typename itk::EnableIfC< itk::IsSame<T, typename itk::NumericTraits<T>::ValueType>::Value >::Type
  test3( void )
  {std::cout << "T is enabled" << std::endl;}

  template< typename T >
  typename itk::DisableIfC< itk::IsSame<T, typename itk::NumericTraits<T>::ValueType>::Value >::Type
  test3( void )
  {std::cout << "T is disabled" << std::endl;}

  template< typename T >
  typename itk::EnableIfC< itk::IsSame<T, typename itk::NumericTraits<T>::ValueType>::Value >::Type
  test4( const T & )
  {std::cout << "T is enabled" << std::endl;}

  template< typename T >
  typename itk::DisableIfC< itk::IsSame<T, typename itk::NumericTraits<T>::ValueType>::Value >::Type
  test4( const T & )
  {std::cout << "T is disabled" << std::endl;}


}

int itkEnableIfTest( int, char*[] )
{

  test1<0>();
  test1<1>();
  test1<2>();

  test2<int>();
  test2<void*>();

  test3<int>();
  test3<itk::FixedArray<double,3> >();

  test4( 1 );
  test4( itk::FixedArray<double,3>() );
  return EXIT_SUCCESS;
}

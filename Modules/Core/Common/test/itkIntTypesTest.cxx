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

#include "itkIntTypes.h"
#include "itkNumericTraits.h"

namespace
{

template <typename T>
bool CheckSize( size_t size, T* = 0 )
{
  return ( sizeof( T ) == size );
}

template <typename T>
bool CheckAtleastSize( size_t size, T* = 0 )
{
  return ( sizeof( T ) >= size );
}


template <typename T>
bool CheckTraits( bool issigned, T* = 0 )
{
  // make sure that we have a specialized NumericTraits
  T t0 = itk::NumericTraits<T>::ZeroValue();
  T t1 = itk::NumericTraits<T>::OneValue();

  // just here so that we use the variable
  itk::NumericTraits<T>::IsPositive(t1);
  itk::NumericTraits<T>::IsNegative(t0);

  // make sure the numeric_limits is specialized
  if (!itk::NumericTraits<T>::is_specialized)
    return false;

  if (itk::NumericTraits<T>::is_signed != issigned )
    return false;

  return true;
}


template<typename T>
bool CheckType( size_t size, bool exactSize, bool issigned, const char * name, T* = 0 )
{
  bool ret = true;

  if ( exactSize )
    ret &= CheckSize<T>( size );
  else
    ret &= CheckAtleastSize<T>( size );

  ret &= CheckTraits<T>( issigned );

  if (ret)
    return ret;

  std::cout << "error with type \"" << name
            << "\" sizeof: " << sizeof(T)
            << " specialized: " << itk::NumericTraits<T>::is_specialized
            << " digits: " << itk::NumericTraits<T>::digits
            << " signed: " << itk::NumericTraits<T>::is_signed
            << std::endl;
  return ret;

}

} // namespace

#define CHECKTYPE( T, SIZE, EXACT, ISSIGNED ) CheckType<T>( SIZE, EXACT, ISSIGNED, #T )

int itkIntTypesTest( int, char *[] )
{
  bool pass = true;
  // fixed width types
  pass &= CHECKTYPE( itk::int8_t, 1, true, true );
  pass &= CHECKTYPE( itk::uint8_t, 1, true, false );

  pass &= CHECKTYPE( itk::int16_t, 2, true, true );
  pass &= CHECKTYPE( itk::uint16_t, 2, true, false );

  pass &= CHECKTYPE( itk::int32_t, 4, true, true );
  pass &= CHECKTYPE( itk::uint32_t, 4, true, false );


  // least types
  pass &= CHECKTYPE( itk::int_least8_t, 1, false, true );
  pass &= CHECKTYPE( itk::uint_least8_t, 1, false, false );

  pass &= CHECKTYPE( itk::int_least16_t, 2, false, true );
  pass &= CHECKTYPE( itk::uint_least16_t, 2, false, false );

  pass &= CHECKTYPE( itk::int_least32_t, 4, false, true );
  pass &= CHECKTYPE( itk::uint_least32_t, 4, false, false );

  // fast types
  pass &= CHECKTYPE( itk::int_fast8_t, 1, false, true );
  pass &= CHECKTYPE( itk::uint_fast8_t, 1, false, false );

  pass &= CHECKTYPE( itk::int_fast16_t, 2, false, true );
  pass &= CHECKTYPE( itk::uint_fast16_t, 2, false, false );

  pass &= CHECKTYPE( itk::int_fast32_t, 4, false, true );
  pass &= CHECKTYPE( itk::uint_fast32_t, 4, false, false );

  pass &= CHECKTYPE( itk::int64_t, 8, true, true );
  pass &= CHECKTYPE( itk::uint64_t, 8, true, false );

  pass &= CHECKTYPE( itk::int_least64_t, 8, false, true );
  pass &= CHECKTYPE( itk::uint_least64_t, 8, false, false );

  pass &= CHECKTYPE( itk::int_fast64_t, 8, false, true );
  pass &= CHECKTYPE( itk::uint_fast64_t, 8, false, false );

  pass &= CHECKTYPE( itk::intmax_t, 4, false, true );
  pass &= CHECKTYPE( itk::uintmax_t, 4, false, false );

  pass &= CHECKTYPE( itk::intptr_t, sizeof(void *), false, true );
  pass &= CHECKTYPE( itk::uintptr_t, sizeof(void *), false, false );


  if ( pass )
    return EXIT_SUCCESS;

  return EXIT_FAILURE;
}

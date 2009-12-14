/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIntTypesTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkIntTypes.h"

#include "itkNumericTraits.h"

namespace 
{

template <typename T>
bool CheckSize( size_t size ) 
{
  return ( sizeof( T ) == size );
}

template <typename T>
bool CheckAtleastSize( size_t size )
{
  return ( sizeof( T ) >= size );
}


template <typename T>
bool CheckTraits( bool issigned ) 
{
  // make sure that we have a specialized NumericTraits
  T t0 = itk::NumericTraits<T>::Zero;
  T t1 = itk::NumericTraits<T>::One;
  
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
bool CheckType( size_t size, bool exact, bool issigned, const std::string &name)
{
  bool ret = true;
  
  if ( exact )
    ret &= CheckSize<T>( size );
  else 
    ret &= CheckAtleastSize<T>( size );

  ret &= CheckTraits<T>( issigned );

  if (ret) 
    return ret;
  
  std::cout << "error with type \"" << name 
            << "\" sizeof: " << sizeof(T)
            << " specialized: " << itk::NumericTraits<T>::is_specialized
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
  pass &= CHECKTYPE( int16_t, 1, true, true );
  pass &= CHECKTYPE( uint8_t, 1, true, false );

  pass &= CHECKTYPE( int16_t, 2, true, true );
  pass &= CHECKTYPE( uint16_t, 2, true, false );

  pass &= CHECKTYPE( int32_t, 4, true, true );
  pass &= CHECKTYPE( uint32_t, 4, true, false );


  // least types
  pass &= CHECKTYPE( int_least8_t, 1, false, true );
  pass &= CHECKTYPE( uint_least8_t, 1, false, false );

  pass &= CHECKTYPE( int_least16_t, 2, false, true );
  pass &= CHECKTYPE( uint_least16_t, 2, false, false );

  pass &= CHECKTYPE( int_least32_t, 4, false, true );
  pass &= CHECKTYPE( uint_least32_t, 4, false, false );

  // fast types
  pass &= CHECKTYPE( int_fast8_t, 1, false, true );
  pass &= CHECKTYPE( uint_fast8_t, 1, false, false );
  
  pass &= CHECKTYPE( int_fast16_t, 2, false, true );
  pass &= CHECKTYPE( uint_fast16_t, 2, false, false );

  pass &= CHECKTYPE( int_fast32_t, 4, false, true );
  pass &= CHECKTYPE( uint_fast32_t, 4, false, false );

#ifndef ITK_NO_INT64

  pass &= CHECKTYPE( int64_t, 8, true, true );
  pass &= CHECKTYPE( uint64_t, 8, true, false );

  pass &= CHECKTYPE( int_least64_t, 8, false, true );
  pass &= CHECKTYPE( uint_least64_t, 8, false, false );

  pass &= CHECKTYPE( int_fast64_t, 8, false, true );
  pass &= CHECKTYPE( uint_fast64_t, 8, false, false );

#endif // ITK_NO_INT64

  
  pass &= CHECKTYPE( intmax_t, 4, false, true );
  pass &= CHECKTYPE( uintmax_t, 4, false, false );

  pass &= CHECKTYPE( intptr_t, sizeof(void *), false, true );
  pass &= CHECKTYPE( uintptr_t, sizeof(void *), false, false );


  if ( pass )
    return EXIT_SUCCESS;

  return EXIT_FAILURE;
}

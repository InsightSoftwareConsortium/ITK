/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkByteSwapTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include <iostream>
#include "itkByteSwapper.h"

int main ( int argc, char* argv[] )
{
  // Test out the Byte Swap code

  std::cout << "Starting test" << std::endl;

  unsigned char uc = 'a', uc1 = 'a';
  unsigned short us = 1, us1 = 1;
  unsigned int ui = 1, ui1 = 1;
  unsigned long ul = 1, ul1 = 1;
  float f = 1.0, f1 = 1.0;
  double d = 1.0, d1 = 1.0;
  
  
  // Try to swap a char

  if ( itk::ByteSwapper<int>::IsBigEndian() == itk::ByteSwapper<int>::IsLE() )
    {
    return 1;
    }
  if ( itk::ByteSwapper<int>::IsBE() == itk::ByteSwapper<int>::IsLittleEndian() )
    {
    return 1;
    }

  if ( itk::ByteSwapper<int>::IsBigEndian() )
    {
    itk::ByteSwapper<unsigned char>::SwapLE ( &uc );
    itk::ByteSwapper<unsigned char>::SwapLE ( &uc );
    }
  else
    {
    itk::ByteSwapper<unsigned char>::SwapBE ( &uc );
    itk::ByteSwapper<unsigned char>::SwapBE ( &uc );
    }
  if ( uc != uc1 )
    {
    return 1;
    }
  std::cout << "Passed unsigned char: " << uc << std::endl;

  if ( itk::ByteSwapper<int>::IsBE() )
    {
    itk::ByteSwapper<unsigned short>::SwapLE ( &us );
    itk::ByteSwapper<unsigned short>::SwapLE ( &us );
    }
  else
    {
    itk::ByteSwapper<unsigned short>::SwapBE ( &us );
    itk::ByteSwapper<unsigned short>::SwapBE ( &us );
    }
  if ( us != us1 )
    {
    return 1;
    }
  std::cout << "Passed unsigned short: " << us << std::endl;

  if ( itk::ByteSwapper<int>::IsBigEndian() )
    {
    itk::ByteSwapper<unsigned int>::SwapLE ( &ui );
    itk::ByteSwapper<unsigned int>::SwapLE ( &ui );
    }
  else
    {
    itk::ByteSwapper<unsigned int>::SwapBE ( &ui );
    itk::ByteSwapper<unsigned int>::SwapBE ( &ui );
    }
  if ( ui != ui1 )
    {
    return 1;
    }
  std::cout << "Passed unsigned int: " << ui << std::endl;


  try
    {
    if ( itk::ByteSwapper<long>::IsBigEndian() )
      {
      itk::ByteSwapper<unsigned long>::SwapLE ( &ul );
      itk::ByteSwapper<unsigned long>::SwapLE ( &ul );
      }
    else
      {
      itk::ByteSwapper<unsigned long>::SwapBE ( &ul );
      itk::ByteSwapper<unsigned long>::SwapBE ( &ul );
      }
    if ( ul != ul1 )
      {
      return 1;
      }
    std::cout << "Passed unsigned long: " << ul << std::endl;
    }
  catch ( itk::ByteSwapperError &e )
    {
    std::cout << "Caught unsigned long exception size is: " << sizeof ( unsigned long ) << std::endl;
    }

  try
    {
    if ( itk::ByteSwapper<int>::IsBigEndian() )
      {
      itk::ByteSwapper<float>::SwapLE ( &f );
      itk::ByteSwapper<float>::SwapLE ( &f );
      }
    else
      {
      itk::ByteSwapper<float>::SwapBE ( &f );
      itk::ByteSwapper<float>::SwapBE ( &f );    
      }
    if ( f != f1 )
      {
      return 1;
      }
    std::cout << "Passed float: " << f << std::endl;
    }
  catch ( itk::ByteSwapperError &e )
    {
    std::cout << "Caught float exception size is: " << sizeof ( float ) << std::endl;
    return 1;
    }

  try
    {
    if ( itk::ByteSwapper<int>::IsBigEndian() )
      {
      itk::ByteSwapper<double>::SwapLE ( &d );
      itk::ByteSwapper<double>::SwapLE ( &d );
      }
    else
      {
      itk::ByteSwapper<double>::SwapBE ( &d );
      itk::ByteSwapper<double>::SwapBE ( &d );
      }
    if ( d != d1 )
      {
      return 1;
      }
    std::cout << "Passed unsigned d: " << d << std::endl;
    }
  catch ( itk::ByteSwapperError &e )
    {
    std::cout << "Caught double exception size is: " << sizeof ( double ) << std::endl;
    return 1;
    }
  return 0;
  
}

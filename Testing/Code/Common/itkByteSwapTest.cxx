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
#include "itkByteSwap.h"

int main ( int argc, char* argv[] )
{
  // Test out the Byte Swap code

  std::cout << "Starting test" << std::endl;

  unsigned char uc = 1, uc1 = 1;
  unsigned short us = 1, us1 = 1;
  unsigned int ui = 1, ui1 = 1;
  unsigned long ul = 1, ul1 = 1;
  float f = 1.0, f1 = 1.0;
  double d = 1.0, d1 = 1.0;
  
  
  // Try to swap a char

  if ( itk::ByteSwap<int>::IsBigEndian() == itk::ByteSwap<int>::IsLE() )
    {
    return 0;
    }
  if ( itk::ByteSwap<int>::IsBE() == itk::ByteSwap<int>::IsLittleEndian() )
    {
    return 0;
    }

  if ( itk::ByteSwap<int>::IsBigEndian() )
    {
    itk::ByteSwap<unsigned char>::SwapLE ( &uc );
    itk::ByteSwap<unsigned char>::SwapLE ( &uc );
    }
  else
    {
    itk::ByteSwap<unsigned char>::SwapBE ( &uc );
    itk::ByteSwap<unsigned char>::SwapBE ( &uc );
    }
  if ( uc != uc1 )
    {
    return 1;
    }
  std::cout << "Passed unsigned char: " << uc << std::endl;

  if ( itk::ByteSwap<int>::IsBE() )
    {
    itk::ByteSwap<unsigned short>::SwapLE ( &us );
    itk::ByteSwap<unsigned short>::SwapLE ( &us );
    }
  else
    {
    itk::ByteSwap<unsigned short>::SwapBE ( &us );
    itk::ByteSwap<unsigned short>::SwapBE ( &us );
    }
  if ( us != us1 )
    {
    return 1;
    }
  std::cout << "Passed unsigned short: " << us << std::endl;

  if ( itk::ByteSwap<int>::IsBigEndian() )
    {
    itk::ByteSwap<unsigned int>::SwapLE ( &ui );
    itk::ByteSwap<unsigned int>::SwapLE ( &ui );
    }
  else
    {
    itk::ByteSwap<unsigned int>::SwapBE ( &ui );
    itk::ByteSwap<unsigned int>::SwapBE ( &ui );
    }
  if ( ui != ui1 )
    {
    return 1;
    }
  std::cout << "Passed unsigned int: " << ui << std::endl;


  try
    {
    if ( itk::ByteSwap<long>::IsBigEndian() )
      {
      itk::ByteSwap<unsigned long>::SwapLE ( &ul );
      itk::ByteSwap<unsigned long>::SwapLE ( &ul );
      }
    else
      {
      itk::ByteSwap<unsigned long>::SwapBE ( &ul );
      itk::ByteSwap<unsigned long>::SwapBE ( &ul );
      }
    if ( ul != ul1 )
      {
      return 1;
      }
    std::cout << "Passed unsigned long: " << ul << std::endl;
    }
  catch ( itk::ByteSwapError &e )
    {
    std::cout << "Caught unsigned long exception size is: " << sizeof ( unsigned long ) << std::endl;
    }

  try
    {
    if ( itk::ByteSwap<int>::IsBigEndian() )
      {
      itk::ByteSwap<float>::SwapBE ( &f );
      itk::ByteSwap<float>::SwapBE ( &f );
      }
    else
      {
      itk::ByteSwap<float>::SwapLE ( &f );
      itk::ByteSwap<float>::SwapLE ( &f );    
      }
    if ( f != f1 )
      {
      return 1;
      }
    std::cout << "Passed float: " << f << std::endl;
    }
  catch ( itk::ByteSwapError &e )
    {
    std::cout << "Caught float exception size is: " << sizeof ( float ) << std::endl;
    }


  try
    {
    if ( itk::ByteSwap<int>::IsBigEndian() )
      {
      itk::ByteSwap<double>::SwapBE ( &d );
      itk::ByteSwap<double>::SwapBE ( &d );
      }
    else
      {
      itk::ByteSwap<double>::SwapBE ( &d );
      itk::ByteSwap<double>::SwapBE ( &d );
      }
    if ( d != d1 )
      {
      return 1;
      }
    std::cout << "Passed unsigned d: " << d << std::endl;
    }
  catch ( itk::ByteSwapError &e )
    {
    std::cout << "Caught double exception size is: " << sizeof ( double ) << std::endl;
    }

  
}

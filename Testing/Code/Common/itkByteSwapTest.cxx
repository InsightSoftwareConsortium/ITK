/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkByteSwapTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include "itkByteSwapper.h"

int itkByteSwapTest ( int, char*[] )
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

  if ( itk::ByteSwapper<int>::SystemIsBigEndian() == itk::ByteSwapper<int>::SystemIsLE() )
    {
    return 1;
    }
  if ( itk::ByteSwapper<int>::SystemIsBE() == itk::ByteSwapper<int>::SystemIsLittleEndian() )
    {
    return 1;
    }

  if ( itk::ByteSwapper<int>::SystemIsBigEndian() )
    {
    itk::ByteSwapper<unsigned char>::SwapFromSystemToLittleEndian ( &uc );
    itk::ByteSwapper<unsigned char>::SwapFromSystemToLittleEndian ( &uc );
    }
  else
    {
    itk::ByteSwapper<unsigned char>::SwapFromSystemToBigEndian ( &uc );
    itk::ByteSwapper<unsigned char>::SwapFromSystemToBigEndian ( &uc );
    }
  if ( uc != uc1 )
    {
    return 1;
    }
  std::cout << "Passed unsigned char: " << uc << std::endl;

  if ( itk::ByteSwapper<int>::SystemIsBE() )
    {
    itk::ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian ( &us );
    itk::ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian ( &us );
    }
  else
    {
    itk::ByteSwapper<unsigned short>::SwapFromSystemToBigEndian ( &us );
    itk::ByteSwapper<unsigned short>::SwapFromSystemToBigEndian ( &us );
    }
  if ( us != us1 )
    {
    return 1;
    }
  std::cout << "Passed unsigned short: " << us << std::endl;

  if ( itk::ByteSwapper<int>::SystemIsBigEndian() )
    {
    itk::ByteSwapper<unsigned int>::SwapFromSystemToLittleEndian ( &ui );
    itk::ByteSwapper<unsigned int>::SwapFromSystemToLittleEndian ( &ui );
    }
  else
    {
    itk::ByteSwapper<unsigned int>::SwapFromSystemToBigEndian ( &ui );
    itk::ByteSwapper<unsigned int>::SwapFromSystemToBigEndian ( &ui );
    }
  if ( ui != ui1 )
    {
    return 1;
    }
  std::cout << "Passed unsigned int: " << ui << std::endl;


  try
    {
    if ( itk::ByteSwapper<long>::SystemIsBigEndian() )
      {
      itk::ByteSwapper<unsigned long>::SwapFromSystemToLittleEndian ( &ul );
      itk::ByteSwapper<unsigned long>::SwapFromSystemToLittleEndian ( &ul );
      }
    else
      {
      itk::ByteSwapper<unsigned long>::SwapFromSystemToBigEndian ( &ul );
      itk::ByteSwapper<unsigned long>::SwapFromSystemToBigEndian ( &ul );
      }
    if ( ul != ul1 )
      {
      return 1;
      }
    std::cout << "Passed unsigned long: " << ul << std::endl;
    }
  catch ( itk::ByteSwapperError & )
    {
    std::cout << "Caught unsigned long exception size is: " << sizeof ( unsigned long ) << std::endl;
    }

  try
    {
    if ( itk::ByteSwapper<int>::SystemIsBigEndian() )
      {
      itk::ByteSwapper<float>::SwapFromSystemToLittleEndian ( &f );
      itk::ByteSwapper<float>::SwapFromSystemToLittleEndian ( &f );
      }
    else
      {
      itk::ByteSwapper<float>::SwapFromSystemToBigEndian ( &f );
      itk::ByteSwapper<float>::SwapFromSystemToBigEndian ( &f );    
      }
    if ( f != f1 )
      {
      return 1;
      }
    std::cout << "Passed float: " << f << std::endl;
    }
  catch ( itk::ByteSwapperError & )
    {
    std::cout << "Caught float exception size is: " << sizeof ( float ) << std::endl;
    return 1;
    }

  try
    {
    if ( itk::ByteSwapper<int>::SystemIsBigEndian() )
      {
      itk::ByteSwapper<double>::SwapFromSystemToLittleEndian ( &d );
      itk::ByteSwapper<double>::SwapFromSystemToLittleEndian ( &d );
      }
    else
      {
      itk::ByteSwapper<double>::SwapFromSystemToBigEndian ( &d );
      itk::ByteSwapper<double>::SwapFromSystemToBigEndian ( &d );
      }
    if ( d != d1 )
      {
      return 1;
      }
    std::cout << "Passed unsigned d: " << d << std::endl;
    }
  catch ( itk::ByteSwapperError &)
    {
    std::cout << "Good catch! Caught double exception size is: " << sizeof ( double ) << std::endl;
    return 1;
    }
  // we failed to throw an exception for the double swap (once it's implemented, this should return 0
  return 0;
  
}

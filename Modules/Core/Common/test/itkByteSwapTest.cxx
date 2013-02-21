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
#include "itkByteSwapper.h"

int itkByteSwapTest ( int, char*[] )
{
  // Test out the Byte Swap code

  std::cout << "Starting test" << std::endl;

  uint8_t uc = 'a', uc1 = 'a';
  uint16_t us = 1, us1 = 1;
  unsigned int ui = 1, ui1 = 1;
  unsigned long ul = 1, ul1 = 1;
  float f = 1.0, f1 = 1.0;
  double d = 1.0, d1 = 1.0;


  // Try to swap a char

  if ( itk::ByteSwapper<int>::SystemIsBigEndian() == itk::ByteSwapper<int>::SystemIsLE() )
    {
    return EXIT_FAILURE;
    }
  if ( itk::ByteSwapper<int>::SystemIsBE() == itk::ByteSwapper<int>::SystemIsLittleEndian() )
    {
    return EXIT_FAILURE;
    }

  if ( itk::ByteSwapper<int>::SystemIsBigEndian() )
    {
    itk::ByteSwapper<uint8_t>::SwapFromSystemToLittleEndian ( &uc );
    itk::ByteSwapper<uint8_t>::SwapFromSystemToLittleEndian ( &uc );
    }
  else
    {
    itk::ByteSwapper<uint8_t>::SwapFromSystemToBigEndian ( &uc );
    itk::ByteSwapper<uint8_t>::SwapFromSystemToBigEndian ( &uc );
    }
  if ( uc != uc1 )
    {
    return EXIT_FAILURE;
    }
  std::cout << "Passed uint8_t: " << uc << std::endl;

  if ( itk::ByteSwapper<int>::SystemIsBE() )
    {
    itk::ByteSwapper<uint16_t>::SwapFromSystemToLittleEndian ( &us );
    itk::ByteSwapper<uint16_t>::SwapFromSystemToLittleEndian ( &us );
    }
  else
    {
    itk::ByteSwapper<uint16_t>::SwapFromSystemToBigEndian ( &us );
    itk::ByteSwapper<uint16_t>::SwapFromSystemToBigEndian ( &us );
    }
  if ( us != us1 )
    {
    return EXIT_FAILURE;
    }
  std::cout << "Passed uint16_t: " << us << std::endl;

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
    return EXIT_FAILURE;
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
      return EXIT_FAILURE;
      }
    std::cout << "Passed unsigned long: " << ul << std::endl;
    }
  catch ( itk::ExceptionObject &err )
    {
    std::cout << "Caught unsigned long exception size is: " << sizeof ( unsigned long ) << std::endl;
    (&err)->Print(std::cerr);
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
      return EXIT_FAILURE;
      }
    std::cout << "Passed float: " << f << std::endl;
    }
  catch ( itk::ExceptionObject &err )
    {
    std::cout << "Caught float exception size is: " << sizeof ( float ) << std::endl;
    (&err)->Print(std::cerr);
    return EXIT_FAILURE;
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
      return EXIT_FAILURE;
      }
    std::cout << "Passed unsigned d: " << d << std::endl;
    }
  catch ( itk::ExceptionObject &err )
    {
    std::cout << "Good catch! Caught double exception size is: " << sizeof ( double ) << std::endl;
    (&err)->Print(std::cerr);
    return EXIT_FAILURE;
    }
  // we failed to throw an exception for the double swap (once it's implemented, this should return 0
  return EXIT_SUCCESS;

}

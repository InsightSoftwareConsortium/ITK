/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkByteSwapper.h"
#include "itkMath.h"
#include "itkGTest.h"

#include <iostream>

TEST(ByteSwap, EndianConsistency)
{
  // SystemIsBigEndian and SystemIsLE must be opposites
  EXPECT_NE(itk::ByteSwapper<int>::SystemIsBigEndian(), itk::ByteSwapper<int>::SystemIsLE());
  // SystemIsBE and SystemIsLittleEndian must be opposites
  EXPECT_NE(itk::ByteSwapper<int>::SystemIsBE(), itk::ByteSwapper<int>::SystemIsLittleEndian());
}

TEST(ByteSwap, DoubleSwapRestoresUnsignedChar)
{
  std::cout << "Starting test" << std::endl;
  unsigned char           uc = 'a';
  constexpr unsigned char uc1 = 'a';
  if constexpr (itk::ByteSwapper<int>::SystemIsBigEndian())
  {
    itk::ByteSwapper<unsigned char>::SwapFromSystemToLittleEndian(&uc);
    itk::ByteSwapper<unsigned char>::SwapFromSystemToLittleEndian(&uc);
  }
  else
  {
    itk::ByteSwapper<unsigned char>::SwapFromSystemToBigEndian(&uc);
    itk::ByteSwapper<unsigned char>::SwapFromSystemToBigEndian(&uc);
  }
  EXPECT_EQ(uc, uc1);
  std::cout << "Passed unsigned char: " << uc << std::endl;
}

TEST(ByteSwap, DoubleSwapRestoresUnsignedShort)
{
  unsigned short           us = 1;
  constexpr unsigned short us1{ 1 };
  if constexpr (itk::ByteSwapper<int>::SystemIsBE())
  {
    itk::ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&us);
    itk::ByteSwapper<unsigned short>::SwapFromSystemToLittleEndian(&us);
  }
  else
  {
    itk::ByteSwapper<unsigned short>::SwapFromSystemToBigEndian(&us);
    itk::ByteSwapper<unsigned short>::SwapFromSystemToBigEndian(&us);
  }
  EXPECT_EQ(us, us1);
  std::cout << "Passed unsigned short: " << us << std::endl;
}

TEST(ByteSwap, DoubleSwapRestoresUnsignedInt)
{
  unsigned int           ui = 1;
  constexpr unsigned int ui1{ 1 };
  if constexpr (itk::ByteSwapper<int>::SystemIsBigEndian())
  {
    itk::ByteSwapper<unsigned int>::SwapFromSystemToLittleEndian(&ui);
    itk::ByteSwapper<unsigned int>::SwapFromSystemToLittleEndian(&ui);
  }
  else
  {
    itk::ByteSwapper<unsigned int>::SwapFromSystemToBigEndian(&ui);
    itk::ByteSwapper<unsigned int>::SwapFromSystemToBigEndian(&ui);
  }
  EXPECT_EQ(ui, ui1);
  std::cout << "Passed unsigned int: " << ui << std::endl;
}

TEST(ByteSwap, DoubleSwapRestoresUnsignedLong)
{
  unsigned long           ul = 1;
  constexpr unsigned long ul1{ 1 };
  try
  {
    if constexpr (itk::ByteSwapper<long>::SystemIsBigEndian())
    {
      itk::ByteSwapper<unsigned long>::SwapFromSystemToLittleEndian(&ul);
      itk::ByteSwapper<unsigned long>::SwapFromSystemToLittleEndian(&ul);
    }
    else
    {
      itk::ByteSwapper<unsigned long>::SwapFromSystemToBigEndian(&ul);
      itk::ByteSwapper<unsigned long>::SwapFromSystemToBigEndian(&ul);
    }
    EXPECT_EQ(ul, ul1);
    std::cout << "Passed unsigned long: " << ul << std::endl;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught unsigned long exception size is: " << sizeof(unsigned long) << std::endl;
    err.Print(std::cerr);
  }
}

TEST(ByteSwap, DoubleSwapRestoresUnsignedLongLong)
{
  unsigned long long           ull = 1;
  constexpr unsigned long long ull1 = 1;
  try
  {
    if constexpr (itk::ByteSwapper<long>::SystemIsBigEndian())
    {
      itk::ByteSwapper<unsigned long long>::SwapFromSystemToLittleEndian(&ull);
      itk::ByteSwapper<unsigned long long>::SwapFromSystemToLittleEndian(&ull);
    }
    else
    {
      itk::ByteSwapper<unsigned long long>::SwapFromSystemToBigEndian(&ull);
      itk::ByteSwapper<unsigned long long>::SwapFromSystemToBigEndian(&ull);
    }
    EXPECT_EQ(ull, ull1);
    std::cout << "Passed unsigned long long: " << ull << std::endl;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught unsigned long long exception size is: " << sizeof(unsigned long long) << std::endl;
    err.Print(std::cerr);
  }
}

TEST(ByteSwap, DoubleSwapRestoresFloat)
{
  float           f = 1.0;
  constexpr float f1{ 1.0 };
  try
  {
    if constexpr (itk::ByteSwapper<int>::SystemIsBigEndian())
    {
      itk::ByteSwapper<float>::SwapFromSystemToLittleEndian(&f);
      itk::ByteSwapper<float>::SwapFromSystemToLittleEndian(&f);
    }
    else
    {
      itk::ByteSwapper<float>::SwapFromSystemToBigEndian(&f);
      itk::ByteSwapper<float>::SwapFromSystemToBigEndian(&f);
    }
    EXPECT_TRUE(itk::Math::ExactlyEquals(f, f1));
    std::cout << "Passed float: " << f << std::endl;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught float exception size is: " << sizeof(float) << std::endl;
    err.Print(std::cerr);
    FAIL() << "Unexpected exception for float swap";
  }
}

TEST(ByteSwap, DoubleSwapRestoresDouble)
{
  double           d = 1.0;
  constexpr double d1{ 1.0 };
  try
  {
    if constexpr (itk::ByteSwapper<int>::SystemIsBigEndian())
    {
      itk::ByteSwapper<double>::SwapFromSystemToLittleEndian(&d);
      itk::ByteSwapper<double>::SwapFromSystemToLittleEndian(&d);
    }
    else
    {
      itk::ByteSwapper<double>::SwapFromSystemToBigEndian(&d);
      itk::ByteSwapper<double>::SwapFromSystemToBigEndian(&d);
    }
    EXPECT_TRUE(itk::Math::ExactlyEquals(d, d1));
    std::cout << "Passed unsigned int d: " << d << std::endl;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Good catch! Caught double exception size is: " << sizeof(double) << std::endl;
    err.Print(std::cerr);
    FAIL() << "Unexpected exception for double swap";
  }
}

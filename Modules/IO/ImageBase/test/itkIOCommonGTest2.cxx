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

#include "itkIOCommon.h"
#include "itkGTest.h"

TEST(IOCommon2, ConvertedLegacyTest)
{
  // Test the atomic pixel type to string conversions
  {
    EXPECT_EQ(itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::AtomicPixelEnum::ITK_UCHAR), "unsigned char");
    EXPECT_EQ(itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::AtomicPixelEnum::ITK_CHAR), "char");
    EXPECT_EQ(itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::AtomicPixelEnum::ITK_USHORT), "unsigned short");
    EXPECT_EQ(itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::AtomicPixelEnum::ITK_SHORT), "short");
    EXPECT_EQ(itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::AtomicPixelEnum::ITK_UINT), "unsigned int");
    EXPECT_EQ(itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::AtomicPixelEnum::ITK_INT), "int");
    EXPECT_EQ(itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::AtomicPixelEnum::ITK_ULONG), "unsigned long");
    EXPECT_EQ(itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::AtomicPixelEnum::ITK_LONG), "long");
    EXPECT_EQ(itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::AtomicPixelEnum::ITK_FLOAT), "float");
    EXPECT_EQ(itk::IOCommon::AtomicPixelTypeToString(itk::IOCommon::AtomicPixelEnum::ITK_DOUBLE), "double");
  }

  // Test the atomic pixel type size computation
  {
    EXPECT_EQ(itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::AtomicPixelEnum::ITK_UCHAR),
              static_cast<unsigned int>(sizeof(unsigned char)));
    EXPECT_EQ(itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::AtomicPixelEnum::ITK_CHAR),
              static_cast<unsigned int>(sizeof(unsigned char)));
    EXPECT_EQ(itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::AtomicPixelEnum::ITK_USHORT),
              static_cast<unsigned int>(sizeof(unsigned short)));
    EXPECT_EQ(itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::AtomicPixelEnum::ITK_SHORT),
              static_cast<unsigned int>(sizeof(short)));
    EXPECT_EQ(itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::AtomicPixelEnum::ITK_UINT),
              static_cast<unsigned int>(sizeof(unsigned int)));
    EXPECT_EQ(itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::AtomicPixelEnum::ITK_INT),
              static_cast<unsigned int>(sizeof(int)));
    EXPECT_EQ(itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::AtomicPixelEnum::ITK_ULONG),
              static_cast<unsigned int>(sizeof(unsigned long)));
    EXPECT_EQ(itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::AtomicPixelEnum::ITK_LONG),
              static_cast<unsigned int>(sizeof(long)));
    EXPECT_EQ(itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::AtomicPixelEnum::ITK_FLOAT),
              static_cast<unsigned int>(sizeof(float)));
    EXPECT_EQ(itk::IOCommon::ComputeSizeOfAtomicPixelType(itk::IOCommon::AtomicPixelEnum::ITK_DOUBLE),
              static_cast<unsigned int>(sizeof(double)));
  }
}

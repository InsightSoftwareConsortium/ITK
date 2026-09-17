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

#include "vnl/vnl_matlab_read.h"
#include "vnl/vnl_matlab_header.h"

#include <gtest/gtest.h>

#include <array>
#include <cstring>
#include <sstream>

namespace
{
// The writer emits the header in native byte order, so any legal type code read
// back verbatim means the file is same-endian as the reader.
std::stringstream
MakeNativeStream(const vxl_int_32 typeCode, const std::array<float, 4> & values)
{
  vnl_matlab_header hdr;
  std::memset(&hdr, 0, sizeof(hdr));
  hdr.type = typeCode;
  hdr.rows = 1;
  hdr.cols = static_cast<vxl_int_32>(values.size());
  hdr.imag = 0;
  hdr.namlen = 2;

  std::stringstream ss;
  ss.write(reinterpret_cast<const char *>(&hdr), sizeof(hdr));
  ss.write("x", 2);
  ss.write(reinterpret_cast<const char *>(values.data()), sizeof(float) * values.size());
  return ss;
}
} // namespace

// All 8 legal single-precision/double-precision code combinations of
// { little/big endian, column/row wise } must be recognized as native.
TEST(VnlMatlabRead, NativeSinglePrecisionCodesAreNotSwapped)
{
  const std::array<float, 4> expected{ 1.5F, -2.25F, 3.0F, 0.125F };

  for (const vxl_int_32 typeCode : { 10, 110, 1010, 1110 })
  {
    std::stringstream  ss = MakeNativeStream(typeCode, expected);
    vnl_matlab_readhdr reader(ss);

    ASSERT_TRUE(static_cast<bool>(reader)) << "type code " << typeCode;
    EXPECT_TRUE(reader.is_single()) << "type code " << typeCode;
    EXPECT_EQ(reader.rows(), 1) << "type code " << typeCode;
    EXPECT_EQ(reader.cols(), 4) << "type code " << typeCode;
    EXPECT_STREQ(reader.name(), "x") << "type code " << typeCode;

    std::array<float, 4> actual{};
    ASSERT_TRUE(reader.read_data(actual.data())) << "type code " << typeCode;
    EXPECT_EQ(actual, expected) << "type code " << typeCode;
  }
}

// A foreign-endian header must still be detected and byte-swapped.
TEST(VnlMatlabRead, ForeignEndianHeaderIsSwapped)
{
  const std::array<float, 4> expected{ 1.5F, -2.25F, 3.0F, 0.125F };

  std::stringstream native = MakeNativeStream(1010, expected);
  std::string       bytes = native.str();

  for (const size_t offset : { size_t{ 0 }, size_t{ 4 }, size_t{ 8 }, size_t{ 12 }, size_t{ 16 } })
  {
    byteswap::swap32(&bytes[offset]);
  }
  const size_t dataOffset = sizeof(vnl_matlab_header) + 2;
  for (size_t offset = dataOffset; offset < bytes.size(); offset += 4)
  {
    byteswap::swap32(&bytes[offset]);
  }

  std::stringstream  ss(bytes);
  vnl_matlab_readhdr reader(ss);

  ASSERT_TRUE(static_cast<bool>(reader));
  EXPECT_TRUE(reader.is_single());
  EXPECT_EQ(reader.rows(), 1);
  EXPECT_EQ(reader.cols(), 4);

  std::array<float, 4> actual{};
  ASSERT_TRUE(reader.read_data(actual.data()));
  EXPECT_EQ(actual, expected);
}

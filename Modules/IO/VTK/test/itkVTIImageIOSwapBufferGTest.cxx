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

// Unit tests for VTIImageIO::SwapBufferForByteOrder.
//
// The old VTIImageIO byte-swap helper hard-coded
// ByteSwapper::SwapRangeFromSystemToBigEndian, which is a no-op when the
// host is big-endian reading a little-endian file -- silently leaving
// file bytes un-swapped.  SwapBufferForByteOrder replaces it with an
// unconditional std::reverse-within-component, so all four
// (fileByteOrder, targetByteOrder) combinations produce correct output
// regardless of the host's native endianness.  These tests exercise
// every combination without needing a big-endian CI runner.

#include "itkVTIImageIO.h"
#include "itkGTest.h"

#include <algorithm>
#include <array>
#include <cstdint>
#include <cstring>
#include <vector>

namespace
{
constexpr itk::IOByteOrderEnum LE = itk::IOByteOrderEnum::LittleEndian;
constexpr itk::IOByteOrderEnum BE = itk::IOByteOrderEnum::BigEndian;

template <typename T>
std::vector<unsigned char>
AsBytes(std::initializer_list<T> values)
{
  std::vector<unsigned char> out;
  out.reserve(values.size() * sizeof(T));
  for (T v : values)
  {
    const auto * p = reinterpret_cast<const unsigned char *>(&v);
    for (std::size_t i = 0; i < sizeof(T); ++i)
    {
      out.push_back(p[i]);
    }
  }
  return out;
}

void
RunCase(std::size_t                        componentSize,
        std::size_t                        numComponents,
        const std::vector<unsigned char> & input,
        const std::vector<unsigned char> & expected,
        itk::IOByteOrderEnum               fileByteOrder,
        itk::IOByteOrderEnum               targetByteOrder)
{
  std::vector<unsigned char> buffer = input;
  itk::VTIImageIO::SwapBufferForByteOrder(buffer.data(), componentSize, numComponents, fileByteOrder, targetByteOrder);
  EXPECT_EQ(buffer, expected);
}
} // namespace

// componentSize == 1: always a no-op regardless of order args.
TEST(VTIImageIOSwapBuffer, ComponentSize1IsNoOp)
{
  const std::vector<unsigned char> in{ 0x01, 0x02, 0x03, 0x04 };
  RunCase(1, 4, in, in, LE, LE);
  RunCase(1, 4, in, in, BE, BE);
  RunCase(1, 4, in, in, LE, BE);
  RunCase(1, 4, in, in, BE, LE);
}

// componentSize == 2: swap each pair when orders differ.
TEST(VTIImageIOSwapBuffer, ComponentSize2)
{
  const std::vector<unsigned char> in{ 0x01, 0x02, 0x03, 0x04, 0xAB, 0xCD };
  const std::vector<unsigned char> swapped{ 0x02, 0x01, 0x04, 0x03, 0xCD, 0xAB };
  RunCase(2, 3, in, in, LE, LE);
  RunCase(2, 3, in, in, BE, BE);
  RunCase(2, 3, in, swapped, LE, BE);
  RunCase(2, 3, in, swapped, BE, LE);
}

// componentSize == 4: reverse each 4-byte group when differing.
TEST(VTIImageIOSwapBuffer, ComponentSize4)
{
  const std::vector<unsigned char> in{ 0x11, 0x22, 0x33, 0x44, 0xDE, 0xAD, 0xBE, 0xEF };
  const std::vector<unsigned char> swapped{ 0x44, 0x33, 0x22, 0x11, 0xEF, 0xBE, 0xAD, 0xDE };
  RunCase(4, 2, in, in, LE, LE);
  RunCase(4, 2, in, in, BE, BE);
  RunCase(4, 2, in, swapped, LE, BE);
  RunCase(4, 2, in, swapped, BE, LE);
}

// componentSize == 8: reverse each 8-byte group when differing.
TEST(VTIImageIOSwapBuffer, ComponentSize8)
{
  const std::vector<unsigned char> in{ 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
                                       0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88 };
  const std::vector<unsigned char> swapped{ 0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01,
                                            0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11 };
  RunCase(8, 2, in, in, LE, LE);
  RunCase(8, 2, in, in, BE, BE);
  RunCase(8, 2, in, swapped, LE, BE);
  RunCase(8, 2, in, swapped, BE, LE);
}

// regression: known-value uint32 reinterpret via LE->LE vs LE->BE.
TEST(VTIImageIOSwapBuffer, KnownValueRegression)
{
  const std::vector<unsigned char> in = AsBytes<std::uint32_t>({ 0x01020304, 0xAABBCCDD });
  std::vector<unsigned char>       swapped = in;
  for (std::size_t p = 0; p < 2; ++p)
  {
    std::reverse(swapped.begin() + p * 4, swapped.begin() + p * 4 + 4);
  }
  RunCase(4, 2, in, swapped, LE, BE);
  RunCase(4, 2, in, swapped, BE, LE);
}

// zero-element buffer is a no-op (must not dereference the null pointer).
TEST(VTIImageIOSwapBuffer, ZeroComponentBufferIsNoOp)
{
  itk::VTIImageIO::SwapBufferForByteOrder(nullptr, 4, 0, LE, BE);
  SUCCEED();
}

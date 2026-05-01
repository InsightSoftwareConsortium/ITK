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

#include "itkTestingMacros.h"
#include "itkVTIImageIO.h"

#include <array>
#include <cstdint>
#include <cstring>
#include <vector>

namespace
{
constexpr itk::IOByteOrderEnum LE = itk::IOByteOrderEnum::LittleEndian;
constexpr itk::IOByteOrderEnum BE = itk::IOByteOrderEnum::BigEndian;

// Compare two byte vectors; print a diff and return false on mismatch.
bool
BytesEqual(const std::vector<unsigned char> & got, const std::vector<unsigned char> & want, const char * where)
{
  if (got.size() != want.size())
  {
    std::cerr << where << ": size mismatch got=" << got.size() << " want=" << want.size() << std::endl;
    return false;
  }
  for (std::size_t i = 0; i < got.size(); ++i)
  {
    if (got[i] != want[i])
    {
      std::cerr << where << ": byte " << i << " got=0x" << std::hex << static_cast<int>(got[i]) << " want=0x"
                << static_cast<int>(want[i]) << std::dec << std::endl;
      return false;
    }
  }
  return true;
}

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

int
RunCase(const char *                       label,
        std::size_t                        componentSize,
        std::size_t                        numComponents,
        const std::vector<unsigned char> & input,
        const std::vector<unsigned char> & expected,
        itk::IOByteOrderEnum               fileByteOrder,
        itk::IOByteOrderEnum               targetByteOrder)
{
  std::vector<unsigned char> buffer = input;
  itk::VTIImageIO::SwapBufferForByteOrder(buffer.data(), componentSize, numComponents, fileByteOrder, targetByteOrder);
  return BytesEqual(buffer, expected, label) ? EXIT_SUCCESS : EXIT_FAILURE;
}
} // namespace

int
itkVTIImageIOSwapBufferTest(int, char *[])
{
  int status = EXIT_SUCCESS;

  // ---- componentSize == 1: always a no-op regardless of order args ----
  {
    const std::vector<unsigned char> in{ 0x01, 0x02, 0x03, 0x04 };
    status |= RunCase("u8 LE->LE", 1, 4, in, in, LE, LE);
    status |= RunCase("u8 BE->BE", 1, 4, in, in, BE, BE);
    status |= RunCase("u8 LE->BE", 1, 4, in, in, LE, BE);
    status |= RunCase("u8 BE->LE", 1, 4, in, in, BE, LE);
  }

  // ---- componentSize == 2: swap each pair when orders differ ----
  {
    const std::vector<unsigned char> in{ 0x01, 0x02, 0x03, 0x04, 0xAB, 0xCD };
    const std::vector<unsigned char> swapped{ 0x02, 0x01, 0x04, 0x03, 0xCD, 0xAB };
    status |= RunCase("u16 LE->LE", 2, 3, in, in, LE, LE);
    status |= RunCase("u16 BE->BE", 2, 3, in, in, BE, BE);
    status |= RunCase("u16 LE->BE", 2, 3, in, swapped, LE, BE);
    status |= RunCase("u16 BE->LE", 2, 3, in, swapped, BE, LE);
  }

  // ---- componentSize == 4: reverse each 4-byte group when differing ----
  {
    const std::vector<unsigned char> in{ 0x11, 0x22, 0x33, 0x44, 0xDE, 0xAD, 0xBE, 0xEF };
    const std::vector<unsigned char> swapped{ 0x44, 0x33, 0x22, 0x11, 0xEF, 0xBE, 0xAD, 0xDE };
    status |= RunCase("u32 LE->LE", 4, 2, in, in, LE, LE);
    status |= RunCase("u32 BE->BE", 4, 2, in, in, BE, BE);
    status |= RunCase("u32 LE->BE", 4, 2, in, swapped, LE, BE);
    status |= RunCase("u32 BE->LE", 4, 2, in, swapped, BE, LE);
  }

  // ---- componentSize == 8: reverse each 8-byte group when differing ----
  {
    const std::vector<unsigned char> in{ 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
                                         0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88 };
    const std::vector<unsigned char> swapped{ 0x08, 0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01,
                                              0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11 };
    status |= RunCase("u64 LE->LE", 8, 2, in, in, LE, LE);
    status |= RunCase("u64 BE->BE", 8, 2, in, in, BE, BE);
    status |= RunCase("u64 LE->BE", 8, 2, in, swapped, LE, BE);
    status |= RunCase("u64 BE->LE", 8, 2, in, swapped, BE, LE);
  }

  // ---- regression: known-value float reinterpret via LE->LE vs LE->BE ----
  // Use a 2-element float32 buffer with a known pattern that the caller
  // can interpret.  Verifies the underlying reverse is byte-accurate.
  {
    const std::vector<unsigned char> in = AsBytes<std::uint32_t>({ 0x01020304, 0xAABBCCDD });
    std::vector<unsigned char>       swapped = in;
    for (std::size_t p = 0; p < 2; ++p)
    {
      std::reverse(swapped.begin() + p * 4, swapped.begin() + p * 4 + 4);
    }
    status |= RunCase("regression LE->BE", 4, 2, in, swapped, LE, BE);
    status |= RunCase("regression BE->LE", 4, 2, in, swapped, BE, LE);
  }

  // ---- zero-element buffer is a no-op ----
  {
    std::vector<unsigned char> empty;
    itk::VTIImageIO::SwapBufferForByteOrder(nullptr, 4, 0, LE, BE);
    (void)empty;
    std::cout << "  zero-component swap: no crash" << std::endl;
  }

  if (status == EXIT_SUCCESS)
  {
    std::cout << "itkVTIImageIOSwapBufferTest PASSED" << std::endl;
  }
  return status;
}

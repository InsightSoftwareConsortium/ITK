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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkByteSwapper_hxx
#define itkByteSwapper_hxx
#include "itkMakeUniqueForOverwrite.h"
#include <algorithm> // For for_each_n and swap.
#include <cstddef>   // For byte.
#include <memory>
#include <cstring>

namespace itk
{
// The following are the public methods --------------------------------

//------Big Endian methods----------------------------------------------

// Use different swap methods based on type

template <typename T>
void
ByteSwapper<T>::SwapFromSystemToBigEndian([[maybe_unused]] T * p)
{
  if constexpr (!m_SystemIsBigEndian && sizeof(T) > 1)
  {
    SwapBytes(*p);
  }
}

template <typename T>
void
ByteSwapper<T>::SwapRangeFromSystemToBigEndian([[maybe_unused]] T * p, [[maybe_unused]] BufferSizeType num)
{
  if constexpr (!m_SystemIsBigEndian && sizeof(T) > 1)
  {
    std::for_each_n(p, num, [](T & element) { SwapBytes(element); });
  }
}

template <typename T>
void
ByteSwapper<T>::SwapWriteRangeFromSystemToBigEndian(const T * p, int num, std::ostream * fp)
{
  if constexpr (m_SystemIsBigEndian || sizeof(T) == 1)
  {
    num *= sizeof(T);
    fp->write(reinterpret_cast<const char *>(p), num);
  }
  else
  {
    SwapWriteRange(p, num, *fp);
  }
}


//------Little Endian methods----------------------------------------------

template <typename T>
void
ByteSwapper<T>::SwapFromSystemToLittleEndian([[maybe_unused]] T * p)
{
  if constexpr (m_SystemIsBigEndian && sizeof(T) > 1)
  {
    SwapBytes(*p);
  }
}

template <typename T>
void
ByteSwapper<T>::SwapRangeFromSystemToLittleEndian([[maybe_unused]] T * p, [[maybe_unused]] BufferSizeType num)
{
  if constexpr (m_SystemIsBigEndian && sizeof(T) > 1)
  {
    std::for_each_n(p, num, [](T & element) { SwapBytes(element); });
  }
}

template <typename T>
void
ByteSwapper<T>::SwapWriteRangeFromSystemToLittleEndian(const T * p, int num, std::ostream * fp)
{
  if constexpr (m_SystemIsBigEndian && sizeof(T) > 1)
  {
    SwapWriteRange(p, num, *fp);
  }
  else
  {
    num *= sizeof(T);
    fp->write(reinterpret_cast<const char *>(p), num);
  }
}


// The following member functions are private:

template <typename T>
void
ByteSwapper<T>::SwapBytes(T & value)
{
  static constexpr size_t numberOfBytes = sizeof(T);

  // Historically (from ITK v1.2.0, March 2003) the following number of bytes are supported:
  if constexpr (numberOfBytes == 2 || numberOfBytes == 4 || numberOfBytes == 8)
  {
    // When the value is an integer, the following code is equivalent to `value = std::byteswap(value)`, in C++26:
    auto * const bytes = reinterpret_cast<std::byte *>(&value);

    for (size_t i{}; i < (numberOfBytes / 2); ++i)
    {
      std::swap(bytes[i], bytes[(numberOfBytes - 1) - i]);
    }
  }
  else
  {
    itkGenericExceptionMacro("Cannot swap number of bytes requested");
  }
}

template <typename T>
void
ByteSwapper<T>::SwapWriteRange(const T * buffer, SizeValueType numberOfElements, std::ostream & outputStream)
{
  auto chunkSize = std::min(numberOfElements, SizeValueType{ 1000000 });

  const auto chunk = make_unique_for_overwrite<T[]>(chunkSize);

  while (numberOfElements > 0)
  {
    std::copy_n(buffer, chunkSize, chunk.get());
    std::for_each_n(chunk.get(), chunkSize, [](T & element) { SwapBytes(element); });

    outputStream.write(reinterpret_cast<const char *>(chunk.get()),
                       static_cast<std::streamsize>(chunkSize * sizeof(T)));
    buffer += chunkSize;
    numberOfElements -= chunkSize;
    chunkSize = std::min(numberOfElements, chunkSize);
  }
}

} // end namespace itk

#endif

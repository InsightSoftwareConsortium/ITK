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
#include <algorithm> // For swap.
#include <memory>
#include <cstring>

namespace itk
{
// The following are the public methods --------------------------------
//
// Machine definitions
#ifdef CMAKE_WORDS_BIGENDIAN
template <typename T>
bool
ByteSwapper<T>::SystemIsBigEndian()
{
  return true;
}
template <typename T>
bool
ByteSwapper<T>::SystemIsLittleEndian()
{
  return false;
}
#else
template <typename T>
bool
ByteSwapper<T>::SystemIsBigEndian()
{
  return false;
}
template <typename T>
bool
ByteSwapper<T>::SystemIsLittleEndian()
{
  return true;
}
#endif

//------Big Endian methods----------------------------------------------

// Use different swap methods based on type
#ifdef CMAKE_WORDS_BIGENDIAN
template <typename T>
void
ByteSwapper<T>::SwapFromSystemToBigEndian(T *)
{}
#else
template <typename T>
void
ByteSwapper<T>::SwapFromSystemToBigEndian(T * p)
{
  switch (sizeof(T))
  {
    case 1:
      return;
    case 2:
      Self::Swap2(p);
      return;
    case 4:
      Self::Swap4(p);
      return;
    case 8:
      Self::Swap8(p);
      return;
    default:
      itkGenericExceptionMacro(<< "Cannot swap number of bytes requested");
  }
}

#endif

#ifdef CMAKE_WORDS_BIGENDIAN
template <typename T>
void
ByteSwapper<T>::SwapRangeFromSystemToBigEndian(T *, BufferSizeType)
{
  // nothing needs to be done here...
}

#else

#  ifdef __INTEL_COMPILER
#    pragma warning disable 280 // remark #280: selector expression is constant
#  endif
template <typename T>
void
ByteSwapper<T>::SwapRangeFromSystemToBigEndian(T * p, BufferSizeType num)
{
  switch (sizeof(T))
  {
    case 1:
      return;
    case 2:
      Self::Swap2Range(p, num);
      return;
    case 4:
      Self::Swap4Range(p, num);
      return;
    case 8:
      Self::Swap8Range(p, num);
      return;
    default:
      itkGenericExceptionMacro(<< "Cannot swap number of bytes requested");
  }
}

#endif

#ifdef CMAKE_WORDS_BIGENDIAN
template <typename T>
void
ByteSwapper<T>::SwapWriteRangeFromSystemToBigEndian(const T * p, int num, OStreamType * fp)
{
  num *= sizeof(T);
  fp->write(reinterpret_cast<const char *>(p), num);
}

#else
template <typename T>
void
ByteSwapper<T>::SwapWriteRangeFromSystemToBigEndian(const T * p, int num, OStreamType * fp)
{
  switch (sizeof(T))
  {
    case 1:
      return;
    case 2:
      Self::SwapWrite2Range(p, num, fp);
      return;
    case 4:
      Self::SwapWrite4Range(p, num, fp);
      return;
    case 8:
      Self::SwapWrite8Range(p, num, fp);
      return;
    default:
      itkGenericExceptionMacro(<< "Cannot swap number of bytes requested");
  }
}

#endif

//------Little Endian methods----------------------------------------------

#ifdef CMAKE_WORDS_BIGENDIAN
template <typename T>
void
ByteSwapper<T>::SwapFromSystemToLittleEndian(T * p)
{
  switch (sizeof(T))
  {
    case 1:
      return;
    case 2:
      Self::Swap2(p);
      return;
    case 4:
      Self::Swap4(p);
      return;
    case 8:
      Self::Swap8(p);
      return;
    default:
      itkGenericExceptionMacro(<< "Cannot swap number of bytes requested");
  }
}

#else
template <typename T>
void
ByteSwapper<T>::SwapFromSystemToLittleEndian(T *)
{}
#endif

#ifdef CMAKE_WORDS_BIGENDIAN
template <typename T>
void
ByteSwapper<T>::SwapRangeFromSystemToLittleEndian(T * p, BufferSizeType num)
{
  switch (sizeof(T))
  {
    case 1:
      return;
    case 2:
      Self::Swap2Range(p, num);
      return;
    case 4:
      Self::Swap4Range(p, num);
      return;
    case 8:
      Self::Swap8Range(p, num);
      return;
    default:
      itkGenericExceptionMacro(<< "Cannot swap number of bytes requested");
  }
}

#else
template <typename T>
void
ByteSwapper<T>::SwapRangeFromSystemToLittleEndian(T *, BufferSizeType)
{}
#endif

#ifdef CMAKE_WORDS_BIGENDIAN
template <typename T>
void
ByteSwapper<T>::SwapWriteRangeFromSystemToLittleEndian(const T * p, int num, OStreamType * fp)
{
  switch (sizeof(T))
  {
    case 1:
      return;
    case 2:
      Self::SwapWrite2Range(p, num, fp);
      return;
    case 4:
      Self::SwapWrite4Range(p, num, fp);
      return;
    case 8:
      Self::SwapWrite8Range(p, num, fp);
      return;
    default:
      itkGenericExceptionMacro(<< "Cannot swap number of bytes requested");
  }
}

#else
template <typename T>
void
ByteSwapper<T>::SwapWriteRangeFromSystemToLittleEndian(const T * p, int num, OStreamType * fp)
{
  num *= sizeof(T);
  fp->write(reinterpret_cast<const char *>(p), num);
}

#endif

// The following are the protected methods -------------------------
//

//------2-byte methods----------------------------------------------

// Swap 2 byte word.
template <typename T>
void
ByteSwapper<T>::Swap2(void * pin)
{
  auto * p = static_cast<char *>(pin);
  std::swap(p[0], p[1]);
}

// Swap bunch of bytes. Num is the number of two byte words to swap.
template <typename T>
void
ByteSwapper<T>::Swap2Range(void * ptr, BufferSizeType num)
{
  auto * pos = static_cast<char *>(ptr);
  for (BufferSizeType i = 0; i < num; ++i)
  {
    Self::Swap2(pos);
    pos = pos + 2;
  }
}

// Swap bunch of bytes. Num is the number of four byte words to swap.
template <typename T>
void
ByteSwapper<T>::SwapWrite2Range(const void * ptr, BufferSizeType num, OStreamType * fp)
{
  BufferSizeType chunkSize = 1000000;
  if (num < chunkSize)
  {
    chunkSize = num;
  }
  const auto cpy = make_unique_for_overwrite<char[]>(chunkSize * 2);
  while (num)
  {
    memcpy(cpy.get(), ptr, chunkSize * 2);

    Self::Swap2Range(cpy.get(), num);

    fp->write(cpy.get(), static_cast<std::streamsize>(2 * chunkSize));
    ptr = static_cast<const char *>(ptr) + chunkSize * 2;
    num -= chunkSize;
    if (num < chunkSize)
    {
      chunkSize = num;
    }
  }
}

//------4-byte methods----------------------------------------------

// Swap four byte word.
template <typename T>
void
ByteSwapper<T>::Swap4(void * ptr)
{
  auto * p = static_cast<char *>(ptr);
  std::swap(p[0], p[3]);
  std::swap(p[1], p[2]);
}

// Swap bunch of bytes. Num is the number of four byte words to swap.
template <typename T>
void
ByteSwapper<T>::Swap4Range(void * ptr, BufferSizeType num)
{
  auto * pos = static_cast<char *>(ptr);

  for (BufferSizeType i = 0; i < num; ++i)
  {
    Self::Swap4(pos);
    pos = pos + 4;
  }
}

// Swap bunch of bytes. Num is the number of four byte words to swap.
template <typename T>
void
ByteSwapper<T>::SwapWrite4Range(const void * ptr, BufferSizeType num, OStreamType * fp)
{
  BufferSizeType chunkSize = 1000000;

  if (num < chunkSize)
  {
    chunkSize = num;
  }
  const auto cpy = make_unique_for_overwrite<char[]>(chunkSize * 4);

  while (num)
  {
    memcpy(cpy.get(), ptr, chunkSize * 4);

    Self::Swap4Range(cpy.get(), num);

    fp->write(cpy.get(), static_cast<std::streamsize>(4 * chunkSize));
    ptr = static_cast<const char *>(ptr) + chunkSize * 4;
    num -= chunkSize;
    if (num < chunkSize)
    {
      chunkSize = num;
    }
  }
}

//------8-byte methods----------------------------------------------

// Swap 8 byte double precision
template <typename T>
void
ByteSwapper<T>::Swap8(void * ptr)
{
  auto * p = static_cast<char *>(ptr);

  std::swap(p[0], p[7]);
  std::swap(p[1], p[6]);
  std::swap(p[2], p[5]);
  std::swap(p[3], p[4]);
}

// Swap bunch of bytes. Num is the number of eight byte words to swap.
template <typename T>
void
ByteSwapper<T>::Swap8Range(void * ptr, BufferSizeType num)
{
  auto * pos = static_cast<char *>(ptr);

  for (BufferSizeType i = 0; i < num; ++i)
  {
    Self::Swap8(pos);
    pos = pos + 8;
  }
}

// Swap bunch of bytes. Num is the number of four byte words to swap.
template <typename T>
void
ByteSwapper<T>::SwapWrite8Range(const void * ptr, BufferSizeType num, OStreamType * fp)
{
  BufferSizeType chunkSize = 1000000;
  if (num < chunkSize)
  {
    chunkSize = num;
  }
  const auto cpy = make_unique_for_overwrite<char[]>(chunkSize * 8);

  while (num)
  {
    memcpy(cpy.get(), ptr, chunkSize * 8);

    Self::Swap8Range(cpy.get(), chunkSize);

    fp->write(cpy.get(), static_cast<std::streamsize>(8 * chunkSize));
    ptr = static_cast<const char *>(ptr) + chunkSize * 8;
    num -= chunkSize;
    if (num < chunkSize)
    {
      chunkSize = num;
    }
  }
}
} // end namespace itk

#endif

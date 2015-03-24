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
#include "itkByteSwapper.h"
#include <memory>
#include <cstring>

namespace itk
{
// The following are the public methods --------------------------------
//
// Machine definitions
#ifdef CMAKE_WORDS_BIGENDIAN
template< typename T >
bool ByteSwapper< T >::SystemIsBigEndian() { return true; }
template< typename T >
bool ByteSwapper< T >::SystemIsLittleEndian() { return false; }
#else
template< typename T >
bool ByteSwapper< T >::SystemIsBigEndian() { return false; }
template< typename T >
bool ByteSwapper< T >::SystemIsLittleEndian() { return true; }
#endif

//------Big Endian methods----------------------------------------------

// Use different swap methods based on type
#ifdef CMAKE_WORDS_BIGENDIAN
template< typename T >
void
ByteSwapper< T >
::SwapFromSystemToBigEndian(T *){}
#else
template< typename T >
void
ByteSwapper< T >
::SwapFromSystemToBigEndian(T *p)
{
  switch ( sizeof( T ) )
    {
    case 1:
      return;
    case 2:
      ByteSwapper< T >::Swap2( (void *)p );
      return;
    case 4:
      ByteSwapper< T >::Swap4( (void *)p );
      return;
    case 8:
      ByteSwapper< T >::Swap8( (void *)p );
      return;
    default:
      itkGenericExceptionMacro (<< "Cannot swap number of bytes requested");
    }
}

#endif

#ifdef CMAKE_WORDS_BIGENDIAN
template< typename T >
void
ByteSwapper< T >
::SwapRangeFromSystemToBigEndian(T *, BufferSizeType)
{
  // nothing needs to be done here...
}

#else

#ifdef __INTEL_COMPILER
#pragma warning disable 280 //remark #280: selector expression is constant
#endif
template< typename T >
void
ByteSwapper< T >
::SwapRangeFromSystemToBigEndian(T *p, BufferSizeType num)
{
  switch ( sizeof( T ) )
    {
    case 1:
      return;
    case 2:
      ByteSwapper< T >::Swap2Range( (void *)p, num );
      return;
    case 4:
      ByteSwapper< T >::Swap4Range( (void *)p, num );
      return;
    case 8:
      ByteSwapper< T >::Swap8Range( (void *)p, num );
      return;
    default:
      itkGenericExceptionMacro (<< "Cannot swap number of bytes requested");
      return;
    }
}

#endif

#ifdef CMAKE_WORDS_BIGENDIAN
template< typename T >
void
ByteSwapper< T >
::SwapWriteRangeFromSystemToBigEndian(T *p, int num, OStreamType *fp)
{
  num *= sizeof( T );
  fp->write( (char *)p, num );
}

#else
template< typename T >
void
ByteSwapper< T >
::SwapWriteRangeFromSystemToBigEndian(T *p, int num, OStreamType *fp)
{
  switch ( sizeof( T ) )
    {
    case 1:
      return;
    case 2:
      ByteSwapper< T >::SwapWrite2Range( (void *)p, num, fp );
      return;
    case 4:
      ByteSwapper< T >::SwapWrite4Range( (void *)p, num, fp );
      return;
    case 8:
      ByteSwapper< T >::SwapWrite8Range( (void *)p, num, fp );
      return;
    default:
      itkGenericExceptionMacro (<< "Cannot swap number of bytes requested");
    }
}

#endif

//------Little Endian methods----------------------------------------------

#ifdef CMAKE_WORDS_BIGENDIAN
template< typename T >
void
ByteSwapper< T >
::SwapFromSystemToLittleEndian(T *p)
{
  switch ( sizeof( T ) )
    {
    case 1:
      return;
    case 2:
      ByteSwapper< T >::Swap2( (void *)p );
      return;
    case 4:
      ByteSwapper< T >::Swap4( (void *)p );
      return;
    case 8:
      ByteSwapper< T >::Swap8( (void *)p );
      return;
    default:
      itkGenericExceptionMacro (<< "Cannot swap number of bytes requested");
    }
}

#else
template< typename T >
void
ByteSwapper< T >
::SwapFromSystemToLittleEndian(T *){}
#endif

#ifdef CMAKE_WORDS_BIGENDIAN
template< typename T >
void
ByteSwapper< T >
::SwapRangeFromSystemToLittleEndian(T *p, BufferSizeType num)
{
  switch ( sizeof( T ) )
    {
    case 1:
      return;
    case 2:
      ByteSwapper< T >::Swap2Range( (void *)p, num );
      return;
    case 4:
      ByteSwapper< T >::Swap4Range( (void *)p, num );
      return;
    case 8:
      ByteSwapper< T >::Swap8Range( (void *)p, num );
      return;
    default:
      itkGenericExceptionMacro (<< "Cannot swap number of bytes requested");
    }
}

#else
template< typename T >
void
ByteSwapper< T >
::SwapRangeFromSystemToLittleEndian(T *, BufferSizeType) {}
#endif

#ifdef CMAKE_WORDS_BIGENDIAN
template< typename T >
void
ByteSwapper< T >
::SwapWriteRangeFromSystemToLittleEndian(T *p, int num, OStreamType *fp)
{
  switch ( sizeof( T ) )
    {
    case 1:
      return;
    case 2:
      ByteSwapper< T >::SwapWrite2Range( (void *)p, num, fp );
      return;
    case 4:
      ByteSwapper< T >::SwapWrite4Range( (void *)p, num, fp );
      return;
    case 8:
      ByteSwapper< T >::SwapWrite8Range( (void *)p, num, fp );
      return;
    default:
      itkGenericExceptionMacro (<< "Cannot swap number of bytes requested");
    }
}

#else
template< typename T >
void
ByteSwapper< T >
::SwapWriteRangeFromSystemToLittleEndian(T *p, int num, OStreamType *fp)
{
  num *= sizeof( T );
  fp->write( (char *)p, num );
}

#endif

// The following are the protected methods -------------------------
//

//------2-byte methods----------------------------------------------

// Swap 2 byte word.
template< typename T >
void
ByteSwapper< T >
::Swap2(void *pin)
{
  unsigned short *      p = reinterpret_cast< unsigned short * >( pin );
  const unsigned short h1 = (*p) << static_cast<short unsigned int>(8);
  const unsigned short h2 = (*p) >> static_cast<short unsigned int>(8);
  *p = h1 | h2;
}

// Swap bunch of bytes. Num is the number of two byte words to swap.
template< typename T >
void
ByteSwapper< T >
::Swap2Range(void *ptr, BufferSizeType num)
{
  char * pos = reinterpret_cast< char * >( ptr );
  for ( BufferSizeType i = 0; i < num; i++ )
    {
    const char one_byte = pos[0];
    pos[0] = pos[1];
    pos[1] = one_byte;
    pos = pos + 2;
    }
}

// Swap bunch of bytes. Num is the number of four byte words to swap.
template< typename T >
void
ByteSwapper< T >
::SwapWrite2Range(void *ptr, BufferSizeType num, OStreamType *fp)
{
  BufferSizeType chunkSize = 1000000;
  if ( num < chunkSize )
    {
    chunkSize = num;
    }
  char * cpy = new char[chunkSize * 2];
  while ( num )
    {
    memcpy(cpy, ptr, chunkSize * 2);

    char * pos = cpy;
    for ( BufferSizeType i = 0; i < chunkSize; i++ )
      {
      const char one_byte = pos[0];
      pos[0] = pos[1];
      pos[1] = one_byte;
      pos = pos + 2;
      }
    fp->write( (char *)cpy, static_cast<std::streamsize>(2 * chunkSize) );
    ptr = (char *)ptr + chunkSize * 2;
    num -= chunkSize;
    if ( num < chunkSize )
      {
      chunkSize = num;
      }
    }
  delete[] cpy;
}

//------4-byte methods----------------------------------------------

// Swap four byte word.
template< typename T >
void
ByteSwapper< T >
::Swap4(void *ptr)
{
  char  one_byte;
  char *p = reinterpret_cast< char * >( ptr );

  one_byte    = p[0];
  p[0] = p[3];
  p[3] = one_byte;

  one_byte    = p[1];
  p[1] = p[2];
  p[2] = one_byte;
}

// Swap bunch of bytes. Num is the number of four byte words to swap.
template< typename T >
void
ByteSwapper< T >
::Swap4Range(void *ptr, BufferSizeType num)
{
  char *         pos = reinterpret_cast< char * >( ptr );

  for ( BufferSizeType i = 0; i < num; i++ )
    {
    char one_byte = pos[0];
    pos[0] = pos[3];
    pos[3] = one_byte;

    one_byte = pos[1];
    pos[1] = pos[2];
    pos[2] = one_byte;
    pos = pos + 4;
    }
}

// Swap bunch of bytes. Num is the number of four byte words to swap.
template< typename T >
void
ByteSwapper< T >
::SwapWrite4Range(void *ptr, BufferSizeType num, OStreamType *fp)
{
  BufferSizeType chunkSize = 1000000;

  if ( num < chunkSize )
    {
    chunkSize = num;
    }
  char * cpy = new char[chunkSize * 4];

  while ( num )
    {
    memcpy(cpy, ptr, chunkSize * 4);

    char * pos = cpy;
    for ( BufferSizeType i = 0; i < chunkSize; i++ )
      {
      char one_byte = pos[0];
      pos[0] = pos[3];
      pos[3] = one_byte;

      one_byte = pos[1];
      pos[1] = pos[2];
      pos[2] = one_byte;
      pos = pos + 4;
      }
    fp->write( (char *)cpy, static_cast<std::streamsize>(4 * chunkSize) );
    ptr  = (char *)ptr + chunkSize * 4;
    num -= chunkSize;
    if ( num < chunkSize )
      {
      chunkSize = num;
      }
    }
  delete[] cpy;
}

//------8-byte methods----------------------------------------------

// Swap 8 byte double precision
template< typename T >
void
ByteSwapper< T >
::Swap8(void *ptr)
{
  char  one_byte;
  char *p = reinterpret_cast< char * >( ptr );

  one_byte    = p[0];
  p[0] = p[7];
  p[7] = one_byte;

  one_byte    = p[1];
  p[1] = p[6];
  p[6] = one_byte;

  one_byte    = p[2];
  p[2] = p[5];
  p[5] = one_byte;

  one_byte    = p[3];
  p[3] = p[4];
  p[4] = one_byte;
}

// Swap bunch of bytes. Num is the number of eight byte words to swap.
template< typename T >
void
ByteSwapper< T >
::Swap8Range(void *ptr, BufferSizeType num)
{
  char *         pos = reinterpret_cast< char * >( ptr );

  for ( BufferSizeType i = 0; i < num; i++ )
    {
    char one_byte    = pos[0];
    pos[0] = pos[7];
    pos[7] = one_byte;

    one_byte    = pos[1];
    pos[1] = pos[6];
    pos[6] = one_byte;

    one_byte    = pos[2];
    pos[2] = pos[5];
    pos[5] = one_byte;

    one_byte    = pos[3];
    pos[3] = pos[4];
    pos[4] = one_byte;
    pos = pos + 8;
    }
}

// Swap bunch of bytes. Num is the number of four byte words to swap.
template< typename T >
void
ByteSwapper< T >
::SwapWrite8Range(void *ptr, BufferSizeType num, OStreamType *fp)
{
  BufferSizeType chunkSize = 1000000;
  if ( num < chunkSize )
    {
    chunkSize = num;
    }
  char * cpy = new char[chunkSize * 8];

  while ( num )
    {
    memcpy(cpy, ptr, chunkSize * 8);

    ByteSwapper< T >::Swap8Range( (void *)cpy, chunkSize );

    fp->write( (char *)cpy, static_cast<std::streamsize>(8 * chunkSize) );
    ptr  = (char *)ptr + chunkSize * 8;
    num -= chunkSize;
    if ( num < chunkSize )
      {
      chunkSize = num;
      }
    }
  delete[] cpy;
}
} // end namespace itk

#endif

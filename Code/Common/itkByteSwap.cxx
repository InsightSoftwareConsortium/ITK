/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkByteSwap.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkByteSwap.h"
#include "itkObjectFactory.h"
#include <memory>

namespace itk
{

// Swap four byte word.
#ifdef CMAKE_WORDS_BIGENDIAN
void 
ByteSwap
::Swap4BE(void *){}
#else
void 
ByteSwap
::Swap4BE(void *ptr)
{
  char one_byte;
  char *p = (char *)ptr;

  one_byte    = p[0];
  p[0] = p[3];
  p[3] = one_byte;

  one_byte    = p[1];
  p[1] = p[2];
  p[2] = one_byte;
}
#endif

// Swap bunch of bytes. Num is the number of four byte words to swap.
#ifdef CMAKE_WORDS_BIGENDIAN
void 
ByteSwap
::Swap4BERange(void *, int){}
#else
void 
ByteSwap
::Swap4BERange(void *ptr, int num)
{
  char one_byte;
  char *pos = (char *)ptr;
  int i;

  for (i = 0; i < num; i++)
    {
    one_byte = pos[0];
    pos[0] = pos[3];
    pos[3] = one_byte;
    
    one_byte = pos[1];
    pos[1] = pos[2];
    pos[2] = one_byte;
    pos = pos + 4;
    }
}
#endif

// Swap bunch of bytes. Num is the number of four byte words to swap.
void 
ByteSwap
::SwapWrite4BERange(void *ptr, int num, std::ostream *fp)
{
#ifndef CMAKE_WORDS_BIGENDIAN
  char one_byte;
  char *pos;
  int i;
  char *cpy;
  int chunkSize = 1000000;
  char *p = (char *)ptr;

  if (num < chunkSize)
    {
    chunkSize = num;
    }
  cpy = new char [chunkSize * 4];
 
  while (num)
    {
    memcpy(cpy, p, chunkSize * 4);
    
    pos = cpy;   
    for (i = 0; i < chunkSize; i++)
      {
      one_byte = pos[0];
      pos[0] = pos[3];
      pos[3] = one_byte;
      
      one_byte = pos[1];
      pos[1] = pos[2];
      pos[2] = one_byte;
      pos = pos + 4;
      }
    fp->write((char *)cpy, 4*chunkSize);
    p += chunkSize*4;
    num -= chunkSize;
    if (num < chunkSize)
      {
      chunkSize = num;
      }
    }

  delete [] cpy;
#else
  fp->write((void *)ptr, 4*num);
#endif
}

// Swap 2 byte word.
#ifdef CMAKE_WORDS_BIGENDIAN
void 
ByteSwap
::Swap2LE(void *p)
{
  unsigned short h1,h2;

  h1 = (unsigned short) *p << 8;
  h2 = (unsigned short) *p >> 8;
  *p = (short) h1 | h2;
}
#else
void 
ByteSwap
::Swap2LE(void *) {}
#endif

// Swap four byte word.
#ifdef CMAKE_WORDS_BIGENDIAN
void 
ByteSwap
::Swap4LE(void *p)
{
  char one_byte;

  one_byte    = p[0];
  p[0] = p[3];
  p[3] = one_byte;

  one_byte    = p[1];
  p[1] = p[2];
  p[2] = one_byte;
}
#else
void 
ByteSwap
::Swap4LE(void *){}
#endif

// Swap bunch of bytes. Num is the number of four byte words to swap.
#ifdef CMAKE_WORDS_BIGENDIAN
void 
ByteSwap
::Swap4LERange(void *p, int num)
{
  char one_byte;
  char *pos = (char *)p;
  int i;
  
  for (i = 0; i < num; i++)
    {
    one_byte    = pos[0];
    pos[0] = pos[3];
    pos[3] = one_byte;
    
    one_byte    = pos[1];
    pos[1] = pos[2];
    pos[2] = one_byte;
    pos = pos + 4;
    }
}
#else
void 
ByteSwap
::Swap4LERange(void *, int) {}
#endif

// Swap 2 byte word.
#ifdef CMAKE_WORDS_BIGENDIAN
void 
ByteSwap
::Swap2BE(void *) {}
#else
void 
ByteSwap
::Swap2BE(void *ptr)
{
  unsigned short h1,h2;
  char *p = (char *)ptr;

  h1 = (unsigned short)*p << 8;
  h2 = (unsigned short)*p >> 8;
  *p = (short) h1 | h2;

}
#endif

// Swap bunch of bytes. Num is the number of two byte words to swap.
#ifdef CMAKE_WORDS_BIGENDIAN
void 
ByteSwap
::Swap2BERange(void *, int) {}
#else
void 
ByteSwap
::Swap2BERange(void *ptr, int num)
{
  char one_byte;
  char *pos = (char *)ptr;
  int i;
  
  for (i = 0; i < num; i++)
    {
    one_byte = pos[0];
    pos[0] = pos[1];
    pos[1] = one_byte;
    pos = pos + 2;
    }
}
#endif

// Swap bunch of bytes. Num is the number of two byte words to swap.
#ifdef CMAKE_WORDS_BIGENDIAN
void 
ByteSwap
::Swap2LERange(void *p, int num)
{
  char one_byte;
  char *pos = (char *)p;
  int i;
  
  for (i = 0; i < num; i++)
    {
    one_byte = pos[0];
    pos[0] = pos[1];
    pos[1] = one_byte;
    pos = pos + 2;
    }
}
#else
void 
ByteSwap
::Swap2LERange(void *p, int num){}
#endif


// Swap bunch of bytes. Num is the number of four byte words to swap.
void 
ByteSwap
::SwapWrite2BERange(void *ptr, int num, std::ostream *fp)
{
#ifndef CMAKE_WORDS_BIGENDIAN
  char one_byte;
  char *pos;
  int i;
  char *cpy;
  int chunkSize = 1000000;
  char *p = (char *) ptr;

  if (num < chunkSize)
    {
    chunkSize = num;
    }
  cpy = new char [chunkSize * 2];
 
  while (num)
    {
    memcpy(cpy, p, chunkSize * 2);
 
    pos = cpy; 
    for (i = 0; i < chunkSize; i++)
      {
      one_byte = pos[0];
      pos[0] = pos[1];
      pos[1] = one_byte;
      pos = pos + 2;
      }
    fp->write((char *)cpy, 2*chunkSize);
    p += chunkSize * 2;
    num -= chunkSize;
    if (num < chunkSize)
      {
      chunkSize = num;
      }
    }
    
  delete [] cpy;
  
#else
  fp->write((void *)ptr, 2*num);
#endif
}

//----------------------------------------------------------------------------
// Swaps the bytes of a buffer.  Uses an arbitrary word size, but
// assumes the word size is divisible by two.
void 
ByteSwap
::SwapVoidRange(void *buffer, int numWords, int wordSize)
{
  unsigned char temp, *out, *buf;
  int idx1, idx2, inc, half;
  
  half = wordSize / 2;
  inc = wordSize - 1;
  buf = (unsigned char *)(buffer);
  
  for (idx1 = 0; idx1 < numWords; ++idx1)
    {
    out = buf + inc;
    for (idx2 = 0; idx2 < half; ++idx2)
      {
      temp = *out;
      *out = *buf;
      *buf = temp;
      ++buf;
      --out;
      }
    buf += half;
    }
}

} // end namespace itk
  
    

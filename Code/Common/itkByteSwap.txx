/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkByteSwap.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkByteSwap.h"
#include "itkExceptionObject.h"
#include "itkObjectFactory.h"
#include <memory>

namespace itk
{

/** \class ByteSwapError
 * Exception thrown when trying to swap type of unexpected
 * number of bytes.
 */
class ByteSwapError : public ExceptionObject
{
public:
  itkTypeMacro(ByteSwapError, ExceptionObject);
};


// The following are the public methods --------------------------------
//
  
//------Big Endian methods----------------------------------------------

// Use different swap methods based on type
#ifdef CMAKE_WORDS_BIGENDIAN
template <class T>
void 
ByteSwap<T>
::SwapBE(T *){}
#else
template <class T>
void 
ByteSwap<T>
::SwapBE(T *p)
{
  switch ( sizeof(T) )
    {
    case 1:
      return;
    case 2:
      ByteSwap<T>::Swap2((void *)p);
      return;
    case 4:
      ByteSwap<T>::Swap4((void *)p);      
      return;
    default:  
      ByteSwapError e;
      e.SetLocation("SwapBE");
      e.SetDescription("Cannot swap number of bytes requested");
      throw e;
    }
}
#endif  

#ifdef CMAKE_WORDS_BIGENDIAN
template <class T>
void 
ByteSwap<T>
::SwapRangeBE(T *, unsigned long num) {}
#else
template <class T>
void 
ByteSwap<T>
::SwapRangeBE(T *p, unsigned long num)
{
  switch ( sizeof(T) )
    {
    case 1:
      return;
    case 2:
      ByteSwap<T>::Swap2Range((void *)p,num);
      return;
    case 4:
      ByteSwap<T>::Swap4Range((void *)p,num);      
      return;
    default:  
      ByteSwapError e;
      e.SetLocation("SwapRangeBE");
      e.SetDescription("Cannot swap number of bytes requested");
      throw e;
    }
}
#endif  

#ifdef CMAKE_WORDS_BIGENDIAN
template <class T>
void 
ByteSwap<T>
::SwapWriteRangeBE(T *p, int num, std::ostream *fp)
{
  num *= sizeof(T);
  fp->write((char *)p, num);
}
#else
template <class T>
void 
ByteSwap<T>
::SwapWriteRangeBE(T *p, int num, std::ostream *fp)
{
  switch ( sizeof(T) )
    {
    case 1:
      return;
    case 2:
      ByteSwap<T>::SwapWrite2Range((void *)p, num, fp);
      return;
    case 4:
      ByteSwap<T>::SwapWrite4Range((void *)p, num, fp);      
      return;
    default:  
      ByteSwapError e;
      e.SetLocation("SwapWriteRangeBE");
      e.SetDescription("Cannot swap number of bytes requested");
      throw e;
    }
}
#endif

//------Little Endian methods----------------------------------------------

#ifdef CMAKE_WORDS_BIGENDIAN
template <class T>
void 
ByteSwap<T>
::SwapLE(T *)
{
  switch ( sizeof(T) )
    {
    case 1:
      return;
    case 2:
      ByteSwap<T>::Swap2((void *)p);
      return;
    case 4:
      ByteSwap<T>::Swap4((void *)p);      
      return;
    default:  
      ByteSwapError e;
      e.SetLocation("SwapLE");
      e.SetDescription("Cannot swap number of bytes requested");
      throw e;
    }
}
#else
template <class T>
void 
ByteSwap<T>
::SwapLE(T *){}
#endif  


#ifdef CMAKE_WORDS_BIGENDIAN
template <class T>
void 
ByteSwap<T>
::SwapRangeLE(T *, unsigned long num) 
{
  switch ( sizeof(T) )
    {
    case 1:
      return;
    case 2:
      ByteSwap<T>::Swap2Range((void *)p,num);
      return;
    case 4:
      ByteSwap<T>::Swap4Range((void *)p,num);      
      return;
    default:  
      ByteSwapError e;
      e.SetLocation("SwapRangeLE");
      e.SetDescription("Cannot swap number of bytes requested");
      throw e;
    }
}
#else
template <class T>
void 
ByteSwap<T>
::SwapRangeLE(T *p, unsigned long num) {}
#endif  

#ifdef CMAKE_WORDS_BIGENDIAN
template <class T>
void 
ByteSwap<T>
static void SwapWriteRangeLE(T *p, int num, std::ostream *fp)
{
  switch ( sizeof(T) )
    {
    case 1:
      return;
    case 2:
      ByteSwap<T>::SwapWrite2Range((void *)p, num, fp);
      return;
    case 4:
      ByteSwap<T>::SwapWrite4Range((void *)p, num, fp);      
      return;
    default:  
      ByteSwapError e;
      e.SetLocation("SwapWriteRangeLE");
      e.SetDescription("Cannot swap number of bytes requested");
      throw e;
    }
}
#else
template <class T>
void 
ByteSwap<T>
::SwapWriteRangeLE(T *p, int num, std::ostream *fp)
{
  num *= sizeof(T);
  fp->write((char *)p, num);
}
#endif

// The following are the protected methods -------------------------
//

//------2-byte methods----------------------------------------------

// Swap 2 byte word.
template <class T>
void 
ByteSwap<T>
::Swap2(void *pin)
{
  unsigned short h1,h2;
  short* p = reinterpret_cast<short*>(pin);
  
  h1 = static_cast<unsigned short>(*p) << 8;
  h2 = static_cast<unsigned short>(*p) >> 8;
  *p = (short) h1 | h2;
}

// Swap bunch of bytes. Num is the number of two byte words to swap.
template <class T>
void 
ByteSwap<T>
::Swap2Range(void *ptr, unsigned long num)
{
  char one_byte;
  char *pos = reinterpret_cast<char *>(ptr);
  int i;
  
  for (i = 0; i < num; i++)
    {
    one_byte = pos[0];
    pos[0] = pos[1];
    pos[1] = one_byte;
    pos = pos + 2;
    }
}

// Swap bunch of bytes. Num is the number of four byte words to swap.
template <class T>
void 
ByteSwap<T>
::SwapWrite2Range(void *ptr, unsigned long num, std::ostream *fp)
{
  char one_byte;
  char *pos;
  int i;
  char *cpy;
  int chunkSize = 1000000;

  if (num < chunkSize)
    {
    chunkSize = num;
    }
  cpy = new char [chunkSize * 2];
 
  while (num)
    {
    memcpy(cpy, ptr, chunkSize * 2);
 
    pos = cpy; 
    for (i = 0; i < chunkSize; i++)
      {
      one_byte = pos[0];
      pos[0] = pos[1];
      pos[1] = one_byte;
      pos = pos + 2;
      }
    fp->write((char *)cpy, 2*chunkSize);
    ptr += chunkSize * 2;
    num -= chunkSize;
    if (num < chunkSize)
      {
      chunkSize = num;
      }
    }
  delete [] cpy;
}

//------4-byte methods----------------------------------------------

// Swap four byte word.
template <class T>
void 
ByteSwap<T>
::Swap4(void *ptr)
{
  char one_byte;
  char *p = reinterpret_cast<char *>(ptr);

  one_byte    = p[0];
  p[0] = p[3];
  p[3] = one_byte;

  one_byte    = p[1];
  p[1] = p[2];
  p[2] = one_byte;
}

// Swap bunch of bytes. Num is the number of four byte words to swap.
template <class T>
void 
ByteSwap<T>
::Swap4Range(void *ptr, unsigned long num)
{
  char one_byte;
  char *pos = reinterpret_cast<char *>(ptr);
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

// Swap bunch of bytes. Num is the number of four byte words to swap.
template <class T>
void 
ByteSwap<T>
::SwapWrite4Range(void *ptr, unsigned long num, std::ostream *fp)
{
  char one_byte;
  char *pos;
  unsigned long i;
  char *cpy;
  unsigned long chunkSize = 1000000;

  if (num < chunkSize)
    {
    chunkSize = num;
    }
  cpy = new char [chunkSize * 4];
 
  while (num)
    {
    memcpy(cpy, ptr, chunkSize * 4);
    
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
    ptr += chunkSize*4;
    num -= chunkSize;
    if (num < chunkSize)
      {
      chunkSize = num;
      }
    }
  delete [] cpy;
}

} // end namespace itk

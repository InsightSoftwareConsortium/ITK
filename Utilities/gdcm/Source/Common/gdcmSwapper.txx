/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library
  Module:  $URL$

  Copyright (c) 2006-2010 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCMSWAPPER_TXX
#define GDCMSWAPPER_TXX

#ifdef GDCM_HAVE_BYTESWAP_H
// TODO: not cross platform...
#include <byteswap.h>
#endif
#include <stdlib.h>

#include "gdcmTag.h"


namespace gdcm
{

#ifdef GDCM_WORDS_BIGENDIAN
  template <> inline uint16_t SwapperNoOp::Swap<uint16_t>(uint16_t val)
    {
#ifdef GDCM_HAVE_BYTESWAP_H
    return bswap_16(val);
#else
    return (val>>8) | (val<<8);
#endif
    }
  template <> inline int16_t SwapperNoOp::Swap<int16_t>(int16_t val)
    {
    return Swap((uint16_t)val);
    }

  template <> inline uint32_t SwapperNoOp::Swap<uint32_t>(uint32_t val)
    {
#ifdef GDCM_HAVE_BYTESWAP_H
    return bswap_32(val);
#else
    val= ((val<<8)&0xFF00FF00) | ((val>>8)&0x00FF00FF);
    val= (val>>16) | (val<<16);
    return val;
#endif
    }
  template <> inline int32_t SwapperNoOp::Swap<int32_t>(int32_t val)
    {
    return Swap((uint32_t)val);
    }
  template <> inline float SwapperNoOp::Swap<float>(float val)
    {
    return Swap((uint32_t)val);
    }
  template <> inline uint64_t SwapperNoOp::Swap<uint64_t>(uint64_t val)
    {
#ifdef GDCM_HAVE_BYTESWAP_H
    return bswap_64(val);
#else
    val= ((val<< 8)&0xFF00FF00FF00FF00ULL) | ((val>> 8)&0x00FF00FF00FF00FFULL);
    val= ((val<<16)&0xFFFF0000FFFF0000ULL) | ((val>>16)&0x0000FFFF0000FFFFULL);
    return (val>>32) | (val<<32);
#endif
    }
  template <> inline int64_t SwapperNoOp::Swap<int64_t>(int64_t val)
    {
    return Swap((uint64_t)val);
    }
  template <> inline double SwapperNoOp::Swap<double>(double val)
    {
    return Swap((uint64_t)val);
    }

  template <> inline Tag SwapperNoOp::Swap<Tag>(Tag val)
    {
    return Tag( Swap(val.GetGroup()), Swap(val.GetElement()) );
    }

  template <> inline void SwapperNoOp::SwapArray(uint8_t *, unsigned int ) {}

  template <> inline void SwapperNoOp::SwapArray(float *array, unsigned int n)
    {
    switch( sizeof(float) )
      {
      case 4:
        SwapperNoOp::SwapArray<uint32_t>((uint32_t*)array,n);
        break;
      default:
        assert(0);
      }
    }

  template <> inline void SwapperNoOp::SwapArray(double *array, unsigned int n)
    {
    switch( sizeof(double) )
      {
      case 8:
        SwapperNoOp::SwapArray<uint64_t>((uint64_t*)array,n);
        break;
      default:
        assert(0);
      }
    }

#else
  template <> inline uint16_t SwapperDoOp::Swap<uint16_t>(uint16_t val)
    {
#ifdef GDCM_HAVE_BYTESWAP_H
    return bswap_16(val);
#else
    return (val>>8) | (val<<8);
#endif
    }
  template <> inline int16_t SwapperDoOp::Swap<int16_t>(int16_t val)
    {
    return Swap((uint16_t)val);
    }

  template <> inline uint32_t SwapperDoOp::Swap<uint32_t>(uint32_t val)
    {
#ifdef GDCM_HAVE_BYTESWAP_H
    return bswap_32(val);
#else
    val= ((val<<8)&0xFF00FF00) | ((val>>8)&0x00FF00FF);
    val= (val>>16) | (val<<16);
    return val;
#endif
    }
  template <> inline int32_t SwapperDoOp::Swap<int32_t>(int32_t val)
    {
    return Swap((uint32_t)val);
    }
  template <> inline float SwapperDoOp::Swap<float>(float val)
    {
    return static_cast<float>(Swap((uint32_t)val));
    }
  template <> inline uint64_t SwapperDoOp::Swap<uint64_t>(uint64_t val)
    {
#ifdef GDCM_HAVE_BYTESWAP_H
    return bswap_64(val);
#else
    val= ((val<< 8)&0xFF00FF00FF00FF00ULL) | ((val>> 8)&0x00FF00FF00FF00FFULL);
    val= ((val<<16)&0xFFFF0000FFFF0000ULL) | ((val>>16)&0x0000FFFF0000FFFFULL);
    return (val>>32) | (val<<32);
#endif
    }
  template <> inline int64_t SwapperDoOp::Swap<int64_t>(int64_t val)
    {
    return Swap((uint64_t)val);
    }
  template <> inline double SwapperDoOp::Swap<double>(double val)
    {
    return static_cast<double>(Swap((uint64_t)val));
    }

  template <> inline Tag SwapperDoOp::Swap<Tag>(Tag val)
    {
    return Tag( Swap((uint32_t)val.GetElementTag()) );
    }

  template <> inline void SwapperDoOp::SwapArray(uint8_t *, size_t ) {}

  template <> inline void SwapperDoOp::SwapArray(float *array, size_t n)
    {
    switch( sizeof(float) )
      {
      case 4:
        SwapperDoOp::SwapArray<uint32_t>((uint32_t*)array,n);
        break;
      default:
        assert(0);
      }
    }

  template <> inline void SwapperDoOp::SwapArray(double *array, size_t n)
    {
    switch( sizeof(double) )
      {
      case 8:
        SwapperDoOp::SwapArray<uint64_t>((uint64_t*)array,n);
        break;
      default:
        assert(0);
      }
    }


#endif
} // end namespace gdcm

#endif // GDCMSWAPPER_TXX
